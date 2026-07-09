# =============================================================================
# fun_ssize_core.R
# Endpoint-aware, closed-form sample-size calculation engine.
#
# Design contract (shared by all Manual-assumption calculators):
#   - Every calculator returns a plain list with a stable schema:
#       ok            logical, TRUE when a numeric result was produced
#       message       character, human-readable error when ok = FALSE
#       endpoint      "binary" | "continuous" | "survival"
#       design_type   e.g. "one-sample", "two-sample", "paired"
#       test_objective "equality" | "superiority" | "noninferiority" | "equivalence"
#       sided         "one-sided" | "two-sided"
#       n_per_arm     numeric vector (named by arm) or single N
#       n_total       integer total (dropout-adjusted, rounded up)
#       n_total_raw   numeric total before dropout inflation / rounding
#       events        integer required events (survival only, else NULL)
#       achieved_power numeric power recomputed at the rounded N
#       inputs        named list of the resolved numeric inputs actually used
#       assumptions   data.frame(parameter, value, source) for traceability
#       formula       character label of the formula used
#       method        character short method id
#       interpretation character one-line protocol sentence
#
# All functions are pure (no Shiny), so they can be unit-tested directly.
# =============================================================================

# null-coalescing helper (base R has none); defined early so all callers see it
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- small numeric helpers --------------------------------------------------

ssize_z <- function(alpha, power, sided) {
  # one-sided uses alpha; two-sided uses alpha/2
  a <- if (identical(sided, "two-sided")) alpha / 2 else alpha
  list(
    z_alpha = stats::qnorm(1 - a),
    z_beta  = stats::qnorm(power),
    alpha_used = a
  )
}

ssize_ceil <- function(x) as.integer(ceiling(x - 1e-9))

ssize_inflate_dropout <- function(n, dropout) {
  if (is.null(dropout) || is.na(dropout) || dropout <= 0) return(n)
  n / (1 - dropout)
}

ssize_ok <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)

ssize_fail <- function(msg, endpoint, design_type, test_objective, sided) {
  list(
    ok = FALSE, message = msg,
    endpoint = endpoint, design_type = design_type,
    test_objective = test_objective, sided = sided,
    n_per_arm = NULL, n_total = NULL, n_total_raw = NULL, events = NULL,
    achieved_power = NULL, inputs = list(),
    assumptions = data.frame(
      parameter = character(0), value = character(0), source = character(0),
      stringsAsFactors = FALSE
    ),
    formula = NA_character_, method = NA_character_, interpretation = NA_character_
  )
}

ssize_assump_row <- function(parameter, value, source = "manual") {
  data.frame(
    parameter = parameter,
    value = if (is.numeric(value)) format(value, trim = TRUE) else as.character(value),
    source = source,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# BINARY endpoint
# =============================================================================

# Exact one-sample binomial power at a given n.
# Upper test when p1 > p0, lower test when p1 < p0.
ssize_binary_exact_power <- function(n, p0, p1, alpha, sided) {
  a <- if (identical(sided, "two-sided")) alpha / 2 else alpha
  if (p1 > p0) {
    crit <- stats::qbinom(1 - a, n, p0) + 1        # reject if X >= crit
    1 - stats::pbinom(crit - 1, n, p1)
  } else {
    crit <- stats::qbinom(a, n, p0) - 1            # reject if X <= crit
    stats::pbinom(crit, n, p1)
  }
}

# One-sample proportion.
#   method = "normal" (asymptotic) or "exact" (binomial).
#   Exact power is non-monotone in n, so the exact sample size is the smallest
#   n such that power(n') >= target for that n AND the next confirm_window
#   sizes (default 1) also meet it, matching the stable-threshold convention
#   used by standard TrialDesign one-sample binomial tables.
ssize_binary_one_sample <- function(p0, p1, alpha = 0.05, power = 0.8,
                                    sided = "one-sided",
                                    test_objective = "equality",
                                    margin = 0, dropout = 0,
                                    method = c("exact", "normal"),
                                    confirm_window = 1) {
  method <- match.arg(method)
  fail <- function(m) ssize_fail(m, "binary", "one-sample", test_objective, sided)
  if (!ssize_ok(p0) || !ssize_ok(p1)) return(fail("p0 and p1 must be numeric proportions."))
  if (p0 <= 0 || p0 >= 1 || p1 <= 0 || p1 >= 1) return(fail("p0 and p1 must lie strictly between 0 and 1."))
  if (!ssize_ok(alpha) || alpha <= 0 || alpha >= 1) return(fail("alpha must be between 0 and 1."))
  if (!ssize_ok(power) || power <= 0 || power >= 1) return(fail("power must be between 0 and 1."))

  # effective difference under the objective (used by normal method + effect)
  delta <- switch(
    test_objective,
    "equality"      = p1 - p0,
    "superiority"   = (p1 - p0) - margin,
    "noninferiority"= (p1 - p0) + margin,
    "equivalence"   = margin - abs(p1 - p0),
    p1 - p0
  )
  if (!ssize_ok(delta) || delta == 0) return(fail("Effect size (adjusted for margin) is zero; increase the difference or margin."))

  z <- ssize_z(alpha, power, sided)

  if (method == "exact") {
    n_star <- NA_integer_
    for (n in 2:100000) {
      ok_here <- ssize_binary_exact_power(n, p0, p1, alpha, sided) >= power
      if (ok_here) {
        # stable-threshold: confirm the next confirm_window sizes also hold
        stable <- TRUE
        if (confirm_window > 0) {
          for (m in seq_len(confirm_window)) {
            if (ssize_binary_exact_power(n + m, p0, p1, alpha, sided) < power) { stable <- FALSE; break }
          }
        }
        if (stable) { n_star <- n; break }
      }
    }
    if (is.na(n_star)) return(fail("Could not reach target power within the search range."))
    n_raw <- n_star
    n <- ssize_ceil(ssize_inflate_dropout(n_star, dropout))
    achieved <- ssize_binary_exact_power(n, p0, p1, alpha, sided)
    formula_lbl <- "smallest n with exact binomial power(n') >= target (stable threshold)"
    method_id <- "binary_one_sample_exact"
  } else {
    num <- (z$z_alpha * sqrt(p0 * (1 - p0)) + z$z_beta * sqrt(p1 * (1 - p1)))^2
    n_raw <- num / (delta^2)
    n <- ssize_ceil(ssize_inflate_dropout(n_raw, dropout))
    se_null <- sqrt(p0 * (1 - p0) / n)
    se_alt  <- sqrt(p1 * (1 - p1) / n)
    achieved <- stats::pnorm((abs(delta) - z$z_alpha * se_null) / se_alt)
    formula_lbl <- "n = [z_alpha*sqrt(p0(1-p0)) + z_beta*sqrt(p1(1-p1))]^2 / (p1 - p0)^2"
    method_id <- "binary_one_sample_normal"
  }

  assumptions <- do.call(rbind, list(
    ssize_assump_row("Null proportion (p0)", p0),
    ssize_assump_row("Alternative proportion (p1)", p1),
    ssize_assump_row("Method", if (method == "exact") "Exact binomial" else "Normal approximation"),
    ssize_assump_row("Test objective", test_objective),
    ssize_assump_row("Margin (delta)", margin),
    ssize_assump_row("Alpha", alpha),
    ssize_assump_row("Power", power),
    ssize_assump_row("Sided", sided),
    ssize_assump_row("Dropout inflation", dropout)
  ))

  list(
    ok = TRUE, message = NA_character_,
    endpoint = "binary", design_type = "one-sample",
    test_objective = test_objective, sided = sided,
    n_per_arm = c(N = n), n_total = n, n_total_raw = n_raw, events = NULL,
    achieved_power = achieved,
    inputs = list(p0 = p0, p1 = p1, alpha = alpha, power = power,
                  margin = margin, dropout = dropout, effect = delta, method = method),
    assumptions = assumptions,
    formula = formula_lbl,
    method = method_id,
    interpretation = sprintf(
      "At %s alpha = %.3f, to detect a change from p0 = %.3f to p1 = %.3f with %.0f%% power (%s), the required sample size is N = %d%s.",
      sided, alpha, p0, p1, power * 100,
      if (method == "exact") "exact binomial" else "normal approximation", n,
      if (dropout > 0) sprintf(" (inflated for %.0f%% dropout)", dropout * 100) else ""
    )
  )
}

# Two-sample proportion (normal approximation), allocation ratio k = n_t / n_c.
ssize_binary_two_sample <- function(pc, pt, alpha = 0.05, power = 0.8,
                                    sided = "one-sided",
                                    test_objective = "equality",
                                    margin = 0, allocation = 1, dropout = 0) {
  fail <- function(m) ssize_fail(m, "binary", "two-sample", test_objective, sided)
  if (!ssize_ok(pc) || !ssize_ok(pt)) return(fail("Control and treatment proportions must be numeric."))
  if (pc <= 0 || pc >= 1 || pt <= 0 || pt >= 1) return(fail("Proportions must lie strictly between 0 and 1."))
  if (!ssize_ok(allocation) || allocation <= 0) return(fail("Allocation ratio must be positive."))
  if (!ssize_ok(alpha) || alpha <= 0 || alpha >= 1) return(fail("alpha must be between 0 and 1."))
  if (!ssize_ok(power) || power <= 0 || power >= 1) return(fail("power must be between 0 and 1."))

  delta <- switch(
    test_objective,
    "equality"       = pt - pc,
    "superiority"    = (pt - pc) - margin,
    "noninferiority" = (pt - pc) + margin,
    "equivalence"    = margin - abs(pt - pc),
    pt - pc
  )
  if (!ssize_ok(delta) || delta == 0) return(fail("Adjusted effect size is zero; check proportions and margin."))

  z <- ssize_z(alpha, power, sided)
  k <- allocation
  # control-arm size, unequal allocation:
  var_term <- pc * (1 - pc) + pt * (1 - pt) / k
  n_c_raw <- (z$z_alpha + z$z_beta)^2 * var_term / (delta^2)
  n_t_raw <- k * n_c_raw

  n_c <- ssize_ceil(ssize_inflate_dropout(n_c_raw, dropout))
  n_t <- ssize_ceil(ssize_inflate_dropout(n_t_raw, dropout))
  n_total <- n_c + n_t
  n_total_raw <- n_c_raw + n_t_raw

  se <- sqrt(pc * (1 - pc) / n_c + pt * (1 - pt) / n_t)
  achieved <- stats::pnorm(abs(delta) / se - z$z_alpha)

  assumptions <- do.call(rbind, list(
    ssize_assump_row("Control rate (pc)", pc),
    ssize_assump_row("Treatment rate (pt)", pt),
    ssize_assump_row("Test objective", test_objective),
    ssize_assump_row("Margin (delta)", margin),
    ssize_assump_row("Allocation ratio (t:c)", allocation),
    ssize_assump_row("Alpha", alpha),
    ssize_assump_row("Power", power),
    ssize_assump_row("Sided", sided),
    ssize_assump_row("Dropout inflation", dropout)
  ))

  list(
    ok = TRUE, message = NA_character_,
    endpoint = "binary", design_type = "two-sample",
    test_objective = test_objective, sided = sided,
    n_per_arm = c(control = n_c, treatment = n_t),
    n_total = n_total, n_total_raw = n_total_raw, events = NULL,
    achieved_power = achieved,
    inputs = list(pc = pc, pt = pt, alpha = alpha, power = power,
                  margin = margin, allocation = allocation, dropout = dropout, effect = delta),
    assumptions = assumptions,
    formula = "n_c = (z_alpha + z_beta)^2 * [pc(1-pc) + pt(1-pt)/k] / (pt - pc)^2 ; n_t = k*n_c",
    method = "binary_two_sample_normal",
    interpretation = sprintf(
      "At %s alpha = %.3f and %.0f%% power, comparing pc = %.3f vs pt = %.3f (allocation %g:1) requires %d control + %d treatment = %d total.",
      sided, alpha, power * 100, pc, pt, allocation, n_c, n_t, n_total
    )
  )
}

# =============================================================================
# CONTINUOUS endpoint (noncentral-t exact solving)
# =============================================================================

# Solve N so that a t-test achieves target power, using the noncentral t.
# design_type: "one-sample" | "paired" (df = n - 1, per-subject SD)
#              "two-sample" (df = n_c + n_t - 2)
ssize_continuous <- function(mu_c, mu_t, sd, alpha = 0.05, power = 0.8,
                             sided = "one-sided",
                             design_type = "one-sample",
                             test_objective = "equality",
                             margin = 0, allocation = 1, dropout = 0,
                             correlation = 0) {
  fail <- function(m) ssize_fail(m, "continuous", design_type, test_objective, sided)
  if (!ssize_ok(mu_c) || !ssize_ok(mu_t)) return(fail("Means must be numeric."))
  if (!ssize_ok(sd) || sd <= 0) return(fail("SD must be a positive number."))
  if (!ssize_ok(alpha) || alpha <= 0 || alpha >= 1) return(fail("alpha must be between 0 and 1."))
  if (!ssize_ok(power) || power <= 0 || power >= 1) return(fail("power must be between 0 and 1."))
  if (!ssize_ok(allocation) || allocation <= 0) return(fail("Allocation ratio must be positive."))

  raw_diff <- mu_t - mu_c
  eff <- switch(
    test_objective,
    "equality"       = raw_diff,
    "superiority"    = raw_diff - margin,
    "noninferiority" = raw_diff + margin,
    "equivalence"    = margin - abs(raw_diff),
    raw_diff
  )
  if (!ssize_ok(eff) || eff == 0) return(fail("Adjusted mean difference is zero; check means and margin."))

  # effective per-analysis SD for paired design uses correlation
  sd_eff <- sd
  if (identical(design_type, "paired")) {
    sd_eff <- sd * sqrt(2 * (1 - correlation))
  }
  d <- abs(eff) / sd_eff

  a <- if (identical(sided, "two-sided")) alpha / 2 else alpha

  power_at <- function(n) {
    if (identical(design_type, "two-sample")) {
      n_c <- n
      n_t <- allocation * n
      df <- n_c + n_t - 2
      if (df < 1) return(0)
      ncp <- abs(eff) / sd * sqrt((n_c * n_t) / (n_c + n_t))
    } else {
      # one-sample or paired
      df <- n - 1
      if (df < 1) return(0)
      ncp <- d * sqrt(n)
    }
    tcrit <- stats::qt(1 - a, df)
    stats::pt(tcrit, df, ncp = ncp, lower.tail = FALSE)
  }

  # search smallest n (control arm units for two-sample) achieving power
  n <- NA_integer_
  for (cand in 2:100000) {
    if (power_at(cand) >= power) { n <- cand; break }
  }
  if (is.na(n)) return(fail("Could not reach target power within the search range."))

  if (identical(design_type, "two-sample")) {
    n_c_raw <- n
    n_t_raw <- allocation * n
    n_c <- ssize_ceil(ssize_inflate_dropout(n_c_raw, dropout))
    n_t <- ssize_ceil(ssize_inflate_dropout(n_t_raw, dropout))
    n_per_arm <- c(control = n_c, treatment = n_t)
    n_total <- n_c + n_t
    n_total_raw <- n_c_raw + n_t_raw
    achieved <- power_at(n)
  } else {
    n_adj <- ssize_ceil(ssize_inflate_dropout(n, dropout))
    n_per_arm <- c(N = n_adj)
    n_total <- n_adj
    n_total_raw <- n
    achieved <- power_at(n)
  }

  assumptions <- do.call(rbind, c(
    list(
      ssize_assump_row("Control mean (mu_c)", mu_c),
      ssize_assump_row("Treatment mean (mu_t)", mu_t),
      ssize_assump_row("SD", sd),
      ssize_assump_row("Standardized effect (d)", round(d, 4)),
      ssize_assump_row("Design type", design_type),
      ssize_assump_row("Test objective", test_objective),
      ssize_assump_row("Margin", margin),
      ssize_assump_row("Alpha", alpha),
      ssize_assump_row("Power", power),
      ssize_assump_row("Sided", sided),
      ssize_assump_row("Dropout inflation", dropout)
    ),
    if (identical(design_type, "paired")) list(ssize_assump_row("Paired correlation", correlation)) else NULL,
    if (identical(design_type, "two-sample")) list(ssize_assump_row("Allocation ratio (t:c)", allocation)) else NULL
  ))

  list(
    ok = TRUE, message = NA_character_,
    endpoint = "continuous", design_type = design_type,
    test_objective = test_objective, sided = sided,
    n_per_arm = n_per_arm, n_total = n_total, n_total_raw = n_total_raw, events = NULL,
    achieved_power = achieved,
    inputs = list(mu_c = mu_c, mu_t = mu_t, sd = sd, alpha = alpha, power = power,
                  margin = margin, allocation = allocation, dropout = dropout,
                  correlation = correlation, effect = eff, d = d),
    assumptions = assumptions,
    formula = "solve power = P(t_{df,ncp} > t_crit); ncp = d*sqrt(n) (one/paired) or |eff|/sd*sqrt(n_c n_t/(n_c+n_t)) (two-sample)",
    method = paste0("continuous_", gsub("-", "_", design_type), "_noncentral_t"),
    interpretation = sprintf(
      "At %s alpha = %.3f and %.0f%% power, a %s t-test for a mean difference of %.3g (SD = %.3g, d = %.3f) requires %s.",
      sided, alpha, power * 100, design_type, eff, sd, d,
      if (identical(design_type, "two-sample"))
        sprintf("%d + %d = %d subjects", n_per_arm[["control"]], n_per_arm[["treatment"]], n_total)
      else sprintf("N = %d subjects", n_total)
    )
  )
}

# =============================================================================
# TIME-TO-EVENT endpoint (log-rank / Schoenfeld events + exponential N)
# =============================================================================

# One-sample survival vs a fixed reference (exponential), Schoenfeld-style.
# Uses hazard ratio hr; if medians are supplied, hr is derived as
# hr = median_ref / median_new  (exponential: hazard = ln2 / median).
ssize_survival_one_sample <- function(hr = NULL,
                                      median_ref = NULL, median_new = NULL,
                                      alpha = 0.05, power = 0.8,
                                      sided = "one-sided", dropout = 0) {
  fail <- function(m) ssize_fail(m, "survival", "one-sample", "equality", sided)
  if (is.null(hr) && (!ssize_ok(median_ref) || !ssize_ok(median_new)))
    return(fail("Provide a hazard ratio, or both reference and new medians."))
  if (is.null(hr)) {
    if (median_ref <= 0 || median_new <= 0) return(fail("Medians must be positive."))
    hr <- median_ref / median_new
  }
  if (!ssize_ok(hr) || hr <= 0 || hr == 1) return(fail("Hazard ratio must be positive and not equal to 1."))
  if (!ssize_ok(alpha) || alpha <= 0 || alpha >= 1) return(fail("alpha must be between 0 and 1."))
  if (!ssize_ok(power) || power <= 0 || power >= 1) return(fail("power must be between 0 and 1."))

  z <- ssize_z(alpha, power, sided)
  events_raw <- (z$z_alpha + z$z_beta)^2 / (log(hr)^2)
  events <- ssize_ceil(events_raw)

  assumptions <- do.call(rbind, list(
    ssize_assump_row("Hazard ratio", round(hr, 4)),
    if (!is.null(median_ref)) ssize_assump_row("Reference median", median_ref) else NULL,
    if (!is.null(median_new)) ssize_assump_row("New median", median_new) else NULL,
    ssize_assump_row("Alpha", alpha),
    ssize_assump_row("Power", power),
    ssize_assump_row("Sided", sided),
    ssize_assump_row("Dropout inflation", dropout)
  ))

  list(
    ok = TRUE, message = NA_character_,
    endpoint = "survival", design_type = "one-sample",
    test_objective = "equality", sided = sided,
    n_per_arm = NULL, n_total = NULL, n_total_raw = NULL,
    events = events, achieved_power = power,
    inputs = list(hr = hr, median_ref = median_ref, median_new = median_new,
                  alpha = alpha, power = power, dropout = dropout),
    assumptions = assumptions,
    formula = "d = (z_alpha + z_beta)^2 / (ln HR)^2",
    method = "survival_one_sample_schoenfeld",
    interpretation = sprintf(
      "At %s alpha = %.3f and %.0f%% power, detecting HR = %.3f requires %d events.",
      sided, alpha, power * 100, hr, events
    )
  )
}

# Two-sample log-rank (Schoenfeld events) with exponential accrual/follow-up
# translation to total N. p_event is the probability a subject has an event
# during the study; if accrual/follow-up/medians are given it is derived
# under an exponential model with uniform accrual.
ssize_survival_two_sample <- function(hr = NULL,
                                      median_c = NULL, median_t = NULL,
                                      alpha = 0.05, power = 0.8,
                                      sided = "one-sided",
                                      allocation = 1,
                                      accrual = NULL, followup = NULL,
                                      event_prob = NULL, dropout = 0) {
  fail <- function(m) ssize_fail(m, "survival", "two-sample", "equality", sided)
  if (is.null(hr) && (!ssize_ok(median_c) || !ssize_ok(median_t)))
    return(fail("Provide a hazard ratio, or both control and treatment medians."))
  if (is.null(hr)) {
    if (median_c <= 0 || median_t <= 0) return(fail("Medians must be positive."))
    hr <- median_c / median_t
  }
  if (!ssize_ok(hr) || hr <= 0 || hr == 1) return(fail("Hazard ratio must be positive and not equal to 1."))
  if (!ssize_ok(allocation) || allocation <= 0) return(fail("Allocation ratio must be positive."))
  if (!ssize_ok(alpha) || alpha <= 0 || alpha >= 1) return(fail("alpha must be between 0 and 1."))
  if (!ssize_ok(power) || power <= 0 || power >= 1) return(fail("power must be between 0 and 1."))

  z <- ssize_z(alpha, power, sided)
  k <- allocation
  # Schoenfeld with allocation: events = (z_a + z_b)^2 * (1+k)^2 / (k * (ln HR)^2)
  events_raw <- (z$z_alpha + z$z_beta)^2 * (1 + k)^2 / (k * log(hr)^2)
  events <- ssize_ceil(events_raw)

  # translate events -> total N via an event probability
  p_ev <- event_prob
  derived_from <- "supplied"
  if (is.null(p_ev)) {
    if (!is.null(median_c) && !is.null(median_t) &&
        ssize_ok(accrual) && ssize_ok(followup)) {
      # exponential survival, uniform accrual over [0, accrual], additional
      # follow-up 'followup'. Simpson-like average event probability per arm.
      lam_c <- log(2) / median_c
      lam_t <- log(2) / median_t
      p_event_arm <- function(lam) {
        # average over uniform accrual of P(event before end): approximate with
        # 3-point Simpson on total time in [followup, accrual+followup]
        t1 <- followup
        t2 <- accrual + followup
        tm <- (t1 + t2) / 2
        f  <- function(t) 1 - exp(-lam * t)
        (f(t1) + 4 * f(tm) + f(t2)) / 6
      }
      pc_ev <- p_event_arm(lam_c)
      pt_ev <- p_event_arm(lam_t)
      # weight by allocation
      p_ev <- (pc_ev + k * pt_ev) / (1 + k)
      derived_from <- "accrual/follow-up (exponential, Simpson avg)"
    }
  }

  n_total <- NULL; n_per_arm <- NULL; n_total_raw <- NULL
  if (!is.null(p_ev) && ssize_ok(p_ev) && p_ev > 0) {
    n_total_raw <- events / p_ev
    n_total_adj <- ssize_inflate_dropout(n_total_raw, dropout)
    # split by allocation
    n_c <- ssize_ceil(n_total_adj / (1 + k))
    n_t <- ssize_ceil(k * n_total_adj / (1 + k))
    n_per_arm <- c(control = n_c, treatment = n_t)
    n_total <- n_c + n_t
  }

  assumptions <- do.call(rbind, Filter(Negate(is.null), list(
    ssize_assump_row("Hazard ratio", round(hr, 4)),
    if (!is.null(median_c)) ssize_assump_row("Control median", median_c) else NULL,
    if (!is.null(median_t)) ssize_assump_row("Treatment median", median_t) else NULL,
    ssize_assump_row("Allocation ratio (t:c)", allocation),
    if (!is.null(accrual)) ssize_assump_row("Accrual duration", accrual) else NULL,
    if (!is.null(followup)) ssize_assump_row("Additional follow-up", followup) else NULL,
    if (!is.null(p_ev)) ssize_assump_row("Event probability", round(p_ev, 4)) else NULL,
    ssize_assump_row("Alpha", alpha),
    ssize_assump_row("Power", power),
    ssize_assump_row("Sided", sided),
    ssize_assump_row("Dropout inflation", dropout)
  )))

  interp <- if (!is.null(n_total)) {
    sprintf(
      "At %s alpha = %.3f and %.0f%% power, detecting HR = %.3f requires %d events, implying %d + %d = %d subjects (event prob %.3f, %s).",
      sided, alpha, power * 100, hr, events,
      n_per_arm[["control"]], n_per_arm[["treatment"]], n_total, p_ev, derived_from
    )
  } else {
    sprintf(
      "At %s alpha = %.3f and %.0f%% power, detecting HR = %.3f requires %d events. Provide accrual/follow-up or an event probability to translate to total N.",
      sided, alpha, power * 100, hr, events
    )
  }

  list(
    ok = TRUE, message = NA_character_,
    endpoint = "survival", design_type = "two-sample",
    test_objective = "equality", sided = sided,
    n_per_arm = n_per_arm, n_total = n_total, n_total_raw = n_total_raw,
    events = events, achieved_power = power,
    inputs = list(hr = hr, median_c = median_c, median_t = median_t,
                  alpha = alpha, power = power, allocation = allocation,
                  accrual = accrual, followup = followup,
                  event_prob = p_ev, dropout = dropout),
    assumptions = assumptions,
    formula = "d = (z_alpha+z_beta)^2 (1+k)^2 / (k (ln HR)^2); N = d / P(event)",
    method = "survival_two_sample_logrank",
    interpretation = interp
  )
}

# =============================================================================
# Dispatcher: resolve (endpoint, design_type) -> calculator, given a flat
# list of design inputs. Used by the Design tab, Scenarios, and Sensitivity.
# =============================================================================

ssize_dispatch <- function(design) {
  ep <- design$endpoint
  dt <- design$design_type
  common <- list(
    alpha = design$alpha, power = design$power,
    sided = design$sided, dropout = design$dropout %||% 0
  )

  if (identical(ep, "binary")) {
    if (identical(dt, "one-sample")) {
      return(ssize_binary_one_sample(
        p0 = design$p0, p1 = design$p1,
        alpha = common$alpha, power = common$power, sided = common$sided,
        test_objective = design$test_objective, margin = design$margin %||% 0,
        dropout = common$dropout, method = design$binary_method %||% "exact"
      ))
    }
    return(ssize_binary_two_sample(
      pc = design$pc, pt = design$pt,
      alpha = common$alpha, power = common$power, sided = common$sided,
      test_objective = design$test_objective, margin = design$margin %||% 0,
      allocation = design$allocation %||% 1, dropout = common$dropout
    ))
  }

  if (identical(ep, "continuous")) {
    return(ssize_continuous(
      mu_c = design$mu_c, mu_t = design$mu_t, sd = design$sd,
      alpha = common$alpha, power = common$power, sided = common$sided,
      design_type = dt, test_objective = design$test_objective,
      margin = design$margin %||% 0, allocation = design$allocation %||% 1,
      dropout = common$dropout, correlation = design$correlation %||% 0
    ))
  }

  if (identical(ep, "survival")) {
    # NA-valued optional numerics (from empty numericInputs) mean "absent"
    na_to_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
    if (identical(dt, "one-sample")) {
      return(ssize_survival_one_sample(
        hr = na_to_null(design$hr),
        median_ref = na_to_null(design$median_c), median_new = na_to_null(design$median_t),
        alpha = common$alpha, power = common$power, sided = common$sided,
        dropout = common$dropout
      ))
    }
    return(ssize_survival_two_sample(
      hr = na_to_null(design$hr),
      median_c = na_to_null(design$median_c), median_t = na_to_null(design$median_t),
      alpha = common$alpha, power = common$power, sided = common$sided,
      allocation = design$allocation %||% 1,
      accrual = na_to_null(design$accrual), followup = na_to_null(design$followup),
      event_prob = na_to_null(design$event_prob), dropout = common$dropout
    ))
  }

  ssize_fail(sprintf("Unknown endpoint '%s'.", ep), ep, dt, design$test_objective, design$sided)
}
