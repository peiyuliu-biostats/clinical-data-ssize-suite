ssize_read_scenario_file <- function(path) {
  data <- stdz_read_table(path)
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

ssize_as_scalar <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) return(default)
  val <- x[[1]]
  if (length(val) == 0 || is.na(val) || identical(val, "")) default else val
}

ssize_as_num <- function(x, default = NA_real_) {
  val <- suppressWarnings(as.numeric(ssize_as_scalar(x, default)))
  if (length(val) == 0 || is.na(val)) default else val
}

ssize_as_chr <- function(x, default = NULL) {
  val <- ssize_as_scalar(x, default)
  if (is.null(val)) default else as.character(val)
}

ssize_design_from_row <- function(row, base_design = list()) {
  endpoint <- ssize_as_chr(row$endpoint %||% row$endpoint_type, base_design$endpoint %||% "binary")
  design_type <- ssize_as_chr(row$design_type, base_design$design_type %||% "one-sample")
  test_objective <- ssize_as_chr(row$test_objective, base_design$test_objective %||% "equality")
  sided <- ssize_as_chr(row$sided, base_design$sided %||% "one-sided")
  allocation <- ssize_as_num(row$allocation %||% row$allocation_ratio, base_design$allocation %||% 1)
  binary_method <- ssize_as_chr(row$binary_method, base_design$binary_method %||% if (identical(design_type, "one-sample")) "exact" else "normal")
  list(
    endpoint = endpoint,
    design_type = design_type,
    test_objective = test_objective,
    sided = sided,
    alpha = ssize_as_num(row$alpha, base_design$alpha %||% 0.05),
    power = ssize_as_num(row$power, base_design$power %||% 0.8),
    dropout = ssize_as_num(row$dropout, base_design$dropout %||% 0),
    allocation = allocation,
    margin = ssize_as_num(row$margin, base_design$margin %||% 0),
    binary_method = binary_method,
    p0 = ssize_as_num(row$p0 %||% row$pc, base_design$p0 %||% 0.2),
    p1 = ssize_as_num(row$p1 %||% row$pt, base_design$p1 %||% 0.5),
    pc = ssize_as_num(row$pc, base_design$pc %||% 0.3),
    pt = ssize_as_num(row$pt, base_design$pt %||% 0.5),
    mu_c = ssize_as_num(row$mu_c, base_design$mu_c %||% 0),
    mu_t = ssize_as_num(row$mu_t, base_design$mu_t %||% 0.5),
    sd = ssize_as_num(row$sd, base_design$sd %||% 1),
    correlation = ssize_as_num(row$correlation, base_design$correlation %||% 0.5),
    hr = ssize_as_num(row$hr, base_design$hr %||% NA_real_),
    median_c = ssize_as_num(row$median_c %||% row$median_ref, base_design$median_c %||% 12),
    median_t = ssize_as_num(row$median_t %||% row$median_new, base_design$median_t %||% 18),
    accrual = ssize_as_num(row$accrual, base_design$accrual %||% 12),
    followup = ssize_as_num(row$followup, base_design$followup %||% 12),
    event_prob = ssize_as_num(row$event_prob, base_design$event_prob %||% NA_real_)
  )
}

ssize_scenario_template <- function(base_design = NULL) {
  if (is.null(base_design)) {
    base_design <- list(
      endpoint = "binary", design_type = "one-sample", test_objective = "equality",
      sided = "one-sided", alpha = 0.05, power = 0.8, dropout = 0,
      allocation = 1, margin = 0, binary_method = "exact",
      p0 = 0.2, p1 = 0.5, pc = 0.3, pt = 0.5,
      mu_c = 0, mu_t = 0.5, sd = 1, correlation = 0.5,
      hr = NA_real_, median_c = 12, median_t = 18,
      accrual = 12, followup = 12, event_prob = NA_real_
    )
  }
  data.frame(
    scenario_id = c("base", "optimistic", "conservative"),
    endpoint = base_design$endpoint,
    design_type = base_design$design_type,
    test_objective = base_design$test_objective,
    sided = base_design$sided,
    alpha = base_design$alpha,
    power = c(base_design$power, min(0.95, base_design$power + 0.1), max(0.7, base_design$power - 0.1)),
    dropout = base_design$dropout,
    allocation = base_design$allocation,
    margin = base_design$margin,
    binary_method = base_design$binary_method,
    p0 = base_design$p0,
    p1 = c(base_design$p1, base_design$p1, (base_design$p0 + base_design$p1) / 2),
    pc = base_design$pc,
    pt = c(base_design$pt, base_design$pt, (base_design$pc + base_design$pt) / 2),
    mu_c = base_design$mu_c,
    mu_t = c(base_design$mu_t, base_design$mu_t, (base_design$mu_c + base_design$mu_t) / 2),
    sd = base_design$sd,
    correlation = base_design$correlation,
    hr = base_design$hr,
    median_c = base_design$median_c,
    median_t = c(base_design$median_t, base_design$median_t, (base_design$median_c + base_design$median_t) / 2),
    accrual = base_design$accrual,
    followup = base_design$followup,
    event_prob = base_design$event_prob,
    notes = c("current design", "higher target power", "smaller treatment effect"),
    stringsAsFactors = FALSE
  )
}

ssize_compute_scenarios <- function(scenarios, base_design = list()) {
  if (is.null(scenarios) || nrow(scenarios) == 0) {
    stop("Scenario table is empty.", call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(scenarios)), function(i) {
    row <- scenarios[i, , drop = FALSE]
    design <- ssize_design_from_row(row, base_design)
    res <- if (identical(design$endpoint, "survival") && !identical(design$test_objective, "equality")) {
      list(ok = FALSE, message = "Survival scenario objectives other than equality are not implemented in the MVP engine.")
    } else {
      tryCatch(ssize_dispatch(design), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    }
    data.frame(
      scenario_id = ssize_as_chr(row$scenario_id, sprintf("scenario_%03d", i)),
      endpoint = design$endpoint,
      design_type = design$design_type,
      test_objective = design$test_objective,
      sided = design$sided,
      alpha = design$alpha,
      power = design$power,
      n_total = if (isTRUE(res$ok) && !is.null(res$n_total)) res$n_total else NA_integer_,
      events = if (isTRUE(res$ok) && !is.null(res$events)) res$events else NA_integer_,
      achieved_power = if (isTRUE(res$ok) && !is.null(res$achieved_power)) res$achieved_power else NA_real_,
      method = if (isTRUE(res$ok)) res$method else NA_character_,
      status = if (isTRUE(res$ok)) "computed" else "needs review",
      message = if (isTRUE(res$ok)) res$interpretation else res$message %||% "Calculation failed.",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

ssize_sensitivity_grid <- function(base_design, parameter, values) {
  if (length(values) == 0 || any(!is.finite(values))) {
    stop("Sensitivity grid values must be numeric.", call. = FALSE)
  }
  rows <- lapply(values, function(value) {
    design <- base_design
    design[[parameter]] <- value
    res <- tryCatch(ssize_dispatch(design), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    data.frame(
      parameter = parameter,
      value = value,
      n_total = if (isTRUE(res$ok) && !is.null(res$n_total)) res$n_total else NA_integer_,
      events = if (isTRUE(res$ok) && !is.null(res$events)) res$events else NA_integer_,
      achieved_power = if (isTRUE(res$ok) && !is.null(res$achieved_power)) res$achieved_power else NA_real_,
      status = if (isTRUE(res$ok)) "computed" else "needs review",
      message = if (isTRUE(res$ok)) res$interpretation else res$message %||% "Calculation failed.",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
