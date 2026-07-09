ssize_staged_adam_sources <- function() {
  data.frame(
    endpoint = c("binary", "continuous", "survival"),
    key = c("adam_adrs_onco", "adam_adsl", "adam_adtte_onco"),
    label = c(
      "pharmaverse ADaM ADRS oncology response",
      "pharmaverse ADaM ADSL demographics",
      "pharmaverse ADaM ADTTE oncology time-to-event"
    ),
    path = c(
      "inst/example_data/standard/adam_adrs_onco.csv",
      "inst/example_data/standard/adam_adsl.csv",
      "inst/example_data/standard/adam_adtte_onco.csv"
    ),
    stringsAsFactors = FALSE
  )
}

ssize_read_staged_adam <- function(endpoint) {
  src <- ssize_staged_adam_sources()
  row <- src[src$endpoint == endpoint, , drop = FALSE]
  if (nrow(row) != 1 || !file.exists(row$path)) {
    stop(sprintf("No staged ADaM-like data is available for endpoint '%s'.", endpoint), call. = FALSE)
  }
  list(source = row, data = stdz_read_table(row$path[[1]]))
}

ssize_read_adam_upload <- function(path) {
  as.data.frame(stdz_read_table(path), stringsAsFactors = FALSE, check.names = FALSE)
}

ssize_non_screen_arms <- function(data) {
  if (!"ARM" %in% names(data)) {
    stop("ADaM-like data must contain ARM for treatment-group estimation.", call. = FALSE)
  }
  arms <- sort(unique(stats::na.omit(as.character(data$ARM))))
  arms <- arms[!grepl("SCREEN", toupper(arms))]
  if (length(arms) < 2) {
    stop("ADaM-like data must contain at least two non-screen ARM values.", call. = FALSE)
  }
  arms
}

ssize_default_arms <- function(data) {
  arms <- ssize_non_screen_arms(data)
  control <- if ("Placebo" %in% arms) "Placebo" else arms[[1]]
  treatment <- arms[arms != control][[1]]
  list(arms = arms, control = control, treatment = treatment)
}

ssize_numeric_candidates <- function(data) {
  vars <- names(data)[vapply(data, function(x) {
    is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(stats::na.omit(x)))))
  }, logical(1))]
  setdiff(vars, c("USUBJID", "SUBJID", "STUDYID"))
}

ssize_estimate_binary_from_data <- function(data, source_label,
                                            paramcd = "RSP",
                                            response_values = c("Y", "CR", "PR"),
                                            control_arm = "Placebo",
                                            treatment_arm = "Xanomeline High Dose") {
  if (!all(c("PARAMCD", "AVALC", "ARM", "USUBJID") %in% names(data))) {
    stop("ADRS data must contain PARAMCD, AVALC, ARM, and USUBJID.", call. = FALSE)
  }
  subset <- data[data$PARAMCD == paramcd & data$ARM %in% c(control_arm, treatment_arm), , drop = FALSE]
  if (nrow(subset) == 0) stop("No ADRS rows match the selected parameter and arms.", call. = FALSE)
  response <- toupper(as.character(subset$AVALC)) %in% toupper(response_values)
  arm_summary <- do.call(rbind, lapply(c(control_arm, treatment_arm), function(arm) {
    x <- response[subset$ARM == arm]
    data.frame(ARM = arm, n = length(x), responders = sum(x), rate = mean(x), stringsAsFactors = FALSE)
  }))
  if (any(arm_summary$n == 0)) stop("Both selected arms must contain observations.", call. = FALSE)
  extract <- function(arm, item) arm_summary[arm_summary$ARM == arm, item][[1]]
  pc <- extract(control_arm, "rate")
  pt <- extract(treatment_arm, "rate")
  list(
    endpoint = "binary",
    table = data.frame(
      arm = arm_summary$ARM,
      n = arm_summary$n,
      responders = arm_summary$responders,
      rate = round(arm_summary$rate, 4),
      stringsAsFactors = FALSE
    ),
    assumptions = data.frame(
      parameter = c("Control rate (pc)", "Treatment rate (pt)", "One-sample null p0", "One-sample alternative p1"),
      value = c(pc, pt, pc, pt),
      source = sprintf("ADRS %s; response=%s", paramcd, paste(response_values, collapse = "/")),
      stringsAsFactors = FALSE
    ),
    design_updates = list(
      endpoint = "binary",
      design_type = "two-sample",
      test_objective = "equality",
      pc = pc,
      pt = pt,
      p0 = pc,
      p1 = pt,
      allocation = 1,
      binary_method = "normal"
    ),
    provenance = sprintf("%s; PARAMCD=%s; control=%s; treatment=%s", source_label, paramcd, control_arm, treatment_arm)
  )
}

ssize_estimate_continuous_from_data <- function(data, source_label,
                                                value_var = "AGE",
                                                control_arm = "Placebo",
                                                treatment_arm = "Xanomeline High Dose") {
  if (!all(c("ARM", value_var) %in% names(data))) {
    stop("ADSL data must contain ARM and the selected continuous variable.", call. = FALSE)
  }
  subset <- data[data$ARM %in% c(control_arm, treatment_arm), , drop = FALSE]
  values <- suppressWarnings(as.numeric(subset[[value_var]]))
  keep <- is.finite(values)
  subset <- subset[keep, , drop = FALSE]
  values <- values[keep]
  if (length(values) < 3) stop("Not enough numeric observations to estimate mean and SD.", call. = FALSE)
  arm_summary <- do.call(rbind, lapply(c(control_arm, treatment_arm), function(arm) {
    x <- values[subset$ARM == arm]
    data.frame(ARM = arm, n = length(x), mean = mean(x), sd = stats::sd(x), stringsAsFactors = FALSE)
  }))
  extract <- function(arm, item) arm_summary[arm_summary$ARM == arm, item][[1]]
  n_c <- extract(control_arm, "n")
  n_t <- extract(treatment_arm, "n")
  if (n_c < 2 || n_t < 2) stop("Both selected arms need at least 2 numeric observations to estimate pooled SD.", call. = FALSE)
  mu_c <- extract(control_arm, "mean")
  mu_t <- extract(treatment_arm, "mean")
  sd_c <- extract(control_arm, "sd")
  sd_t <- extract(treatment_arm, "sd")
  pooled_sd <- sqrt(((n_c - 1) * sd_c^2 + (n_t - 1) * sd_t^2) / (n_c + n_t - 2))
  list(
    endpoint = "continuous",
    table = data.frame(
      arm = arm_summary$ARM,
      n = arm_summary$n,
      mean = round(arm_summary$mean, 4),
      sd = round(arm_summary$sd, 4),
      stringsAsFactors = FALSE
    ),
    assumptions = data.frame(
      parameter = c("Control mean (mu_c)", "Treatment mean (mu_t)", "Pooled SD", "Allocation ratio (t:c)"),
      value = c(mu_c, mu_t, pooled_sd, n_t / n_c),
      source = sprintf("ADSL %s by ARM", value_var),
      stringsAsFactors = FALSE
    ),
    design_updates = list(
      endpoint = "continuous",
      design_type = "two-sample",
      test_objective = "equality",
      mu_c = mu_c,
      mu_t = mu_t,
      sd = pooled_sd,
      allocation = n_t / n_c
    ),
    provenance = sprintf("%s; variable=%s; control=%s; treatment=%s", source_label, value_var, control_arm, treatment_arm)
  )
}

ssize_estimate_survival_from_data <- function(data, source_label,
                                              paramcd = "OS",
                                              control_arm = "Placebo",
                                              treatment_arm = "Xanomeline High Dose") {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for ADTTE survival estimation.", call. = FALSE)
  }
  if (!all(c("PARAMCD", "ARM", "AVAL", "CNSR") %in% names(data))) {
    stop("ADTTE data must contain PARAMCD, ARM, AVAL, and CNSR.", call. = FALSE)
  }
  subset <- data[data$PARAMCD == paramcd & data$ARM %in% c(control_arm, treatment_arm), , drop = FALSE]
  time <- suppressWarnings(as.numeric(subset$AVAL))
  event <- as.integer(subset$CNSR == 0)
  keep <- is.finite(time) & !is.na(event)
  subset <- subset[keep, , drop = FALSE]
  time <- time[keep]
  event <- event[keep]
  if (length(time) < 3 || sum(event) < 1) stop("Not enough events to estimate survival assumptions.", call. = FALSE)
  arm_levels <- c(control_arm, treatment_arm)
  events <- vapply(arm_levels, function(arm) sum(event[subset$ARM == arm]), integer(1))
  n <- vapply(arm_levels, function(arm) sum(subset$ARM == arm), integer(1))
  total_time <- vapply(arm_levels, function(arm) sum(time[subset$ARM == arm], na.rm = TRUE), numeric(1))
  hazard <- (events + 0.5) / total_time
  medians <- log(2) / hazard
  event_prob <- sum(events) / sum(n)
  hr <- hazard[[treatment_arm]] / hazard[[control_arm]]
  list(
    endpoint = "survival",
    table = data.frame(
      arm = arm_levels,
      n = as.integer(n),
      events = as.integer(events),
      event_rate = round(events / n, 4),
      total_time = round(total_time, 4),
      exponential_median = round(medians, 4),
      hazard_rate = round(hazard, 6),
      stringsAsFactors = FALSE
    ),
    assumptions = data.frame(
      parameter = c("Control median", "Treatment median", "Hazard ratio", "Event probability", "Allocation ratio (t:c)"),
      value = c(medians[[control_arm]], medians[[treatment_arm]], hr, event_prob, n[[treatment_arm]] / n[[control_arm]]),
      source = sprintf("ADTTE %s; exponential hazard; CNSR=0 event; 0.5 continuity correction", paramcd),
      stringsAsFactors = FALSE
    ),
    design_updates = list(
      endpoint = "survival",
      design_type = "two-sample",
      test_objective = "equality",
      median_c = medians[[control_arm]],
      median_t = medians[[treatment_arm]],
      hr = hr,
      event_prob = event_prob,
      allocation = n[[treatment_arm]] / n[[control_arm]]
    ),
    provenance = sprintf("%s; PARAMCD=%s; control=%s; treatment=%s", source_label, paramcd, control_arm, treatment_arm)
  )
}

ssize_estimate_from_adam_data <- function(endpoint, data, source_label, ...) {
  switch(
    endpoint,
    binary = ssize_estimate_binary_from_data(data, source_label, ...),
    continuous = ssize_estimate_continuous_from_data(data, source_label, ...),
    survival = ssize_estimate_survival_from_data(data, source_label, ...),
    stop(sprintf("Unsupported endpoint for ADaM estimation: %s", endpoint), call. = FALSE)
  )
}

ssize_estimate_from_staged <- function(endpoint, ...) {
  obj <- ssize_read_staged_adam(endpoint)
  out <- ssize_estimate_from_adam_data(endpoint, obj$data, obj$source$label, ...)
  out$source <- obj$source
  out
}
