initial_ssize_rv <- function() {
  reactiveValues(
    mode = "manual",
    endpoint = "binary",
    design = list(
      endpoint = "binary",
      design_type = "one-sample",
      test_objective = "equality",
      sided = "one-sided",
      alpha = 0.05,
      power = 0.8,
      dropout = 0,
      allocation = 1,
      margin = 0,
      # binary
      binary_method = "exact",
      p0 = 0.2, p1 = 0.5,
      pc = 0.3, pt = 0.5,
      # continuous
      mu_c = 0, mu_t = 0.5, sd = 1, correlation = 0.5,
      # survival
      hr = NA_real_,
      median_c = 12, median_t = 18,
      accrual = 12, followup = 12, event_prob = NA_real_
    ),
    # last successful calculation result (schema from ssize_dispatch)
    result = NULL,
    result_error = NULL,
    result_stamp = NULL,
    # provenance for traceable assumptions
    provenance = list(source = "Manual assumptions", detail = NULL, stamp = NULL),
    adam_source = "example",
    adam_source_label = NULL,
    estimate = NULL,
    estimate_error = NULL,
    estimate_applied = FALSE,
    apply_counter = 0
  )
}
