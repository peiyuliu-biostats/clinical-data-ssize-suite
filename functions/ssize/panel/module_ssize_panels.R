# =============================================================================
# Sample Size analysis panels.
# Design / Methods / Report are fully wired to the live calculation result and
# its traceable assumptions. Scenarios and Sensitivity both reuse the same
# endpoint-aware calculation engine as Design.
# =============================================================================

# ---- small render helpers ---------------------------------------------------

ssize_result_ready <- function(ssize_rv) {
  !is.null(ssize_rv$result) && isTRUE(ssize_rv$result$ok)
}

ssize_assump_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tags$p(class = "suite-muted", "No assumptions recorded."))
  tags$table(
    style = "width:100%; border-collapse: collapse;",
    tags$thead(tags$tr(
      tags$th(style = "text-align:left; border-bottom:2px solid #ddd; padding:6px 8px;", "Parameter"),
      tags$th(style = "text-align:left; border-bottom:2px solid #ddd; padding:6px 8px;", "Value"),
      tags$th(style = "text-align:left; border-bottom:2px solid #ddd; padding:6px 8px;", "Source")
    )),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(style = "border-bottom:1px solid #eee; padding:6px 8px;", df$parameter[i]),
        tags$td(style = "border-bottom:1px solid #eee; padding:6px 8px;", df$value[i]),
        tags$td(style = "border-bottom:1px solid #eee; padding:6px 8px; color:#666;", df$source[i])
      )
    }))
  )
}

ssize_result_cards <- function(res) {
  cards <- list()
  if (!is.null(res$events)) {
    cards[[length(cards) + 1]] <- tags$div(
      class = "suite-card",
      tags$h4(style = "margin:0;", "Required events"),
      tags$div(style = "font-size:26px; font-weight:700; color:#1D7F5C;", res$events)
    )
  }
  if (!is.null(res$n_total)) {
    per_arm <- if (!is.null(res$n_per_arm) && length(res$n_per_arm) > 1) {
      paste(sprintf("%s = %d", names(res$n_per_arm), as.integer(res$n_per_arm)), collapse = "; ")
    } else NULL
    cards[[length(cards) + 1]] <- tags$div(
      class = "suite-card",
      tags$h4(style = "margin:0;", "Total sample size"),
      tags$div(style = "font-size:26px; font-weight:700; color:#333;", res$n_total),
      if (!is.null(per_arm)) tags$div(class = "suite-muted", per_arm) else NULL
    )
  }
  if (!is.null(res$achieved_power)) {
    cards[[length(cards) + 1]] <- tags$div(
      class = "suite-card",
      tags$h4(style = "margin:0;", "Achieved power"),
      tags$div(style = "font-size:20px; font-weight:700; color:#333;", sprintf("%.3f", res$achieved_power))
    )
  }
  tags$div(class = "ssize-result-cards", style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:18px; margin-bottom:14px; clear:both;", cards)
}

ssize_method_narrative <- function(res) {
  if (identical(res$endpoint, "binary") && identical(res$design_type, "one-sample")) {
    if (identical(res$method, "binary_one_sample_exact")) {
      return("The one-sample binary calculation searches for the smallest sample size whose exact binomial rejection rule reaches the target power under the alternative proportion. The search uses the selected one- or two-sided alpha and a stable-threshold convention for the discreteness of exact binomial power.")
    }
    return("The one-sample binary normal approximation uses null and alternative binomial variances on the proportion scale, with the selected alpha, power, margin, and dropout inflation applied after the raw sample size is obtained.")
  }
  if (identical(res$endpoint, "binary")) {
    return("The two-sample binary calculation uses a large-sample normal approximation for two independent proportions. The control-arm sample size is solved first, the treatment arm follows the allocation ratio, and both arms are rounded upward after dropout inflation.")
  }
  if (identical(res$endpoint, "continuous")) {
    return("The continuous endpoint calculation solves power using the noncentral t distribution. One-sample and paired designs use subject-level N; two-sample designs solve the control-arm size and derive treatment-arm size from the allocation ratio.")
  }
  if (identical(res$endpoint, "survival") && identical(res$design_type, "one-sample")) {
    return("The one-sample time-to-event calculation uses a Schoenfeld-style event formula based on the log hazard ratio. If medians are provided, the hazard ratio is derived under an exponential survival assumption.")
  }
  if (identical(res$endpoint, "survival")) {
    return("The two-sample time-to-event calculation uses Schoenfeld required events for a log-rank equality comparison. Required events are translated to total sample size using either a supplied event probability or an exponential accrual/follow-up approximation.")
  }
  "The selected design is evaluated by the endpoint-specific sample-size engine."
}

ssize_report_protocol_paragraph <- function(res, prov) {
  source_text <- prov$source %||% "Manual assumptions"
  paste(
    res$interpretation,
    sprintf("The calculation used the %s method. Assumptions were sourced from %s.", res$method, source_text),
    "All displayed sample sizes are rounded upward; dropout, when specified, is applied as N/(1-dropout)."
  )
}

# =============================================================================
# DESIGN tab
# =============================================================================

module_UI_ssize_design <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Design"),
    uiOutput(ns("status")),
    uiOutput(ns("estimate_summary")),
    uiOutput(ns("result")),
    uiOutput(ns("path")),
    h4("Traceable assumptions"),
    p(class = "suite-muted", "Every parameter used in the calculation, with its provenance."),
    uiOutput(ns("assumptions")),
    downloadButton(ns("dl_assumptions"), "Download assumptions (CSV)")
  )
}

module_server_ssize_design <- function(id, ssize_rv) {
  moduleServer(id, function(input, output, session) {
    output$status <- renderUI({
      if (identical(ssize_rv$mode, "estimate") && !isTRUE(ssize_rv$estimate_applied)) {
        return(suite_compact_status(
          "ADaM estimation",
          "needs apply",
          "Estimate endpoint assumptions in the left Settings panel, then apply them to Design.",
          "pending"
        ))
      }
      if (!is.null(ssize_rv$result_error)) {
        return(suite_compact_status("Design input", "needs review", ssize_rv$result_error, "warn"))
      }
      if (ssize_result_ready(ssize_rv)) {
        return(suite_compact_status("Sample size", "computed",
                 sprintf("Computed %s.", ssize_rv$result_stamp %||% ""), "ready"))
      }
      suite_compact_status("Sample size", "needs input", "Set parameters and calculate, or estimate assumptions from ADaM.", "pending")
    })

    output$estimate_summary <- renderUI({
      if (!identical(ssize_rv$mode, "estimate") && !isTRUE(ssize_rv$estimate_applied)) return(NULL)
      if (!is.null(ssize_rv$estimate_error)) {
        return(suite_locked_notice("Estimated assumptions", ssize_rv$estimate_error))
      }
      if (is.null(ssize_rv$estimate)) {
        return(suite_stage_notice(
          "Estimated assumptions",
          "Choose example ADaM or upload ADaM-like data in Settings, then click Estimate assumptions."
        ))
      }
      tagList(
        h4("Estimated assumptions from ADaM"),
        p(class = "suite-muted", "These estimates are not a separate analysis path. They populate the same Design engine used by manual assumptions."),
        ssize_assump_table(ssize_rv$estimate$assumptions),
        h4("Estimation detail"),
        suite_scroll_table(tableOutput(session$ns("estimate_detail")), max_height = "260px", min_width = "760px"),
        h4("Estimation provenance"),
        div(class = "suite-mono", paste0(
          ssize_rv$estimate$provenance,
          "\nApplied to Design: ",
          if (isTRUE(ssize_rv$estimate_applied)) "yes" else "no"
        ))
      )
    })

    output$estimate_detail <- renderTable({
      req(ssize_rv$estimate)
      ssize_rv$estimate$table
    }, striped = TRUE, bordered = TRUE, width = "100%")

    outputOptions(output, "estimate_detail", suspendWhenHidden = FALSE)

    output$design_inputs <- renderUI({
      if (!ssize_result_ready(ssize_rv)) return(NULL)
      ssize_assump_table(ssize_rv$result$assumptions)
    })

    output$result <- renderUI({
      if (!ssize_result_ready(ssize_rv)) {
        return(if (!is.null(ssize_rv$result_error))
          suite_locked_notice("No result", ssize_rv$result_error)
          else suite_stage_notice("Primary result", "Sample-size results will appear here once parameters are valid."))
      }
      res <- ssize_rv$result
      tagList(
        tags$div(style = "height:2px;"),
        ssize_result_cards(res),
        tags$p(style = "margin-top:8px; font-size:14px;", res$interpretation)
      )
    })

    output$path <- renderUI({
      d <- ssize_rv$design
      tagList(
        h4("Calculation path"),
        div(class = "suite-mono",
            sprintf("%s -> %s -> %s -> %s -> %s",
                    d$endpoint, d$design_type, d$test_objective, d$sided,
                    if (ssize_result_ready(ssize_rv)) ssize_rv$result$method else "sample size"))
      )
    })

    output$assumptions <- renderUI({
      if (!ssize_result_ready(ssize_rv)) return(tags$p(class = "suite-muted", "Available after a successful calculation."))
      ssize_assump_table(ssize_rv$result$assumptions)
    })

    output$dl_assumptions <- downloadHandler(
      filename = function() sprintf("ssize_assumptions_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        df <- if (ssize_result_ready(ssize_rv)) ssize_rv$result$assumptions else
          data.frame(parameter = character(0), value = character(0), source = character(0))
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

# =============================================================================
# SCENARIOS tab
# =============================================================================

module_UI_ssize_scenarios <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Scenarios"),
    p(class = "suite-muted", "Upload a CSV scenario table or use the current Design to generate base / optimistic / conservative scenarios. Each row is computed with the same Design engine."),
    fluidRow(
      column(
        6,
        fileInput(ns("scenario_file"), "Upload scenario CSV", accept = c(".csv", ".txt")),
        actionButton(ns("use_template"), "Use current Design scenarios", icon = icon("table"))
      ),
      column(
        6,
        downloadButton(ns("dl_template"), "Download scenario template"),
        downloadButton(ns("dl_results"), "Download scenario results")
      )
    ),
    uiOutput(ns("status")),
    h4("Scenario input"),
    suite_scroll_table(tableOutput(ns("scenario_input")), max_height = "220px", min_width = "1200px"),
    h4("Batch compute results"),
    suite_scroll_table(tableOutput(ns("scenario_results")), max_height = "360px", min_width = "1200px")
  )
}

module_server_ssize_scenarios <- function(id, ssize_rv) {
  moduleServer(id, function(input, output, session) {
    scenario_data <- reactiveVal(NULL)
    scenario_results <- reactiveVal(NULL)
    scenario_error <- reactiveVal(NULL)

    run_batch <- function(data) {
      out <- tryCatch(ssize_compute_scenarios(data, ssize_rv$design), error = function(e) e)
      if (inherits(out, "error")) {
        scenario_results(NULL)
        scenario_error(conditionMessage(out))
      } else {
        scenario_results(out)
        scenario_error(NULL)
      }
    }

    observeEvent(input$use_template, {
      data <- ssize_scenario_template(ssize_rv$design)
      scenario_data(data)
      run_batch(data)
    }, ignoreInit = TRUE)

    observeEvent(input$scenario_file, {
      req(input$scenario_file)
      data <- tryCatch(read.csv(input$scenario_file$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
      if (inherits(data, "error")) {
        scenario_data(NULL)
        scenario_results(NULL)
        scenario_error(conditionMessage(data))
      } else {
        scenario_data(data)
        run_batch(data)
      }
    }, ignoreInit = TRUE)

    output$status <- renderUI({
      if (!is.null(scenario_error())) {
        return(suite_compact_status("Scenario batch", "needs review", scenario_error(), "warn"))
      }
      if (!is.null(scenario_results())) {
        return(suite_compact_status("Scenario batch", "computed", sprintf("%d scenarios computed.", nrow(scenario_results())), "ready"))
      }
      suite_compact_status("Scenario batch", "needs input", "Upload a scenario CSV or generate scenarios from the current Design.", "pending")
    })

    output$scenario_input <- renderTable({
      data <- scenario_data()
      if (is.null(data)) return(ssize_scenario_template(ssize_rv$design)[0, ])
      data
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$scenario_results <- renderTable({
      data <- scenario_results()
      if (is.null(data)) {
        return(data.frame(message = "No scenario results yet.", stringsAsFactors = FALSE))
      }
      data
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$dl_template <- downloadHandler(
      filename = function() sprintf("ssize_scenario_template_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) utils::write.csv(ssize_scenario_template(ssize_rv$design), file, row.names = FALSE)
    )

    output$dl_results <- downloadHandler(
      filename = function() sprintf("ssize_scenario_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        data <- scenario_results()
        if (is.null(data)) data <- data.frame(message = "No scenario results yet.", stringsAsFactors = FALSE)
        utils::write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

# =============================================================================
# SENSITIVITY tab
# =============================================================================

module_UI_ssize_sensitivity <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sensitivity"),
    p(class = "suite-muted", "Sweep one design parameter at a time and recompute required N/events using the current Design as the base case."),
    fluidRow(
      column(4, selectInput(ns("parameter"), "Parameter", choices = character(0))),
      column(5, textInput(ns("values"), "Grid values", value = "0.7,0.8,0.9")),
      column(3, br(), actionButton(ns("run"), "Run sensitivity", class = "btn-primary", width = "100%"))
    ),
    uiOutput(ns("status")),
    h4("Sensitivity table"),
    suite_scroll_table(tableOutput(ns("sensitivity_table")), max_height = "320px", min_width = "900px"),
    h4("Sensitivity plot"),
    plotOutput(ns("sensitivity_plot"), height = "320px"),
    downloadButton(ns("dl_sensitivity"), "Download sensitivity results")
  )
}

module_server_ssize_sensitivity <- function(id, ssize_rv) {
  moduleServer(id, function(input, output, session) {
    sensitivity_results <- reactiveVal(NULL)
    sensitivity_error <- reactiveVal(NULL)

    parameter_choices <- reactive({
      d <- ssize_rv$design
      common <- c("power", "alpha", "dropout", "allocation", "margin")
      endpoint <- switch(
        d$endpoint,
        binary = if (identical(d$design_type, "one-sample")) c("p0", "p1") else c("pc", "pt"),
        continuous = c("mu_c", "mu_t", "sd", "correlation"),
        survival = c("hr", "median_c", "median_t", "accrual", "followup", "event_prob"),
        character(0)
      )
      unique(c(endpoint, common))
    })

    observe({
      choices <- parameter_choices()
      selected <- if ("power" %in% choices) "power" else choices[[1]]
      updateSelectInput(session, "parameter", choices = choices, selected = selected)
    })

    observeEvent(input$parameter, {
      d <- ssize_rv$design
      current <- suppressWarnings(as.numeric(d[[input$parameter]]))
      if (!is.finite(current)) current <- switch(input$parameter, hr = 0.7, event_prob = 0.5, power = 0.8, alpha = 0.05, 1)
      values <- sort(unique(round(c(current * 0.8, current, current * 1.2), 4)))
      if (identical(input$parameter, "power")) values <- c(0.7, 0.8, 0.9)
      if (identical(input$parameter, "alpha")) values <- c(0.01, 0.025, 0.05, 0.1)
      if (identical(input$parameter, "dropout")) values <- c(0, 0.1, 0.2)
      updateTextInput(session, "values", value = paste(values, collapse = ","))
    }, ignoreInit = TRUE)

    observeEvent(input$run, {
      vals <- suppressWarnings(as.numeric(trimws(strsplit(input$values %||% "", ",", fixed = TRUE)[[1]])))
      out <- tryCatch(ssize_sensitivity_grid(ssize_rv$design, input$parameter, vals), error = function(e) e)
      if (inherits(out, "error")) {
        sensitivity_results(NULL)
        sensitivity_error(conditionMessage(out))
      } else {
        sensitivity_results(out)
        sensitivity_error(NULL)
      }
    }, ignoreInit = TRUE)

    output$status <- renderUI({
      if (!is.null(sensitivity_error())) {
        return(suite_compact_status("Sensitivity", "needs review", sensitivity_error(), "warn"))
      }
      if (!is.null(sensitivity_results())) {
        return(suite_compact_status("Sensitivity", "computed", sprintf("%d grid points computed.", nrow(sensitivity_results())), "ready"))
      }
      suite_compact_status("Sensitivity", "needs input", "Choose a parameter, enter comma-separated values, and run.", "pending")
    })

    output$sensitivity_table <- renderTable({
      data <- sensitivity_results()
      if (is.null(data)) return(data.frame(message = "No sensitivity results yet.", stringsAsFactors = FALSE))
      data
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$sensitivity_plot <- renderPlot({
      data <- sensitivity_results()
      req(data)
      y <- if (all(is.na(data$n_total)) && any(!is.na(data$events))) data$events else data$n_total
      ylab <- if (all(is.na(data$n_total)) && any(!is.na(data$events))) "Required events" else "Total sample size"
      graphics::plot(
        data$value, y,
        type = "b", pch = 19, lwd = 2,
        xlab = data$parameter[[1]],
        ylab = ylab,
        col = "#333333"
      )
      graphics::grid(col = "#e6e6e6")
    })

    output$dl_sensitivity <- downloadHandler(
      filename = function() sprintf("ssize_sensitivity_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        data <- sensitivity_results()
        if (is.null(data)) data <- data.frame(message = "No sensitivity results yet.", stringsAsFactors = FALSE)
        utils::write.csv(data, file, row.names = FALSE)
      }
    )
  })
}

# =============================================================================
# METHODS tab (live)
# =============================================================================

module_UI_ssize_methods <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Methods"),
    uiOutput(ns("methods"))
  )
}

module_server_ssize_methods <- function(id, ssize_rv) {
  moduleServer(id, function(input, output, session) {
    output$methods <- renderUI({
      d <- ssize_rv$design
      generic <- tags$ul(
        tags$li("Binary: exact binomial (one-sample) and normal-approximation proportion tests."),
        tags$li("Continuous: one-sample, paired, and two-sample t-tests solved via the noncentral t distribution."),
        tags$li("Time-to-event: Schoenfeld required events under an exponential/log-rank assumption, translated to total N via the event probability.")
      )
      if (!ssize_result_ready(ssize_rv)) {
        return(tagList(
          suite_stage_notice("Formula and assumptions",
            "The exact formula, hypothesis statement, and parameter definitions for the current design appear here once a valid calculation is available."),
          h4("Method families"), generic
        ))
      }
      res <- ssize_rv$result
      hyp <- switch(
        res$test_objective,
        "equality" = "H0: no difference vs H1: a difference of the stated size.",
        "superiority" = "H0: difference <= margin vs H1: difference > margin.",
        "noninferiority" = "H0: treatment worse than control by more than the margin vs H1: non-inferior.",
        "equivalence" = "H0: |difference| >= margin vs H1: |difference| < margin.",
        "Hypothesis defined by the selected objective."
      )
      tagList(
        h4("Current design"),
        tags$p(sprintf("%s, %s, %s, %s.", res$endpoint, res$design_type, res$test_objective, res$sided)),
        h4("Hypothesis"), tags$p(hyp),
        h4("Method narrative"), tags$p(ssize_method_narrative(res)),
        h4("Formula"), div(class = "suite-mono", res$formula),
        h4("Method id"), div(class = "suite-mono", res$method),
        h4("Resolved inputs"),
        ssize_assump_table(res$assumptions),
        h4("Rounding and traceability"),
        tags$ul(
          tags$li("Raw sample sizes are rounded upward after dropout inflation."),
          tags$li("ADaM-derived assumptions are estimated before the Design calculation and then passed to the same engine used for manual assumptions."),
          tags$li("Scenario and sensitivity results reuse the identical dispatcher and do not maintain a separate formula implementation.")
        ),
        h4("Method families"), generic,
        tags$details(
          tags$summary("Validation references"),
          tags$ul(
            tags$li("Continuous one-sample d = 0.3, 1-sided alpha 0.05, power 0.8 -> N = 71."),
            tags$li("Survival one-sample HR ~ 2.29, 1-sided alpha 0.05, power 0.8 -> 9 events."),
            tags$li("Binary one-sample exact p0 = 0.2, p1 = 0.5, 1-sided -> N ~ 18-19 (exact-binomial threshold convention).")
          )
        )
      )
    })
  })
}

# =============================================================================
# REPORT tab (live)
# =============================================================================

module_UI_ssize_report <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Report"),
    uiOutput(ns("report")),
    uiOutput(ns("download_ui"))
  )
}

module_server_ssize_report <- function(id, ssize_rv) {
  moduleServer(id, function(input, output, session) {

    report_text <- reactive({
      if (!ssize_result_ready(ssize_rv)) return(NULL)
      res <- ssize_rv$result
      prov <- ssize_rv$provenance
      lines <- c(
        "SAMPLE SIZE DESIGN REPORT",
        sprintf("Generated: %s", ssize_rv$result_stamp %||% format(Sys.time())),
        sprintf("Provenance: %s (%s)", prov$source %||% "Manual assumptions", prov$detail %||% ""),
        "",
        "Design:",
        sprintf("  Endpoint: %s", res$endpoint),
        sprintf("  Design type: %s", res$design_type),
        sprintf("  Objective: %s", res$test_objective),
        sprintf("  Sided: %s", res$sided),
        "",
        "Primary result:",
        if (!is.null(res$events)) sprintf("  Required events: %d", res$events) else NULL,
        if (!is.null(res$n_total)) sprintf("  Total sample size: %d", res$n_total) else NULL,
        if (!is.null(res$n_per_arm) && length(res$n_per_arm) > 1)
          sprintf("  Per arm: %s", paste(sprintf("%s=%d", names(res$n_per_arm), as.integer(res$n_per_arm)), collapse = ", ")) else NULL,
        if (!is.null(res$achieved_power)) sprintf("  Achieved power: %.3f", res$achieved_power) else NULL,
        "",
        "Method:",
        sprintf("  %s", res$method),
        sprintf("  %s", res$formula),
        strwrap(ssize_method_narrative(res), width = 90, prefix = "  "),
        "",
        "Traceable assumptions:",
        apply(res$assumptions, 1, function(x) sprintf("  - %s: %s [%s]", x[["parameter"]], x[["value"]], x[["source"]])),
        "",
        "Protocol paragraph:",
        strwrap(ssize_report_protocol_paragraph(res, prov), width = 90, prefix = "  ")
      )
      paste(Filter(Negate(is.null), lines), collapse = "\n")
    })

    output$report <- renderUI({
      if (!ssize_result_ready(ssize_rv)) {
        return(suite_stage_notice("Protocol-ready report",
          "A concise sample-size paragraph and downloadable summary appear here once a valid calculation is available."))
      }
      tagList(
        tags$pre(style = "background:#f6f6f8; border-radius:6px; padding:12px; white-space:pre-wrap;",
                 report_text())
      )
    })

    output$download_ui <- renderUI({
      if (!ssize_result_ready(ssize_rv)) return(NULL)
      tagList(
        downloadButton(session$ns("dl_txt"), "Download summary (TXT)"),
        downloadButton(session$ns("dl_csv"), "Download assumptions (CSV)")
      )
    })

    output$dl_txt <- downloadHandler(
      filename = function() sprintf("ssize_report_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) writeLines(report_text() %||% "No result.", file)
    )
    output$dl_csv <- downloadHandler(
      filename = function() sprintf("ssize_assumptions_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        df <- if (ssize_result_ready(ssize_rv)) ssize_rv$result$assumptions else
          data.frame(parameter = character(0), value = character(0), source = character(0))
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
