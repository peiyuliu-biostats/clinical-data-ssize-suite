module_UI_ssize_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      ns("mode"),
      suite_help_label(
        "Input mode",
        "Manual assumptions uses typed design parameters. Estimate assumptions from ADaM first estimates endpoint assumptions from example or uploaded ADaM-like data, then applies them to the same Design engine."
      ),
      choices = c(
        "Manual assumptions" = "manual",
        "Estimate assumptions from ADaM" = "estimate"
      ),
      selected = "manual",
      width = "100%"
    ),
    uiOutput(ns("mode_notice")),
    tags$hr(style = "margin:8px 0;"),
    radioButtons(
      ns("endpoint"),
      suite_help_label("Endpoint type", "Selects the endpoint family and the ADaM dataset expected for estimation: ADRS, ADSL, or ADTTE."),
      choices = c("Binary" = "binary", "Continuous" = "continuous", "Time-to-event" = "survival"),
      selected = "binary",
      width = "100%"
    ),
    uiOutput(ns("design_type_ui")),
    uiOutput(ns("objective_ui")),
    radioButtons(
      ns("sided"),
      "1 or 2 sided test?",
      choices = c("1-sided" = "one-sided", "2-sided" = "two-sided"),
      selected = "one-sided",
      inline = TRUE
    ),
    tags$hr(style = "margin:8px 0;"),
    tags$h4("Common Parameters"),
    fluidRow(
      column(6, numericInput(ns("alpha"), suite_help_label("Alpha", "Type I error rate. Two-sided tests use alpha/2 in the critical value."), value = 0.05, min = 0.001, max = 0.5, step = 0.005)),
      column(6, numericInput(ns("power"), suite_help_label("Power", "Target 1 - beta."), value = 0.8, min = 0.5, max = 0.999, step = 0.01))
    ),
    fluidRow(
      column(6, numericInput(ns("dropout"), suite_help_label("Dropout", "Proportion lost; N is inflated by 1/(1-dropout)."), value = 0, min = 0, max = 0.9, step = 0.05))
    ),
    tags$hr(style = "margin:8px 0;"),
    uiOutput(ns("mode_specific_ui"))
  )
}

module_server_ssize_sidebar <- function(id, ssize_rv, stdz_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    design_type_choices <- function(endpoint) {
      switch(
        endpoint,
        binary = c("One-sample" = "one-sample", "Two-sample" = "two-sample"),
        continuous = c("One-sample" = "one-sample", "Two-sample" = "two-sample", "Paired" = "paired"),
        survival = c("One-sample (vs reference)" = "one-sample", "Two-sample (log-rank)" = "two-sample")
      )
    }

    objective_choices <- function(endpoint) {
      if (identical(endpoint, "survival")) {
        c("Equality (log-rank)" = "equality")
      } else {
        c("Equality" = "equality", "Superiority" = "superiority",
          "Non-inferiority" = "noninferiority", "Equivalence" = "equivalence")
      }
    }

    output$mode_notice <- renderUI({
      if (identical(input$mode, "manual")) return(NULL)
      suite_compact_status(
        "ADaM estimation",
        "needs assumptions",
        "Choose an example or upload ADaM-like data, estimate endpoint assumptions, then apply them to Design.",
        "pending"
      )
    })

    output$design_type_ui <- renderUI({
      if (!identical(input$mode, "manual")) return(NULL)
      selectInput(
        ns("design_type"),
        "Design type",
        choices = design_type_choices(input$endpoint),
        selected = design_type_choices(input$endpoint)[[1]]
      )
    })

    output$objective_ui <- renderUI({
      if (!identical(input$mode, "manual")) return(NULL)
      selectInput(
        ns("test_objective"),
        suite_help_label("Test objective", "Equality tests a difference; superiority/non-inferiority/equivalence apply a margin on the effect scale."),
        choices = objective_choices(input$endpoint),
        selected = objective_choices(input$endpoint)[[1]]
      )
    })

    show_margin <- reactive({
      !is.null(input$test_objective) &&
        input$test_objective %in% c("superiority", "noninferiority", "equivalence")
    })

    manual_param_card <- function() {
      ep <- input$endpoint
      dt <- input$design_type
      if (is.null(dt)) dt <- design_type_choices(ep)[[1]]
      margin_ui <- if (isTRUE(show_margin())) {
        numericInput(ns("margin"), suite_help_label("Margin (delta)", "Non-inferiority / superiority / equivalence margin on the effect scale."), value = 0.1, step = 0.01)
      } else NULL

      if (identical(ep, "binary")) {
        if (identical(dt, "one-sample")) {
          tagList(
            radioButtons(ns("binary_method"), suite_help_label("Method", "Exact binomial is the default for one-sample proportions; normal approximation is provided for comparison."), choices = c("Exact binomial" = "exact", "Normal approx." = "normal"), selected = "exact", inline = TRUE),
            fluidRow(
              column(6, numericInput(ns("p0"), suite_help_label("Null proportion p0", "Response/event rate under H0."), value = 0.2, min = 0.001, max = 0.999, step = 0.01)),
              column(6, numericInput(ns("p1"), suite_help_label("Alt. proportion p1", "Response/event rate under H1."), value = 0.5, min = 0.001, max = 0.999, step = 0.01))
            ),
            margin_ui
          )
        } else {
          tagList(
            fluidRow(
              column(6, numericInput(ns("pc"), suite_help_label("Control rate pc", "Event/response proportion in the control arm."), value = 0.3, min = 0.001, max = 0.999, step = 0.01)),
              column(6, numericInput(ns("pt"), suite_help_label("Treatment rate pt", "Event/response proportion in the treatment arm."), value = 0.5, min = 0.001, max = 0.999, step = 0.01))
            ),
            numericInput(ns("allocation"), suite_help_label("Allocation ratio (t:c)", "n_treatment / n_control."), value = 1, min = 0.1, max = 10, step = 0.1),
            margin_ui
          )
        }
      } else if (identical(ep, "continuous")) {
        cor_ui <- if (identical(dt, "paired")) numericInput(ns("correlation"), suite_help_label("Paired correlation", "Correlation between paired measurements; larger reduces effective SD."), value = 0.5, min = -0.99, max = 0.99, step = 0.05) else NULL
        alloc_ui <- if (identical(dt, "two-sample")) numericInput(ns("allocation"), suite_help_label("Allocation ratio (t:c)", "n_treatment / n_control."), value = 1, min = 0.1, max = 10, step = 0.1) else NULL
        tagList(
          fluidRow(
            column(6, numericInput(ns("mu_c"), suite_help_label("Control mean", "Mean under control / reference."), value = 0, step = 0.1)),
            column(6, numericInput(ns("mu_t"), suite_help_label("Treatment mean", "Mean under treatment."), value = 0.5, step = 0.1))
          ),
          numericInput(ns("sd"), suite_help_label("SD", "Common standard deviation (per subject)."), value = 1, min = 0.0001, step = 0.1),
          alloc_ui, cor_ui, margin_ui
        )
      } else if (identical(dt, "one-sample")) {
        tagList(
          helpText("Provide medians (exponential) to derive HR, or override HR directly."),
          fluidRow(
            column(6, numericInput(ns("median_c"), suite_help_label("Reference median", "Reference/historical median survival."), value = 12, min = 0.1, step = 1)),
            column(6, numericInput(ns("median_t"), suite_help_label("New median", "Expected median under the new treatment."), value = 18, min = 0.1, step = 1))
          ),
          numericInput(ns("hr"), suite_help_label("Hazard ratio (optional)", "If provided, overrides the medians."), value = NA, min = 0.01, step = 0.05)
        )
      } else {
        tagList(
          helpText("Provide medians to derive HR and, with accrual/follow-up, translate events to total N."),
          fluidRow(
            column(6, numericInput(ns("median_c"), suite_help_label("Control median", "Median survival, control arm."), value = 12, min = 0.1, step = 1)),
            column(6, numericInput(ns("median_t"), suite_help_label("Treatment median", "Median survival, treatment arm."), value = 18, min = 0.1, step = 1))
          ),
          numericInput(ns("hr"), suite_help_label("Hazard ratio (optional)", "If provided, overrides the medians for the events calculation."), value = NA, min = 0.01, step = 0.05),
          numericInput(ns("allocation"), suite_help_label("Allocation ratio (t:c)", "n_treatment / n_control."), value = 1, min = 0.1, max = 10, step = 0.1),
          fluidRow(
            column(6, numericInput(ns("accrual"), suite_help_label("Accrual duration", "Uniform accrual window."), value = 12, min = 0, step = 1)),
            column(6, numericInput(ns("followup"), suite_help_label("Additional follow-up", "Follow-up after accrual ends."), value = 12, min = 0, step = 1))
          ),
          numericInput(ns("event_prob"), suite_help_label("Event probability (optional)", "Override the derived P(event); if set, accrual/follow-up are ignored for N."), value = NA, min = 0.001, max = 1, step = 0.01)
        )
      }
    }

    uploaded_file_with_ext <- reactive({
      req(input$adam_file)
      ext <- tools::file_ext(input$adam_file$name)
      if (!nzchar(ext)) stop("Uploaded ADaM-like data must have a file extension.", call. = FALSE)
      tmp <- tempfile(fileext = paste0(".", ext))
      file.copy(input$adam_file$datapath, tmp, overwrite = TRUE)
      tmp
    })

    built_adam <- reactive({
      if (is.null(stdz_rv) || is.null(stdz_rv$build) || is.null(stdz_rv$build$datasets)) {
        return(NULL)
      }
      target <- switch(input$endpoint, binary = "ADRS", continuous = "ADSL", survival = "ADTTE")
      data <- stdz_rv$build$datasets[[target]]
      if (is.null(data)) return(NULL)
      list(data = data, label = sprintf("built %s from Data Standardization", target), dataset = target)
    })

    adam_data <- reactive({
      if (!identical(input$mode, "estimate")) return(NULL)
      if (identical(input$adam_source, "upload")) {
        req(input$adam_file)
        data <- ssize_read_adam_upload(uploaded_file_with_ext())
        list(data = data, label = sprintf("uploaded ADaM-like file: %s", input$adam_file$name))
      } else if (identical(input$adam_source, "built")) {
        obj <- built_adam()
        if (is.null(obj)) {
          target <- switch(input$endpoint, binary = "ADRS", continuous = "ADSL", survival = "ADTTE")
          stop(sprintf("No built %s dataset is available. Complete Data Standardization -> Standards Build, or use example/upload ADaM.", target), call. = FALSE)
        }
        obj
      } else {
        obj <- ssize_read_staged_adam(input$endpoint)
        list(data = obj$data, label = obj$source$label)
      }
    })

    output$mode_specific_ui <- renderUI({
      if (identical(input$mode, "manual")) {
        return(tagList(
          tags$h4("Endpoint Parameters"),
          manual_param_card(),
          tags$hr(style = "margin:8px 0;"),
          actionButton(ns("calc"), "Calculate sample size", class = "btn-primary", width = "100%"),
          tags$div(style = "height:6px;"),
          helpText("Manual assumptions compute immediately on Calculate and on parameter changes. Results and provenance appear in Design, Methods, and Report.")
        ))
      }
      tagList(
        tags$h4("ADaM Data Source"),
        suite_compact_status(
          "Estimated design",
          "two-sample equality",
          "ADaM estimation derives endpoint assumptions for the initial two-arm equality design; margins remain in Manual assumptions.",
          "pending"
        ),
        radioButtons(
          ns("adam_source"),
          NULL,
          choices = c(
            "Use example ADaM" = "example",
            "Use built ADaM from Data Standardization" = "built",
            "Upload ADaM-like data" = "upload"
          ),
          selected = ssize_rv$adam_source %||% "example",
          width = "100%"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("adam_source")),
          fileInput(ns("adam_file"), "Upload ADaM-like data", accept = c(".csv", ".xlsx", ".sas7bdat", ".xpt", ".rds"))
        ),
        uiOutput(ns("adam_controls")),
        tags$hr(style = "margin:8px 0;"),
        actionButton(ns("estimate"), "Estimate assumptions", icon = icon("database"), width = "100%"),
        tags$div(style = "height:6px;"),
        actionButton(ns("apply_estimate"), "Apply estimates to Design", icon = icon("arrow-right"), class = "btn-primary", width = "100%"),
        helpText("Example, built, and uploaded ADaM-like data use the same estimation and Design calculation path.")
      )
    })

    output$adam_controls <- renderUI({
      req(identical(input$mode, "estimate"))
      obj <- tryCatch(adam_data(), error = function(e) e)
      if (inherits(obj, "error")) {
        return(suite_locked_notice("ADaM input needed", conditionMessage(obj)))
      }
      data <- obj$data
      arms <- tryCatch(ssize_default_arms(data), error = function(e) e)
      if (inherits(arms, "error")) {
        return(suite_locked_notice("ADaM input check", conditionMessage(arms)))
      }
      if (identical(input$endpoint, "binary")) {
        req("PARAMCD" %in% names(data))
        tagList(
          selectInput(ns("paramcd"), "ADRS PARAMCD", choices = sort(unique(data$PARAMCD)), selected = if ("RSP" %in% data$PARAMCD) "RSP" else unique(data$PARAMCD)[[1]]),
          textInput(ns("response_values"), "Response values", value = "Y,CR,PR"),
          fluidRow(
            column(6, selectInput(ns("control_arm"), "Control arm", choices = arms$arms, selected = arms$control)),
            column(6, selectInput(ns("treatment_arm"), "Treatment arm", choices = arms$arms, selected = arms$treatment))
          )
        )
      } else if (identical(input$endpoint, "continuous")) {
        numeric_vars <- ssize_numeric_candidates(data)
        if (length(numeric_vars) == 0) return(suite_locked_notice("Continuous variable", "No numeric analysis variable was found."))
        tagList(
          selectInput(ns("value_var"), "Continuous variable", choices = numeric_vars, selected = if ("AGE" %in% numeric_vars) "AGE" else numeric_vars[[1]]),
          fluidRow(
            column(6, selectInput(ns("control_arm"), "Control arm", choices = arms$arms, selected = arms$control)),
            column(6, selectInput(ns("treatment_arm"), "Treatment arm", choices = arms$arms, selected = arms$treatment))
          )
        )
      } else {
        req("PARAMCD" %in% names(data))
        tagList(
          selectInput(ns("paramcd"), "ADTTE PARAMCD", choices = sort(unique(data$PARAMCD)), selected = if ("OS" %in% data$PARAMCD) "OS" else unique(data$PARAMCD)[[1]]),
          fluidRow(
            column(6, selectInput(ns("control_arm"), "Control arm", choices = arms$arms, selected = arms$control)),
            column(6, selectInput(ns("treatment_arm"), "Treatment arm", choices = arms$arms, selected = arms$treatment))
          )
        )
      }
    })

    observe({
      ssize_rv$mode <- input$mode
      ssize_rv$endpoint <- input$endpoint
      ssize_rv$adam_source <- input$adam_source %||% ssize_rv$adam_source
      d <- ssize_rv$design
      d$endpoint <- input$endpoint
      if (identical(input$mode, "manual")) {
        if (!is.null(input$design_type)) d$design_type <- input$design_type
        if (!is.null(input$test_objective)) d$test_objective <- input$test_objective
      }
      d$sided <- input$sided
      d$alpha <- input$alpha
      d$power <- input$power
      d$dropout <- input$dropout %||% 0
      if (!is.null(input$margin)) d$margin <- input$margin
      if (!is.null(input$allocation)) d$allocation <- input$allocation
      if (!is.null(input$binary_method)) d$binary_method <- input$binary_method
      if (!is.null(input$p0)) d$p0 <- input$p0
      if (!is.null(input$p1)) d$p1 <- input$p1
      if (!is.null(input$pc)) d$pc <- input$pc
      if (!is.null(input$pt)) d$pt <- input$pt
      if (!is.null(input$mu_c)) d$mu_c <- input$mu_c
      if (!is.null(input$mu_t)) d$mu_t <- input$mu_t
      if (!is.null(input$sd)) d$sd <- input$sd
      if (!is.null(input$correlation)) d$correlation <- input$correlation
      if (!is.null(input$hr)) d$hr <- input$hr
      if (!is.null(input$median_c)) d$median_c <- input$median_c
      if (!is.null(input$median_t)) d$median_t <- input$median_t
      if (!is.null(input$accrual)) d$accrual <- input$accrual
      if (!is.null(input$followup)) d$followup <- input$followup
      if (!is.null(input$event_prob)) d$event_prob <- input$event_prob
      ssize_rv$design <- d
    })

    observeEvent(list(input$mode, input$endpoint, input$adam_source, input$adam_file$name), {
      ssize_rv$estimate <- NULL
      ssize_rv$estimate_error <- NULL
      ssize_rv$estimate_applied <- FALSE
    }, ignoreInit = TRUE)

    run_estimate <- function() {
      obj <- adam_data()
      ep <- input$endpoint
      result <- tryCatch({
        if (identical(ep, "binary")) {
          vals <- trimws(strsplit(input$response_values %||% "Y", ",", fixed = TRUE)[[1]])
          ssize_estimate_from_adam_data(ep, obj$data, obj$label, paramcd = input$paramcd, response_values = vals, control_arm = input$control_arm, treatment_arm = input$treatment_arm)
        } else if (identical(ep, "continuous")) {
          ssize_estimate_from_adam_data(ep, obj$data, obj$label, value_var = input$value_var, control_arm = input$control_arm, treatment_arm = input$treatment_arm)
        } else {
          ssize_estimate_from_adam_data(ep, obj$data, obj$label, paramcd = input$paramcd, control_arm = input$control_arm, treatment_arm = input$treatment_arm)
        }
      }, error = function(e) e)
      if (inherits(result, "error")) {
        ssize_rv$estimate <- NULL
        ssize_rv$estimate_error <- conditionMessage(result)
        ssize_rv$estimate_applied <- FALSE
      } else {
        ssize_rv$estimate <- result
        ssize_rv$estimate_error <- NULL
        ssize_rv$estimate_applied <- FALSE
        ssize_rv$adam_source_label <- obj$label
      }
      invisible(result)
    }

    run_calc <- function() {
      can_calc <- identical(ssize_rv$mode, "manual") || isTRUE(ssize_rv$estimate_applied)
      if (!can_calc) {
        ssize_rv$result <- NULL
        ssize_rv$result_error <- "Estimate assumptions from ADaM, then apply them to Design."
        return(invisible())
      }
      res <- tryCatch(ssize_dispatch(ssize_rv$design), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (isTRUE(res$ok)) {
        from_estimate <- isTRUE(ssize_rv$estimate_applied) && !is.null(ssize_rv$estimate)
        if (from_estimate && !is.null(res$assumptions)) {
          estimated_parameters <- c(as.character(ssize_rv$estimate$assumptions$parameter), "SD")
          res$assumptions$source[res$assumptions$parameter %in% estimated_parameters] <- "estimated from ADaM"
        }
        ssize_rv$result <- res
        ssize_rv$result_error <- NULL
        ssize_rv$result_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ssize_rv$provenance <- list(
          source = if (from_estimate) "Estimated from ADaM-like data" else "Manual assumptions",
          detail = if (from_estimate) ssize_rv$estimate$provenance else sprintf("%s / %s / %s", res$endpoint, res$design_type, res$test_objective),
          stamp = ssize_rv$result_stamp
        )
      } else {
        ssize_rv$result <- NULL
        ssize_rv$result_error <- res$message %||% "Calculation failed."
      }
      invisible()
    }

    apply_estimate <- function() {
      if (is.null(ssize_rv$estimate)) run_estimate()
      req(ssize_rv$estimate)
      d <- ssize_rv$design
      for (name in names(ssize_rv$estimate$design_updates)) d[[name]] <- ssize_rv$estimate$design_updates[[name]]
      d$alpha <- input$alpha
      d$power <- input$power
      d$dropout <- input$dropout %||% 0
      d$sided <- input$sided
      ssize_rv$design <- d
      ssize_rv$endpoint <- d$endpoint
      ssize_rv$estimate_applied <- TRUE
      run_calc()
      ssize_rv$apply_counter <- ssize_rv$apply_counter + 1
    }

    observeEvent(input$estimate, run_estimate(), ignoreInit = TRUE)
    observeEvent(input$apply_estimate, apply_estimate(), ignoreInit = TRUE)
    observeEvent(input$calc, run_calc(), ignoreInit = TRUE)

    observeEvent(ssize_rv$apply_counter, {
      est <- ssize_rv$estimate
      if (is.null(est) || is.null(est$design_updates)) return()
      u <- est$design_updates
      updateRadioButtons(session, "endpoint", selected = u$endpoint %||% ssize_rv$design$endpoint)
      if (!is.null(u$design_type)) updateSelectInput(session, "design_type", selected = u$design_type)
      if (!is.null(u$test_objective)) updateSelectInput(session, "test_objective", selected = u$test_objective)
      apply_input_updates <- function() {
        if (!is.null(u$p0)) updateNumericInput(session, "p0", value = u$p0)
        if (!is.null(u$p1)) updateNumericInput(session, "p1", value = u$p1)
        if (!is.null(u$pc)) updateNumericInput(session, "pc", value = u$pc)
        if (!is.null(u$pt)) updateNumericInput(session, "pt", value = u$pt)
        if (!is.null(u$binary_method)) updateRadioButtons(session, "binary_method", selected = u$binary_method)
        if (!is.null(u$mu_c)) updateNumericInput(session, "mu_c", value = u$mu_c)
        if (!is.null(u$mu_t)) updateNumericInput(session, "mu_t", value = u$mu_t)
        if (!is.null(u$sd)) updateNumericInput(session, "sd", value = u$sd)
        if (!is.null(u$hr)) updateNumericInput(session, "hr", value = u$hr)
        if (!is.null(u$median_c)) updateNumericInput(session, "median_c", value = u$median_c)
        if (!is.null(u$median_t)) updateNumericInput(session, "median_t", value = u$median_t)
        if (!is.null(u$event_prob)) updateNumericInput(session, "event_prob", value = u$event_prob)
        if (!is.null(u$allocation)) updateNumericInput(session, "allocation", value = u$allocation)
      }
      apply_input_updates()
      session$onFlushed(apply_input_updates, once = TRUE)
    }, ignoreInit = TRUE)

    design_sig <- reactive({
      d <- ssize_rv$design
      paste(ssize_rv$mode, isTRUE(ssize_rv$estimate_applied), d$endpoint, d$design_type, d$test_objective, d$sided,
            d$alpha, d$power, d$dropout, d$allocation, d$margin, d$binary_method,
            d$p0, d$p1, d$pc, d$pt, d$mu_c, d$mu_t, d$sd, d$correlation,
            d$hr, d$median_c, d$median_t, d$accrual, d$followup, d$event_prob,
            sep = "|")
    })
    observeEvent(design_sig(), {
      if (identical(ssize_rv$mode, "manual") || isTRUE(ssize_rv$estimate_applied)) run_calc()
    }, ignoreInit = FALSE)
  })
}
