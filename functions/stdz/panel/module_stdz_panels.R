module_UI_stdz_profile <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Raw Data Profile"),
    uiOutput(ns("summary")),
    uiOutput(ns("error")),
    h4("Overview"),
    tableOutput(ns("overview")),
    h4("Readiness checks"),
    tableOutput(ns("readiness")),
    h4("Variable profile"),
    tableOutput(ns("variables")),
    tags$details(
      tags$summary("Show first 10 rows"),
      tableOutput(ns("preview"))
    )
  )
}

module_server_stdz_profile <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    profile_result <- reactive({
      src <- stdz_rv$data_source
      if (is.null(src$path) || !nzchar(src$path)) {
        stop("No data source selected. Choose a staged example or upload a raw/source file.", call. = FALSE)
      }
      dat <- stdz_read_table(src$path)
      stdz_profile_table(dat, src$label, src$path)
    })

    observe({
      result <- tryCatch(
        {
          profile_result()
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        stdz_rv$profile <- NULL
        stdz_rv$profile_error <- conditionMessage(result)
        stdz_rv$stage$profile <- FALSE
        stdz_rv$profile_source_id <- NULL
        stdz_rv$mapping <- NULL
        stdz_rv$mapping_error <- NULL
        stdz_rv$mapping_upload_error <- NULL
        stdz_rv$mapping_source <- NULL
        stdz_rv$mapping_validation <- NULL
        stdz_rv$build <- NULL
        stdz_rv$build_error <- NULL
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$mapping <- FALSE
        stdz_rv$stage$build <- FALSE
        stdz_rv$stage$qc <- FALSE
      } else {
        source_id <- paste(stdz_rv$data_source$mode, stdz_rv$data_source$label, stdz_rv$data_source$path, sep = "|")
        if (!identical(stdz_rv$profile_source_id, source_id)) {
          stdz_rv$mapping <- NULL
          stdz_rv$mapping_error <- NULL
          stdz_rv$mapping_upload_error <- NULL
          stdz_rv$mapping_source <- NULL
          stdz_rv$mapping_validation <- NULL
          stdz_rv$build <- NULL
          stdz_rv$build_error <- NULL
          stdz_rv$qc <- NULL
          stdz_rv$qc_error <- NULL
          stdz_rv$stage$mapping <- FALSE
          stdz_rv$stage$build <- FALSE
          stdz_rv$stage$qc <- FALSE
          stdz_rv$profile_source_id <- source_id
        }
        stdz_rv$profile <- result
        stdz_rv$profile_error <- NULL
        stdz_rv$stage$profile <- TRUE
      }
    })

    output$summary <- renderUI({
      src <- stdz_rv$data_source
      suite_compact_status(
        "Profile",
        if (isTRUE(stdz_rv$stage$profile)) "ready" else "pending",
        sprintf(
          "Mode: %s. Study: %s. Source: %s.",
          src$mode,
          stdz_rv$study$study_id,
          src$label
        ),
        if (isTRUE(stdz_rv$stage$profile)) "ready" else "pending"
      )
    })

    output$error <- renderUI({
      if (is.null(stdz_rv$profile_error)) return(NULL)
      suite_stage_notice("Profile error", stdz_rv$profile_error)
    })
    output$overview <- renderTable({
      req(stdz_rv$profile)
      stdz_rv$profile$overview
    }, striped = TRUE, bordered = TRUE, width = "100%")
    output$readiness <- renderTable({
      req(stdz_rv$profile)
      stdz_rv$profile$readiness
    }, striped = TRUE, bordered = TRUE, width = "100%")
    output$variables <- renderTable({
      req(stdz_rv$profile)
      head(stdz_rv$profile$variables, 25)
    }, striped = TRUE, bordered = TRUE, width = "100%")
    output$preview <- renderTable({
      req(stdz_rv$profile)
      stdz_rv$profile$preview
    }, striped = TRUE, bordered = TRUE, width = "100%")
  })
}

module_UI_stdz_mapping <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Mapping & Cleaning Rules"),
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    fluidRow(
      column(
        width = 6,
        fileInput(
          ns("mapping_upload"),
          suite_help_label(
            "Upload mapping specification",
            "Upload a CSV or XLSX that matches the currently profiled raw/source dataset. A generic template must be edited before upload."
          ),
          accept = c(".csv", ".xlsx")
        ),
        helpText("Uploaded mapping is accepted only when its source variables exist in the current Profile.")
      ),
      column(
        width = 6,
        tags$div(style = "margin-top:25px;"),
        downloadButton(ns("download_template"), "Download blank schema"),
        downloadButton(ns("download_current"), "Download current mapping"),
        uiOutput(ns("example_key_download"))
      )
    ),
    h4("Mapping summary"),
    tableOutput(ns("mapping_summary")),
    h4("Field recognition and target-variable recommendation"),
    suite_scroll_table(tableOutput(ns("mapping_table"))),
    tags$details(
      tags$summary("Show source variable profile used for recommendation"),
      suite_scroll_table(tableOutput(ns("source_profile")))
    ),
    tags$details(
      tags$summary("Show required template columns"),
      suite_placeholder_table(list(
        c("source_table", "source_variable", "target_layer", "target_dataset"),
        c("target_variable", "variable_label", "cleaning_rule", "derivation_rule"),
        c("required", "confidence", "recommendation_source", "notes")
      ))
    )
  )
}

module_server_stdz_mapping <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    output$gate <- renderUI({
      suite_gate(
        stdz_rv$stage$profile,
        "Mapping",
        "Run a successful Profile stage before defining mapping and cleaning rules.",
        NULL
      )
    })

    recommended_mapping <- reactive({
      req(stdz_rv$stage$profile)
      stdz_recommend_mapping(stdz_rv$profile, stdz_rv$data_source)
    })

    observe({
      req(stdz_rv$stage$profile)
      if (!is.null(input$mapping_upload) && !is.null(stdz_rv$mapping_source)) return()
      result <- tryCatch(
        {
          mapping <- recommended_mapping()
          validation <- stdz_validate_mapping(mapping, stdz_rv$profile)
          list(mapping = validation$mapping, validation = validation, source = "recommended")
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        stdz_rv$mapping <- NULL
        stdz_rv$mapping_error <- conditionMessage(result)
        stdz_rv$mapping_upload_error <- NULL
        stdz_rv$mapping_source <- NULL
        stdz_rv$mapping_validation <- NULL
        stdz_rv$build <- NULL
        stdz_rv$build_error <- NULL
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$mapping <- FALSE
        stdz_rv$stage$build <- FALSE
        stdz_rv$stage$qc <- FALSE
      } else {
        stdz_rv$mapping <- result$mapping
        stdz_rv$mapping_error <- NULL
        stdz_rv$mapping_upload_error <- NULL
        stdz_rv$mapping_source <- result$source
        stdz_rv$mapping_validation <- result$validation
        stdz_rv$stage$mapping <- isTRUE(result$validation$ok)
        stdz_rv$build <- NULL
        stdz_rv$build_error <- NULL
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$build <- FALSE
        stdz_rv$stage$qc <- FALSE
      }
    })

    observeEvent(input$mapping_upload, {
      req(stdz_rv$stage$profile)
      upload <- input$mapping_upload

      # Any mapping-upload attempt invalidates downstream stages until the
      # uploaded specification is validated and accepted.
      stdz_rv$build <- NULL
      stdz_rv$build_error <- NULL
      stdz_rv$qc <- NULL
      stdz_rv$qc_error <- NULL
      stdz_rv$stage$build <- FALSE
      stdz_rv$stage$qc <- FALSE

      result <- tryCatch(
        {
          mapping <- stdz_read_mapping_file(upload$datapath)
          validation <- stdz_validate_mapping(mapping, stdz_rv$profile)
          list(mapping = validation$mapping, validation = validation, source = paste("uploaded:", upload$name))
        },
        error = function(e) e
      )

      if (!inherits(result, "error") && isTRUE(result$validation$ok)) {
        # Accepted: apply the uploaded mapping exactly as provided.
        stdz_rv$mapping <- result$mapping
        stdz_rv$mapping_error <- NULL
        stdz_rv$mapping_upload_error <- NULL
        stdz_rv$mapping_source <- result$source
        stdz_rv$mapping_validation <- result$validation
        stdz_rv$stage$mapping <- TRUE
      } else {
        # HARD BLOCK: a rejected mapping is never silently replaced by a
        # machine-recommended one. The pipeline stops (Build stays gated)
        # until the user fixes and re-uploads, or clears the file to fall
        # back to the recommendation explicitly.
        reason <- if (inherits(result, "error")) {
          conditionMessage(result)
        } else {
          paste(result$validation$errors, collapse = " ")
        }
        stdz_rv$mapping <- NULL
        stdz_rv$mapping_error <- NULL
        stdz_rv$mapping_validation <- if (inherits(result, "error")) NULL else result$validation
        stdz_rv$mapping_source <- "upload rejected"
        stdz_rv$stage$mapping <- FALSE
        stdz_rv$mapping_upload_error <- paste(
          "Uploaded mapping rejected and NOT applied (pipeline blocked):", reason
        )
      }
    })

    output$status <- renderUI({
      req(stdz_rv$stage$profile)
      upload_notice <- if (is.null(stdz_rv$mapping_upload_error)) {
        NULL
      } else {
        suite_stage_notice(
          tagList("Upload validation", suite_status_pill("rejected", "warn")),
          stdz_rv$mapping_upload_error
        )
      }
      if (!is.null(stdz_rv$mapping_error)) {
        return(tagList(
          suite_stage_notice(
            tagList("Mapping validation", suite_status_pill("needs review", "warn")),
            stdz_rv$mapping_error
          ),
          upload_notice
        ))
      }
      source <- if (is.null(stdz_rv$mapping_source)) "not available" else stdz_rv$mapping_source
      tagList(
        suite_compact_status(
          "Mapping",
          if (isTRUE(stdz_rv$stage$mapping)) "valid" else "pending",
          sprintf("Source: %s. Use the current mapping for build, or upload a curated mapping that matches this Profile.", source),
          if (isTRUE(stdz_rv$stage$mapping)) "ready" else "pending"
        ),
        upload_notice
      )
    })

    output$mapping_summary <- renderTable({
      req(stdz_rv$stage$profile)
      stdz_mapping_summary(stdz_rv$mapping)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$mapping_table <- renderTable({
      req(stdz_rv$stage$profile)
      req(stdz_rv$mapping)
      head(stdz_rv$mapping, 30)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$source_profile <- renderTable({
      req(stdz_rv$stage$profile)
      stdz_rv$profile$variables
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$example_key_download <- renderUI({
      req(stdz_rv$stage$profile)
      examples <- stdz_available_examples()
      key <- stdz_rv$data_source$example_key
      if (is.null(key) || !key %in% examples$key) return(NULL)
      row <- examples[examples$key == key, , drop = FALSE]
      if (nrow(row) != 1 || is.na(row$mapping_key) || !nzchar(row$mapping_key) || !file.exists(row$mapping_key)) return(NULL)
      tags$div(
        style = "display:inline-block;margin-left:4px;",
        downloadButton(session$ns("download_example_key"), "Download example mapping key")
      )
    })

    output$download_template <- downloadHandler(
      filename = function() {
        "mapping_template_blank.csv"
      },
      content = function(file) {
        template <- as.data.frame(setNames(rep(list(character()), length(stdz_mapping_template_columns())), stdz_mapping_template_columns()))
        write.csv(template, file, row.names = FALSE, na = "")
      }
    )

    output$download_current <- downloadHandler(
      filename = function() {
        paste0("mapping_current_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        mapping <- stdz_rv$mapping
        if (is.null(mapping)) mapping <- recommended_mapping()
        write.csv(stdz_standardize_mapping_columns(mapping), file, row.names = FALSE, na = "")
      }
    )

    output$download_example_key <- downloadHandler(
      filename = function() {
        paste0(stdz_rv$data_source$example_key, "_mapping_key.csv")
      },
      content = function(file) {
        examples <- stdz_available_examples()
        key <- stdz_rv$data_source$example_key
        row <- examples[examples$key == key, , drop = FALSE]
        req(nrow(row) == 1, !is.na(row$mapping_key), nzchar(row$mapping_key), file.exists(row$mapping_key))
        file.copy(row$mapping_key, file, overwrite = TRUE)
      }
    )
  })
}

module_UI_stdz_build <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Standards Build"),
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    actionButton(ns("run_build"), "Build standards outputs", icon = icon("play")),
    h4("Build summary"),
    tableOutput(ns("summary")),
    h4("TLF-ready dataset summary"),
    tableOutput(ns("tlf_ready")),
    h4("Built dataset preview"),
    uiOutput(ns("dataset_selector")),
    suite_scroll_table(tableOutput(ns("dataset_preview")), max_height = "360px", min_width = "900px"),
    h4("Traceability"),
    suite_scroll_table(tableOutput(ns("traceability")), max_height = "360px", min_width = "1300px")
  )
}

module_server_stdz_build <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    output$gate <- renderUI({
      suite_gate(
        stdz_rv$stage$mapping,
        "Standards Build",
        "Complete Mapping before building SDTM, ADaM, or TLF-ready outputs.",
        suite_compact_status(
          "Standards Build",
          if (isTRUE(stdz_rv$stage$build)) "built" else "ready",
          "Generate MVP SDTM/ADaM/TLF-ready outputs from the current mapping.",
          if (isTRUE(stdz_rv$stage$build)) "ready" else "pending"
        )
      )
    })

    observeEvent(input$run_build, {
      req(stdz_rv$stage$mapping)
      result <- tryCatch(
        stdz_build_standards(stdz_rv$data_source, stdz_rv$study, stdz_rv$mapping),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        stdz_rv$build <- NULL
        stdz_rv$build_error <- conditionMessage(result)
        stdz_rv$stage$build <- FALSE
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$qc <- FALSE
      } else {
        stdz_rv$build <- result
        stdz_rv$build_error <- NULL
        stdz_rv$stage$build <- TRUE
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$qc <- FALSE
      }
    })

    output$status <- renderUI({
      req(stdz_rv$stage$mapping)
      if (!is.null(stdz_rv$build_error)) {
        return(suite_stage_notice(
          tagList("Build error", suite_status_pill("needs review", "warn")),
          stdz_rv$build_error
        ))
      }
      if (isTRUE(stdz_rv$stage$build)) {
        return(suite_compact_status(
          "Build output",
          "complete",
          paste("Datasets:", paste(names(stdz_rv$build$datasets), collapse = ", ")),
          "ready"
        ))
      }
      NULL
    })

    output$summary <- renderTable({
      req(stdz_rv$build)
      stdz_rv$build$summary
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$tlf_ready <- renderTable({
      req(stdz_rv$build)
      stdz_rv$build$tlf_ready
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$dataset_selector <- renderUI({
      req(stdz_rv$build)
      selectInput(session$ns("dataset_name"), "Dataset", choices = names(stdz_rv$build$datasets), selected = names(stdz_rv$build$datasets)[[1]])
    })

    output$dataset_preview <- renderTable({
      req(stdz_rv$build, input$dataset_name)
      head(stdz_rv$build$datasets[[input$dataset_name]], 20)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$traceability <- renderTable({
      req(stdz_rv$build)
      stdz_rv$build$traceability
    }, striped = TRUE, bordered = TRUE, width = "100%")
  })
}

module_UI_stdz_qc <- function(id) {
  ns <- NS(id)
  tagList(
    h3("QC & Traceability"),
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    actionButton(ns("run_qc"), "Run QC checks", icon = icon("check")),
    downloadButton(ns("download_qc_report"), "Download QC report"),
    h4("QC summary"),
    tableOutput(ns("summary")),
    h4("Rule checks"),
    suite_scroll_table(tableOutput(ns("checks")), max_height = "360px", min_width = "1000px"),
    h4("Reference difference check"),
    suite_scroll_table(tableOutput(ns("diff")), max_height = "260px", min_width = "1000px"),
    h4("Traceability"),
    suite_scroll_table(tableOutput(ns("traceability")), max_height = "360px", min_width = "1300px")
  )
}

module_server_stdz_qc <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    output$gate <- renderUI({
      suite_gate(
        stdz_rv$stage$build,
        "QC & Traceability",
        "Build standards outputs before running QC and traceability checks.",
        suite_compact_status(
          "QC & Traceability",
          if (isTRUE(stdz_rv$stage$qc)) "complete" else "ready",
          "Run rule checks, reference difference checks, and traceability review.",
          if (isTRUE(stdz_rv$stage$qc)) "ready" else "pending"
        )
      )
    })

    observeEvent(input$run_qc, {
      req(stdz_rv$stage$build)
      result <- tryCatch(
        stdz_run_qc(stdz_rv$build, stdz_rv$data_source),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        stdz_rv$qc <- NULL
        stdz_rv$qc_error <- conditionMessage(result)
        stdz_rv$stage$qc <- FALSE
      } else {
        stdz_rv$qc <- result
        stdz_rv$qc_error <- NULL
        stdz_rv$stage$qc <- !identical(result$status, "fail")
      }
    })

    output$status <- renderUI({
      req(stdz_rv$stage$build)
      if (!is.null(stdz_rv$qc_error)) {
        return(suite_stage_notice(
          tagList("QC error", suite_status_pill("needs review", "warn")),
          stdz_rv$qc_error
        ))
      }
      if (!is.null(stdz_rv$qc)) {
        return(suite_compact_status(
          "QC result",
          stdz_rv$qc$status,
          if (identical(stdz_rv$qc$status, "pass")) "No blocking issues detected." else "Warnings should be reviewed before export.",
          if (identical(stdz_rv$qc$status, "pass")) "ready" else "warn"
        ))
      }
      NULL
    })

    output$summary <- renderTable({
      req(stdz_rv$qc)
      stdz_rv$qc$summary
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$checks <- renderTable({
      req(stdz_rv$qc)
      stdz_rv$qc$checks
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$diff <- renderTable({
      req(stdz_rv$qc)
      stdz_rv$qc$diff
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$traceability <- renderTable({
      req(stdz_rv$qc)
      stdz_rv$qc$traceability
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$download_qc_report <- downloadHandler(
      filename = function() {
        paste0("qc_traceability_report_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(stdz_rv$qc)
        write.csv(stdz_rv$qc$report, file, row.names = FALSE, na = "")
      }
    )
  })
}

module_UI_stdz_export <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Export"),
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    fluidRow(
      column(
        width = 12,
        downloadButton(ns("download_package"), "Download delivery package (.zip)"),
        downloadButton(ns("download_datasets"), "Download built datasets (.zip)"),
        downloadButton(ns("download_mapping"), "Download mapping"),
        downloadButton(ns("download_qc"), "Download QC report"),
        downloadButton(ns("download_traceability"), "Download traceability")
      )
    ),
    h4("Package manifest"),
    tableOutput(ns("manifest")),
    h4("Package contents"),
    tags$ul(
      tags$li("datasets/: built DM/AE/EX/LB/VS/RS/ADTTE CSV files as applicable."),
      tags$li("specifications/mapping_current.csv."),
      tags$li("qc/: QC report, rule checks, and reference difference checks."),
      tags$li("traceability/traceability.csv."),
      tags$li("tlf/tlf_ready_summary.csv."),
      tags$li("manifest.csv, build_summary.csv, qc_summary.csv.")
    )
  )
}

module_server_stdz_export <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    output$gate <- renderUI({
      suite_gate(
        stdz_rv$stage$qc,
        "Export",
        "Complete QC & Traceability before exporting a delivery package.",
        suite_compact_status(
          "Export",
          "ready",
          "Download built datasets, mapping, QC report, traceability, and delivery manifest.",
          "ready"
        )
      )
    })

    export_manifest <- reactive({
      req(stdz_rv$stage$qc, stdz_rv$build, stdz_rv$qc)
      stdz_export_manifest(stdz_rv$data_source, stdz_rv$study, stdz_rv$build, stdz_rv$qc)
    })

    output$status <- renderUI({
      req(stdz_rv$stage$qc)
      suite_compact_status(
        "Delivery package",
        "available",
        paste("Datasets:", paste(names(stdz_rv$build$datasets), collapse = ", ")),
        "ready"
      )
    })

    output$manifest <- renderTable({
      export_manifest()
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$download_package <- downloadHandler(
      filename = function() {
        paste0("clinical_data_delivery_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(stdz_rv$stage$qc, stdz_rv$build, stdz_rv$qc)
        stdz_create_export_package(file, stdz_rv$data_source, stdz_rv$study, stdz_rv$mapping, stdz_rv$build, stdz_rv$qc)
      },
      contentType = "application/zip"
    )

    output$download_datasets <- downloadHandler(
      filename = function() {
        paste0("built_datasets_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(stdz_rv$stage$qc, stdz_rv$build)
        root <- file.path(tempdir(), paste0("stdz_datasets_", as.integer(Sys.time())))
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
        for (name in names(stdz_rv$build$datasets)) {
          write.csv(stdz_rv$build$datasets[[name]], file.path(root, paste0(tolower(name), ".csv")), row.names = FALSE, na = "")
        }
        stdz_zip_files(file, root, list.files(root))
      },
      contentType = "application/zip"
    )

    output$download_mapping <- downloadHandler(
      filename = function() {
        paste0("mapping_current_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(stdz_rv$stage$qc, stdz_rv$mapping)
        write.csv(stdz_standardize_mapping_columns(stdz_rv$mapping), file, row.names = FALSE, na = "")
      }
    )

    output$download_qc <- downloadHandler(
      filename = function() {
        paste0("qc_report_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(stdz_rv$stage$qc, stdz_rv$qc)
        write.csv(stdz_rv$qc$report, file, row.names = FALSE, na = "")
      }
    )

    output$download_traceability <- downloadHandler(
      filename = function() {
        paste0("traceability_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(stdz_rv$stage$qc, stdz_rv$qc)
        write.csv(stdz_rv$qc$traceability, file, row.names = FALSE, na = "")
      }
    )
  })
}
