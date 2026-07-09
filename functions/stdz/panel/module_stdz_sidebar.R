module_UI_stdz_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      ns("mode"),
      suite_help_label(
        "Data source",
        "Upload raw/source-like data for real work, or load the staged public examples for local validation."
      ),
      choices = c("Upload raw data" = "upload", "Load example dataset" = "example"),
      selected = "example",
      width = "100%"
    ),
    uiOutput(ns("example_selector")),
    uiOutput(ns("reference_summary")),
    fileInput(
      ns("raw_upload"),
      "Upload raw/source data",
      accept = c(".csv", ".xlsx", ".sas7bdat", ".xpt", ".rds")
    ),
    tags$hr(style = "margin:8px 0;"),
    tags$h4("Study Metadata"),
    textInput(ns("study_id"), "Study ID", value = "DEMO-001"),
    selectInput(
      ns("phase"),
      "Phase",
      choices = c("Phase I", "Phase II", "Phase III", "Observational"),
      selected = "Phase II"
    ),
    selectInput(
      ns("standard"),
      "Target standard",
      choices = c("SDTM/ADaM MVP", "SDTMIG + ADaMIG", "TLF-ready only"),
      selected = "SDTM/ADaM MVP"
    ),
    tags$hr(style = "margin:8px 0;"),
    tags$h4("Build Targets"),
    checkboxGroupInput(
      ns("targets"),
      NULL,
      choices = c("SDTM domains", "ADaM datasets", "TLF-ready summaries", "QC package"),
      selected = c("SDTM domains", "ADaM datasets", "QC package")
    ),
    helpText("Profile uses either a staged public example or the uploaded file. Downstream stages remain gated until profile is successful.")
  )
}

module_server_stdz_sidebar <- function(id, stdz_rv) {
  moduleServer(id, function(input, output, session) {
    examples <- stdz_available_examples()
    references <- stdz_available_reference_datasets()

    output$example_selector <- renderUI({
      if (nrow(examples) == 0) {
        return(tags$div(
          class = "text-muted",
          style = "margin-bottom:10px;",
          "No raw/source-like staged example is available. Use Upload raw data or run the public-resource preparation script."
        ))
      }
      selectInput(
        session$ns("example_key"),
        "Staged raw/source-like example",
        choices = stats::setNames(examples$key, examples$label),
        selected = if ("phuse_tdf_dm" %in% examples$key) "phuse_tdf_dm" else examples$key[[1]]
      )
    })

    output$reference_summary <- renderUI({
      if (nrow(references) == 0) return(NULL)
      standards <- paste(sort(unique(references$standard)), collapse = " / ")
      tags$div(
        class = "text-muted",
        style = "font-size:12px;margin:-4px 0 10px 0;line-height:1.35;",
        sprintf(
          "%d %s standard reference datasets are available for Standards Build/QC and sample-size estimation; they are not raw Profile inputs.",
          nrow(references),
          standards
        )
      )
    })

    observe({
      stdz_rv$mode <- input$mode
      stdz_rv$study$study_id <- input$study_id
      stdz_rv$study$phase <- input$phase
      stdz_rv$study$standard <- input$standard
      stdz_rv$study$targets <- input$targets
    })

    observe({
      req(input$mode)
      if (identical(input$mode, "example")) {
        validate(need(nrow(examples) > 0, "No raw/source-like example dataset is available."))
        req(input$example_key)
        row <- examples[examples$key == input$example_key, , drop = FALSE]
        req(nrow(row) == 1)
        stdz_rv$data_source <- list(
          mode = "example",
          example_key = row$key[[1]],
          label = row$label[[1]],
          path = row$path[[1]],
          uploaded_path = NULL,
          uploaded_name = NULL
        )
      } else {
        upload <- input$raw_upload
        stdz_rv$data_source <- list(
          mode = "upload",
          example_key = NULL,
          label = if (is.null(upload)) "No uploaded file" else upload$name,
          path = if (is.null(upload)) NULL else upload$datapath,
          uploaded_path = if (is.null(upload)) NULL else upload$datapath,
          uploaded_name = if (is.null(upload)) NULL else upload$name
        )
      }
    })
  })
}
