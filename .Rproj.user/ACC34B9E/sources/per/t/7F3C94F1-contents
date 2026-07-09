library(shiny)
library(shinydashboard)

Sys.setlocale("LC_TIME", "English")

invisible(lapply(
  list.files("functions", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  source
))

ui <- dashboardPage(
  dashboardHeader(
    title = "Clinical Trial Data Standards & Sample Size Suite",
    tags$li(
      class = "dropdown",
      tags$style(HTML(suite_app_css()))
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Standardization", tabName = "stdz", icon = icon("database")),
      menuItem("Sample Size", tabName = "ssize", icon = icon("calculator")),
      menuItem("Methods Guide", tabName = "methods", icon = icon("book")),
      menuItem("GitHub", icon = icon("github"), href = "https://github.com/", newtab = TRUE),
      menuItem("Author", tabName = "author", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stdz",
        fluidRow(
          box(
            title = "Settings", width = 4, status = "primary",
            solidHeader = TRUE, collapsible = FALSE,
            module_UI_stdz_sidebar("stdz_sidebar")
          ),
          box(
            title = "Analysis Panels", width = 8, status = "warning",
            solidHeader = TRUE, collapsible = FALSE,
            tabBox(
              width = 12, id = "stdz_tabs",
              tabPanel("Profile", module_UI_stdz_profile("stdz_profile")),
              tabPanel("Mapping", module_UI_stdz_mapping("stdz_mapping")),
              tabPanel("Standards Build", module_UI_stdz_build("stdz_build")),
              tabPanel("QC & Traceability", module_UI_stdz_qc("stdz_qc")),
              tabPanel("Export", module_UI_stdz_export("stdz_export"))
            )
          )
        )
      ),
      tabItem(
        tabName = "ssize",
        fluidRow(
          box(
            title = "Settings", width = 4, status = "primary",
            solidHeader = TRUE, collapsible = FALSE,
            module_UI_ssize_sidebar("ssize_sidebar")
          ),
          box(
            title = "Analysis Panels", width = 8, status = "warning",
            solidHeader = TRUE, collapsible = FALSE,
            tabBox(
              width = 12, id = "ssize_tabs",
              tabPanel("Design", module_UI_ssize_design("ssize_design")),
              tabPanel("Scenarios", module_UI_ssize_scenarios("ssize_scenarios")),
              tabPanel("Sensitivity", module_UI_ssize_sensitivity("ssize_sensitivity")),
              tabPanel("Methods", module_UI_ssize_methods("ssize_methods")),
              tabPanel("Report", module_UI_ssize_report("ssize_report"))
            )
          )
        )
      ),
      tabItem(
        tabName = "methods",
        fluidRow(
          box(
            title = "Methods Guide", width = 12, status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            h3("Clinical Trial Data Standards & Sample Size Suite"),
            p("This page will document the data standardization workflow and endpoint-specific sample-size methods."),
            tags$ul(
              tags$li("Data Standardization: Profile -> Mapping -> Standards Build -> QC & Traceability -> Export."),
              tags$li("Sample Size: Design -> Scenarios -> Sensitivity -> Methods -> Report."),
              tags$li("References will include the TrialDesign help PDFs and public pharmaverse/PHUSE resources already staged under inst/.")
            )
          )
        )
      ),
      tabItem(
        tabName = "author",
        fluidRow(
          box(
            title = "Author", width = 12, status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            p("Project scaffold for a clinical data standardization and sample-size Shiny suite.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  stdz_rv <- initial_stdz_rv()
  ssize_rv <- initial_ssize_rv()

  module_server_stdz_sidebar("stdz_sidebar", stdz_rv)
  module_server_stdz_profile("stdz_profile", stdz_rv)
  module_server_stdz_mapping("stdz_mapping", stdz_rv)
  module_server_stdz_build("stdz_build", stdz_rv)
  module_server_stdz_qc("stdz_qc", stdz_rv)
  module_server_stdz_export("stdz_export", stdz_rv)

  module_server_ssize_sidebar("ssize_sidebar", ssize_rv, stdz_rv)
  module_server_ssize_design("ssize_design", ssize_rv)
  module_server_ssize_scenarios("ssize_scenarios", ssize_rv)
  module_server_ssize_sensitivity("ssize_sensitivity", ssize_rv)
  module_server_ssize_methods("ssize_methods", ssize_rv)
  module_server_ssize_report("ssize_report", ssize_rv)
}

shinyApp(ui, server)
