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
      menuItem("GitHub", icon = icon("github"),
               href = "https://github.com/peiyuliu-biostats/clinical-data-ssize-suite",
               newtab = TRUE),
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
            p(class = "suite-muted",
              "This guide describes the two connected tasks in the suite and states the exact ",
              "statistical formulas used by the sample-size engine. The text is written to be ",
              "self-contained, so that a reviewer can reproduce every number without reading the source code."),

            h4("1. Scope and general principles"),
            p("The suite supports two tasks. The first is the standardization of raw clinical data into ",
              "SDTM/ADaM-like datasets. The second is the calculation of the required sample size (or number ",
              "of events) for the primary endpoint of a study. The two tasks are connected: the assumptions ",
              "of the sample-size calculation can be estimated directly from standardized ADaM-like data, so ",
              "that the same design engine is used for manual and data-driven inputs."),
            tags$ul(
              tags$li("Data Standardization workflow: Profile → Mapping → Standards Build → QC & Traceability → Export."),
              tags$li("Sample Size workflow: Design → Scenarios → Sensitivity → Methods → Report."),
              tags$li("All calculators are deterministic. They use closed-form expressions, or an exact search ",
                      "over integer sample sizes, and never rely on simulation."),
              tags$li("Every calculation returns the same result structure (endpoint, design type, objective, ",
                      "required N or events, achieved power, and a table of traceable assumptions).")
            ),

            h4("2. Notation"),
            p("Let ", tags$i("Φ"), " denote the standard normal cumulative distribution function and ",
              tags$i("Φ"), tags$sup("−1"), " its quantile function. For significance level ",
              tags$i("α"), " and target power ", tags$i("1 − β"), ":"),
            tags$ul(
              tags$li(HTML("<i>a</i> = <i>&alpha;</i> for a one-sided test, and <i>a</i> = <i>&alpha;</i>/2 for a two-sided test;")),
              tags$li(HTML("<i>z<sub>a</sub></i> = <i>&Phi;</i><sup>&minus;1</sup>(1 &minus; <i>a</i>);")),
              tags$li(HTML("<i>z<sub>&beta;</sub></i> = <i>&Phi;</i><sup>&minus;1</sup>(power)."))
            ),
            p(HTML("The allocation ratio is <i>k</i> = <i>n<sub>t</sub></i> / <i>n<sub>c</sub></i> (treatment to control). ",
                   "Every raw sample size is first inflated for dropout as <i>n</i> / (1 &minus; <i>d</i>), where ",
                   "<i>d</i> is the dropout proportion, and then rounded up to the next integer. Required events for ",
                   "survival endpoints are also rounded up.")),

            h4("3. Effect measure and test objective"),
            p(HTML("Let <i>&Delta;</i> be the raw difference on the natural scale of the endpoint ",
                   "(for example <i>p</i><sub>1</sub> &minus; <i>p</i><sub>0</sub>, <i>p<sub>t</sub></i> &minus; <i>p<sub>c</sub></i>, ",
                   "or <i>&mu;<sub>t</sub></i> &minus; <i>&mu;<sub>c</sub></i>) and let <i>m</i> be the margin. ",
                   "The effect actually tested, <i>&delta;</i>, is defined by the objective:")),
            tags$ul(
              tags$li(HTML("equality: <i>&delta;</i> = <i>&Delta;</i>;")),
              tags$li(HTML("superiority: <i>&delta;</i> = <i>&Delta;</i> &minus; <i>m</i>;")),
              tags$li(HTML("non-inferiority: <i>&delta;</i> = <i>&Delta;</i> + <i>m</i>;")),
              tags$li(HTML("equivalence: <i>&delta;</i> = <i>m</i> &minus; |<i>&Delta;</i>|."))
            ),
            p(class = "suite-muted",
              "For time-to-event endpoints only the equality objective is implemented in the current engine."),

            h4("4. Data Standardization"),
            p(tags$b("4.1 Profile. "),
              "The selected raw table is read and summarized. For each variable the profile reports the data ",
              "type, the count and percentage of missing values, the number of unique values, example values, ",
              "and whether the variable is a date candidate. A value is treated as a date candidate when at ",
              "least 70% of a sample of non-missing values parse under a fixed list of common formats, or when ",
              "the variable name matches a date pattern. The profile also infers the likely domain and reports ",
              "readiness checks (readable input, at least one row and column, a subject-identifier candidate, ",
              "and a date-like variable)."),
            p(tags$b("4.2 Mapping. "),
              "Each source variable is mapped to a target SDTM or ADaM variable taken from a local target ",
              "dictionary. When no verified mapping key is available, a recommendation is produced by trying, ",
              "in order: (i) exact match of the cleaned variable name, (ii) name containment, (iii) label ",
              "containment, and (iv) an approximate match by Levenshtein edit distance with a length-dependent ",
              "threshold. Each recommendation carries a confidence label (high, medium, low, or unmapped). ",
              "A cleaning rule (for example date parsing, numeric cast, or terminology normalization) is ",
              "attached to each mapped variable."),
            p(tags$b("4.3 Standards Build. "),
              "The mapping is applied to the raw data to build one or more target datasets. Date values are ",
              "parsed to ISO 8601 (YYYY-MM-DD) by a format-by-format procedure that anchors each candidate ",
              "format to the whole string, so that partial or invalid dates remain flagged rather than being ",
              "silently coerced. Each parsed value is recorded as one of: iso, reformatted, unparseable, or ",
              "missing. Yes/No fields are normalized to Y/N and numeric fields are cast to numbers. The unique ",
              "subject identifier is derived, when not supplied, as USUBJID = STUDYID-SITEID-SUBJID. The ",
              "sequence number --SEQ is assigned within each subject after ordering records by their natural ",
              "content keys, so that the same raw data always produces the same --SEQ."),
            p(tags$b("4.4 QC & Traceability. "),
              "The built datasets are checked against a set of rules: presence of required variables ",
              "(USUBJID is mandatory and its absence is a hard failure), presence of STUDYID, presence and ",
              "consistency of DOMAIN for SDTM domains, completeness of USUBJID, duplicate rows, ISO date ",
              "format, and Y/N terminology. Date-parse outcomes from the build step are surfaced as explicit ",
              "checks. When an expected reference dataset exists, the built and expected datasets are compared ",
              "by a key-based join (natural keys, then USUBJID, then SUBJID), never by row position; the ",
              "derived --SEQ is excluded from both the key and the value comparison. The comparison reports the ",
              "matched and unmatched keys and the cell-level match rate."),
            p(tags$b("4.5 Export. "),
              "The datasets, the current mapping specification, the QC report, the traceability table, the ",
              "TLF-ready summary, and a manifest are written to a single ZIP package."),

            h4("5. Sample size — binary endpoint"),
            p(tags$b("5.1 One-sample proportion, normal approximation. "),
              "The required sample size is"),
            div(class = "suite-mono",
                HTML("n = [ <i>z<sub>a</sub></i> &middot; &radic;(<i>p</i><sub>0</sub>(1 &minus; <i>p</i><sub>0</sub>)) + ",
                     "<i>z<sub>&beta;</sub></i> &middot; &radic;(<i>p</i><sub>1</sub>(1 &minus; <i>p</i><sub>1</sub>)) ]<sup>2</sup> / ",
                     "(<i>p</i><sub>1</sub> &minus; <i>p</i><sub>0</sub>)<sup>2</sup>")),
            p(tags$b("5.2 One-sample proportion, exact binomial. "),
              HTML("The required <i>n</i> is the smallest integer for which the exact binomial test attains the ",
                   "target power under <i>p</i><sub>1</sub>. Because exact binomial power is not monotone in ",
                   "<i>n</i>, the search additionally requires that the next <i>w</i> sizes (confirmation window, ",
                   "default <i>w</i> = 1) also keep power &ge; target. The rejection region is one-sided upper ",
                   "when <i>p</i><sub>1</sub> &gt; <i>p</i><sub>0</sub> and one-sided lower otherwise, with the ",
                   "critical count taken from the binomial quantile at level <i>a</i>.")),
            p(tags$b("5.3 Two-sample proportions, normal approximation. "),
              "The control-arm size is solved first, and the treatment arm follows the allocation ratio:"),
            div(class = "suite-mono",
                HTML("<i>n<sub>c</sub></i> = (<i>z<sub>a</sub></i> + <i>z<sub>&beta;</sub></i>)<sup>2</sup> &middot; ",
                     "[ <i>p<sub>c</sub></i>(1 &minus; <i>p<sub>c</sub></i>) + <i>p<sub>t</sub></i>(1 &minus; <i>p<sub>t</sub></i>) / <i>k</i> ] / ",
                     "(<i>p<sub>t</sub></i> &minus; <i>p<sub>c</sub></i>)<sup>2</sup> ,&nbsp;&nbsp; <i>n<sub>t</sub></i> = <i>k</i> &middot; <i>n<sub>c</sub></i>")),

            h4("6. Sample size — continuous endpoint"),
            p(HTML("The required sample size is obtained by solving the exact power equation of the t-test under ",
                   "the noncentral t distribution. Power at <i>n</i> equals P(<i>T</i> &gt; <i>t</i><sub>crit</sub>), ",
                   "where <i>T</i> follows a noncentral t with degrees of freedom df and noncentrality <i>&lambda;</i>, ",
                   "and <i>t</i><sub>crit</sub> = <i>t</i><sub>df, 1&minus;a</sub> is the upper critical value:")),
            tags$ul(
              tags$li(HTML("one-sample and paired: df = <i>n</i> &minus; 1, <i>&lambda;</i> = <i>d</i> &middot; &radic;<i>n</i>, ",
                           "with standardized effect <i>d</i> = |<i>&delta;</i>| / <i>&sigma;</i><sub>eff</sub>;")),
              tags$li(HTML("paired design: <i>&sigma;</i><sub>eff</sub> = <i>&sigma;</i> &middot; &radic;(2(1 &minus; <i>&rho;</i>)), ",
                           "where <i>&rho;</i> is the correlation between paired measurements;")),
              tags$li(HTML("two-sample: df = <i>n<sub>c</sub></i> + <i>n<sub>t</sub></i> &minus; 2, ",
                           "<i>&lambda;</i> = (|<i>&delta;</i>| / <i>&sigma;</i>) &middot; ",
                           "&radic;( <i>n<sub>c</sub></i> <i>n<sub>t</sub></i> / (<i>n<sub>c</sub></i> + <i>n<sub>t</sub></i>) ), ",
                           "with <i>n<sub>t</sub></i> = <i>k</i> &middot; <i>n<sub>c</sub></i>."))
            ),
            p("The smallest integer sample size that reaches the target power is returned."),

            h4("7. Sample size — time-to-event endpoint"),
            p(tags$b("7.1 One-sample (versus a fixed reference). "),
              "The required number of events follows the Schoenfeld expression"),
            div(class = "suite-mono",
                HTML("<i>d</i> = (<i>z<sub>a</sub></i> + <i>z<sub>&beta;</sub></i>)<sup>2</sup> / (ln HR)<sup>2</sup>")),
            p(HTML("If a reference median <i>M</i><sub>ref</sub> and a new median <i>M</i><sub>new</sub> are given ",
                   "instead of a hazard ratio, the hazard ratio is derived under an exponential model as ",
                   "HR = <i>M</i><sub>ref</sub> / <i>M</i><sub>new</sub> (equivalently, hazard = ln 2 / median).")),
            p(tags$b("7.2 Two-sample (log-rank). "),
              "With allocation ratio k, the required number of events is"),
            div(class = "suite-mono",
                HTML("<i>d</i> = (<i>z<sub>a</sub></i> + <i>z<sub>&beta;</sub></i>)<sup>2</sup> &middot; (1 + <i>k</i>)<sup>2</sup> / ",
                     "( <i>k</i> &middot; (ln HR)<sup>2</sup> )")),
            p(HTML("The number of events is translated into a total sample size through the event probability: ",
                   "N = <i>d</i> / P(event). P(event) is either supplied directly, or derived under an exponential ",
                   "survival model with uniform accrual. In the derived case, with survival distribution ",
                   "S(<i>t</i>) = 1 &minus; e<sup>&minus;<i>&lambda;t</i></sup>, additional follow-up <i>f</i>, and ",
                   "accrual duration <i>a</i>, the average event probability of an arm with hazard <i>&lambda;</i> is ",
                   "approximated by Simpson's rule over the interval [<i>f</i>, <i>a</i> + <i>f</i>]:")),
            div(class = "suite-mono",
                HTML("P<sub>arm</sub> = [ S(<i>f</i>) + 4 &middot; S(<i>f</i> + <i>a</i>/2) + S(<i>a</i> + <i>f</i>) ] / 6 ,&nbsp;&nbsp; ",
                     "P(event) = ( P<sub>c</sub> + <i>k</i> &middot; P<sub>t</sub> ) / (1 + <i>k</i>)")),

            h4("8. Estimating assumptions from ADaM-like data"),
            p("Instead of typing the design parameters, the user may estimate them from example, built, or ",
              "uploaded ADaM-like data. The estimated parameters populate the same design engine described above."),
            tags$ul(
              tags$li(HTML("Binary (ADRS): the response rate of each arm is estimated as responders / <i>n</i>, ",
                           "where a record is a responder when AVALC belongs to the chosen response set for the ",
                           "selected PARAMCD.")),
              tags$li(HTML("Continuous (ADSL): the mean and standard deviation of each arm are estimated, and the ",
                           "pooled standard deviation is <i>s<sub>p</sub></i> = ",
                           "&radic;( ((<i>n<sub>c</sub></i>&minus;1)<i>s<sub>c</sub></i><sup>2</sup> + ",
                           "(<i>n<sub>t</sub></i>&minus;1)<i>s<sub>t</sub></i><sup>2</sup>) / ",
                           "(<i>n<sub>c</sub></i> + <i>n<sub>t</sub></i> &minus; 2) ). The allocation ratio is set to ",
                           "<i>n<sub>t</sub></i> / <i>n<sub>c</sub></i>.")),
              tags$li(HTML("Time-to-event (ADTTE): a record is an event when CNSR = 0. The exponential hazard of ",
                           "each arm is estimated with a 0.5 continuity correction as ",
                           "<i>&lambda;</i> = (events + 0.5) / (total exposure time), the median as ln 2 / <i>&lambda;</i>, ",
                           "the hazard ratio as <i>&lambda;<sub>t</sub></i> / <i>&lambda;<sub>c</sub></i>, and the ",
                           "event probability as total events / total subjects."))
            ),

            h4("9. Scenarios, sensitivity, and reporting"),
            tags$ul(
              tags$li("Scenarios: a table of parameter sets (uploaded, or generated as base / optimistic / ",
                      "conservative from the current design) is evaluated row by row with the same engine."),
              tags$li("Sensitivity: one design parameter is varied over a user-supplied grid while all other ",
                      "parameters are held at the current design, and the required N or events is recomputed at ",
                      "each grid point."),
              tags$li("Report: the design, the primary result, the method label and formula, and the full table ",
                      "of traceable assumptions are assembled into a protocol-ready summary that can be exported.")
            ),

            h4("10. Assumptions and limitations"),
            p("Each calculation is valid only under its stated model: the normal approximation for proportions ",
              "(unless the exact binomial method is selected), the noncentral t distribution for means, and an ",
              "exponential survival model with proportional hazards for time-to-event endpoints. The two-sample ",
              "survival sample size depends on the assumed event-probability model. The engine does not model ",
              "interim analyses, group-sequential boundaries, stratification, competing risks, clustering, or ",
              "covariate adjustment. Results are intended to support design decisions and should be confirmed ",
              "with an independent method before use in a study protocol."),

            h4("11. References"),
            tags$ul(
              tags$li("Schoenfeld DA. Sample-size formula for the proportional-hazards regression model. ",
                      "Biometrics. 1983;39(2):499–503."),
              tags$li("Chow SC, Shao J, Wang H, Lokhnygina Y. Sample Size Calculations in Clinical Research. ",
                      "3rd ed. Chapman and Hall/CRC; 2018."),
              tags$li("Julious SA. Sample Sizes for Clinical Trials. Chapman and Hall/CRC; 2010."),
              tags$li("CDISC. Study Data Tabulation Model (SDTM) and Analysis Data Model (ADaM) Implementation Guides. ",
                      "https://www.cdisc.org."),
              tags$li("pharmaverse example SDTM/ADaM data and PHUSE Test Data Factory resources ",
                      "(staged under inst/ in this repository).")
            ),

            tags$hr(),
            h4("Author"),
            p(tags$b("Peiyu Liu"), tags$br(),
              "Department of Biostatistics, University of Florida", tags$br(),
              "Contact: ", tags$a(href = "mailto:peiyu.liu.stats@gmail.com", "peiyu.liu.stats@gmail.com"), tags$br(),
              "Source code: ",
              tags$a(href = "https://github.com/peiyuliu-biostats/clinical-data-ssize-suite",
                     target = "_blank", "github.com/peiyuliu-biostats/clinical-data-ssize-suite")),
            p(class = "suite-muted", "Questions and suggestions are welcome.")
          )
        )
      ),
      tabItem(
        tabName = "author",
        fluidRow(
          box(
            title = "Author", width = 12, status = "info",
            solidHeader = TRUE, collapsible = FALSE,
            h3("Peiyu Liu"),
            p("Department of Biostatistics", tags$br(),
              "University of Florida"),
            p(tags$b("Contact: "),
              tags$a(href = "mailto:peiyu.liu.stats@gmail.com", "peiyu.liu.stats@gmail.com")),
            p(tags$b("Source code: "),
              tags$a(href = "https://github.com/peiyuliu-biostats/clinical-data-ssize-suite",
                     target = "_blank", "github.com/peiyuliu-biostats/clinical-data-ssize-suite")),
            p(class = "suite-muted",
              "An R Shiny suite for clinical data standardization, ADaM-driven assumption estimation, ",
              "and endpoint-specific sample-size calculation. Questions and suggestions are welcome.")
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
