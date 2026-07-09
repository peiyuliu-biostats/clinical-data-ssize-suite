stdz_available_examples <- function() {
  curated <- data.frame(
    key = "phuse_tdf_dm",
    label = "PHUSE TDF DM raw-like SAS dataset",
    path = "inst/example_data/raw/phuse_tdf_dm.sas7bdat",
    source_standard_dataset = NA_character_,
    target_domain = "DM",
    rows = NA_integer_,
    columns = NA_integer_,
    mapping_key = NA_character_,
    expected_reference = NA_character_,
    metadata = NA_character_,
    role = "raw/source-like example",
    stringsAsFactors = FALSE
  )
  manifest_path <- "inst/example_data/raw/generated_raw_manifest.csv"
  generated <- if (file.exists(manifest_path)) {
    read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    curated[FALSE, , drop = FALSE]
  }
  needed <- setdiff(names(curated), names(generated))
  for (name in needed) generated[[name]] <- NA_character_
  examples <- rbind(curated, generated[, names(curated), drop = FALSE])
  examples <- examples[file.exists(examples$path), , drop = FALSE]
  row.names(examples) <- NULL
  examples
}

stdz_available_reference_datasets <- function() {
  files <- c(
    sdtm_dm = "inst/example_data/standard/sdtm_dm.csv",
    sdtm_ae = "inst/example_data/standard/sdtm_ae.csv",
    sdtm_ex = "inst/example_data/standard/sdtm_ex.csv",
    sdtm_lb = "inst/example_data/standard/sdtm_lb.csv",
    sdtm_vs = "inst/example_data/standard/sdtm_vs.csv",
    sdtm_rs_onco = "inst/example_data/standard/sdtm_rs_onco.csv",
    adam_adsl = "inst/example_data/standard/adam_adsl.csv",
    adam_adae = "inst/example_data/standard/adam_adae.csv",
    adam_adtte_onco = "inst/example_data/standard/adam_adtte_onco.csv",
    adam_adrs_onco = "inst/example_data/standard/adam_adrs_onco.csv"
  )
  labels <- c(
    sdtm_dm = "pharmaverse SDTM DM",
    sdtm_ae = "pharmaverse SDTM AE",
    sdtm_ex = "pharmaverse SDTM EX",
    sdtm_lb = "pharmaverse SDTM LB",
    sdtm_vs = "pharmaverse SDTM VS",
    sdtm_rs_onco = "pharmaverse SDTM RS oncology",
    adam_adsl = "pharmaverse ADaM ADSL",
    adam_adae = "pharmaverse ADaM ADAE",
    adam_adtte_onco = "pharmaverse ADaM ADTTE oncology",
    adam_adrs_onco = "pharmaverse ADaM ADRS oncology"
  )
  existing <- file.exists(files)
  keys <- names(files)[existing]
  data.frame(
    key = keys,
    label = unname(labels[existing]),
    path = unname(files[existing]),
    standard = ifelse(grepl("^sdtm", keys), "SDTM", "ADaM"),
    role = "standard reference, not raw input",
    stringsAsFactors = FALSE
  )
}

stdz_read_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- switch(
    ext,
    csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    txt = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    rds = readRDS(path),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read .xlsx files.", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    },
    sas7bdat = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read .sas7bdat files.", call. = FALSE)
      }
      as.data.frame(haven::read_sas(path), stringsAsFactors = FALSE)
    },
    xpt = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' is required to read .xpt files.", call. = FALSE)
      }
      as.data.frame(haven::read_xpt(path), stringsAsFactors = FALSE)
    },
    stop(sprintf("Unsupported file extension: .%s", ext), call. = FALSE)
  )
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

stdz_infer_domain <- function(name, vars) {
  nm <- toupper(name)
  vars_upper <- toupper(vars)
  if (grepl("ADSL", nm)) return("ADSL")
  if (grepl("ADAE", nm)) return("ADAE")
  if (grepl("ADTTE", nm)) return("ADTTE")
  if (grepl("ADRS", nm)) return("ADRS")
  domain_hits <- c("DM", "AE", "EX", "LB", "VS", "RS", "DS", "CM", "MH")
  for (domain in domain_hits) {
    if (grepl(paste0("(^|[ _/-])", domain, "($|[ _./-])"), nm)) return(domain)
    if (paste0(domain, "SEQ") %in% vars_upper) return(domain)
  }
  if (all(c("USUBJID", "STUDYID") %in% vars_upper)) return("Subject-level or SDTM-like")
  "Unknown"
}

stdz_is_date_candidate <- function(x, name) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(TRUE)
  if (!is.character(x)) return(grepl("DTC$|DATE|DT$", toupper(name)))
  vals <- stats::na.omit(x)
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(grepl("DTC$|DATE|DT$", toupper(name)))
  vals <- head(vals, 100)
  parsed <- tryCatch(
    suppressWarnings(as.Date(vals, tryFormats = c(
      "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d%b%Y", "%d-%b-%Y", "%Y%m%d"
    ))),
    error = function(e) rep(as.Date(NA), length(vals))
  )
  mean(!is.na(parsed)) >= 0.7 || grepl("DTC$|DATE|DT$", toupper(name))
}

stdz_profile_table <- function(data, source_name, source_path) {
  stopifnot(is.data.frame(data))
  rows <- nrow(data)
  cols <- ncol(data)
  miss <- vapply(data, function(x) sum(is.na(x) | (is.character(x) & !nzchar(x))), integer(1))
  type <- vapply(data, function(x) class(x)[1], character(1))
  unique_n <- vapply(data, function(x) length(unique(x)), integer(1))
  examples <- vapply(data, function(x) {
    vals <- unique(stats::na.omit(as.character(x)))
    vals <- vals[nzchar(vals)]
    paste(head(vals, 3), collapse = " | ")
  }, character(1))
  date_candidate <- vapply(seq_along(data), function(i) {
    stdz_is_date_candidate(data[[i]], names(data)[i])
  }, logical(1))
  duplicated_rows <- if (rows > 0) sum(duplicated(data)) else 0
  variable_profile <- data.frame(
    variable = names(data),
    type = type,
    missing_n = as.integer(miss),
    missing_pct = if (rows > 0) round(100 * miss / rows, 2) else 0,
    unique_n = as.integer(unique_n),
    date_candidate = ifelse(date_candidate, "yes", "no"),
    examples = examples,
    stringsAsFactors = FALSE
  )
  overview <- data.frame(
    metric = c("source", "path", "rows", "columns", "duplicated_rows", "missing_cells", "inferred_domain"),
    value = c(
      source_name,
      source_path,
      format(rows, big.mark = ","),
      format(cols, big.mark = ","),
      format(duplicated_rows, big.mark = ","),
      format(sum(miss), big.mark = ","),
      stdz_infer_domain(source_name, names(data))
    ),
    stringsAsFactors = FALSE
  )
  readiness <- data.frame(
    check = c("Readable input", "At least one row", "At least one column", "Subject identifier candidate", "Date-like variable candidate"),
    status = c(
      "pass",
      ifelse(rows > 0, "pass", "fail"),
      ifelse(cols > 0, "pass", "fail"),
      ifelse(any(toupper(names(data)) %in% c("USUBJID", "SUBJID", "SUBJECT_ID", "SUBJ_NO")), "pass", "warn"),
      ifelse(any(date_candidate), "pass", "warn")
    ),
    stringsAsFactors = FALSE
  )
  list(
    overview = overview,
    variables = variable_profile,
    readiness = readiness,
    preview = head(data, 10)
  )
}
