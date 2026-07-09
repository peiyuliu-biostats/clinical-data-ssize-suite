stdz_target_dictionary <- function() {
  base <- data.frame(
    target_layer = c(
      rep("SDTM", 12), rep("SDTM", 11), rep("SDTM", 11), rep("SDTM", 12),
      rep("SDTM", 9), rep("SDTM", 8), rep("ADaM", 10)
    ),
    target_dataset = c(
      rep("DM", 12), rep("AE", 11), rep("EX", 11), rep("LB", 12),
      rep("VS", 9), rep("RS", 8), rep("ADTTE", 10)
    ),
    target_variable = c(
      "SUBJID", "SITEID", "RFICDTC", "BRTHDTC", "SEX", "RACE", "ETHNIC", "ARM", "ACTARM", "COUNTRY", "RFXSTDTC", "DTHFL",
      "USUBJID", "AESPID", "AETERM", "AEDECOD", "AESEV", "AESER", "AEREL", "AEACN", "AEOUT", "AESTDTC", "AEENDTC",
      "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU", "EXDOSFRM", "EXDOSFRQ", "EXROUTE", "VISIT", "EXSTDTC", "EXENDTC",
      "USUBJID", "LBSEQ", "LBDTC", "VISIT", "LBTESTCD", "LBTEST", "LBCAT", "LBORRES", "LBORRESU", "LBORNRLO", "LBORNRHI", "LBNRIND",
      "USUBJID", "VISIT", "VSDTC", "VSTEST", "VSPOS", "VSORRES", "VSORRESU", "VSBLFL", "VSTPT",
      "USUBJID", "VISIT", "RSDTC", "RSTEST", "RSEVAL", "RSORRES", "RSACPTFL", "RSREASND",
      "USUBJID", "PARAM", "EVNTDESC", "CNSR", "ADT", "STARTDT", "AVAL", "ARM", "AGE", "SEX"
    ),
    variable_label = c(
      "Subject Identifier for the Study", "Study Site Identifier", "Date/Time of Informed Consent", "Date/Time of Birth",
      "Sex", "Race", "Ethnicity", "Planned Arm", "Description of Actual Arm", "Country", "Date/Time of First Study Treatment", "Subject Death Flag",
      "Unique Subject Identifier", "Sponsor-Defined Identifier", "Reported Term for the Adverse Event", "Dictionary-Derived Term",
      "Severity/Intensity", "Serious Event", "Causality", "Action Taken with Study Treatment", "Outcome of Adverse Event", "Start Date/Time of Adverse Event", "End Date/Time of Adverse Event",
      "Unique Subject Identifier", "Sequence Number", "Name of Treatment", "Dose", "Dose Units", "Dose Form", "Dosing Frequency per Interval", "Route of Administration", "Visit Name", "Start Date/Time of Treatment", "End Date/Time of Treatment",
      "Unique Subject Identifier", "Sequence Number", "Date/Time of Specimen Collection", "Visit Name", "Lab Test Short Name", "Lab Test Name", "Category for Lab Test", "Result or Finding in Original Units", "Original Units", "Reference Range Lower Limit", "Reference Range Upper Limit", "Reference Range Indicator",
      "Unique Subject Identifier", "Visit Name", "Date/Time of Measurements", "Vital Signs Test Name", "Vital Signs Position", "Result or Finding in Original Units", "Original Units", "Baseline Flag", "Planned Time Point Name",
      "Unique Subject Identifier", "Visit Name", "Date/Time of Response Assessment", "Response Assessment Short Name", "Evaluator", "Result or Finding in Original Units", "Accepted Record Flag", "Reason Not Done",
      "Unique Subject Identifier", "Parameter", "Event Description", "Censor", "Analysis Date", "Time-to-Event Origin Date", "Analysis Value", "Planned Arm", "Age", "Sex"
    ),
    required = c(
      "yes", "no", "no", "no", "yes", "no", "no", "no", "no", "no", "no", "no",
      "yes", rep("no", 10),
      "yes", rep("no", 10),
      "yes", rep("no", 11),
      "yes", rep("no", 8),
      "yes", rep("no", 7),
      "yes", rep("no", 9)
    ),
    stringsAsFactors = FALSE
  )
  extra <- data.frame(
    target_layer = c("SDTM", "SDTM"),
    target_dataset = c("DM", "RS"),
    target_variable = c("USUBJID", "RSSTRESC"),
    variable_label = c("Unique Subject Identifier", "Character Result/Finding in Std Format"),
    required = c("yes", "no"),
    stringsAsFactors = FALSE
  )
  rbind(base, extra)
}

stdz_mapping_template_columns <- function() {
  c(
    "source_table", "source_variable", "target_layer", "target_dataset", "target_variable",
    "variable_label", "cleaning_rule", "derivation_rule", "required", "confidence", "recommendation_source", "notes"
  )
}

stdz_standardize_mapping_columns <- function(mapping) {
  aliases <- c(
    raw_dataset = "source_table",
    raw_variable = "source_variable",
    transformation = "cleaning_rule"
  )
  for (old in names(aliases)) {
    new <- aliases[[old]]
    if (old %in% names(mapping) && !new %in% names(mapping)) mapping[[new]] <- mapping[[old]]
  }
  expected <- stdz_mapping_template_columns()
  for (name in setdiff(expected, names(mapping))) mapping[[name]] <- ""
  mapping <- mapping[, expected, drop = FALSE]
  mapping[] <- lapply(mapping, as.character)
  mapping
}

stdz_read_mapping_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  mapping <- switch(
    ext,
    csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    txt = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read .xlsx mapping files.", call. = FALSE)
      }
      as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    },
    stop(sprintf("Unsupported mapping file extension: .%s", ext), call. = FALSE)
  )
  stdz_standardize_mapping_columns(mapping)
}

stdz_clean_name <- function(x) {
  gsub("[^A-Z0-9]", "", toupper(x))
}

stdz_source_table_name <- function(data_source) {
  if (!is.null(data_source$example_key) && nzchar(data_source$example_key)) return(data_source$example_key)
  if (!is.null(data_source$uploaded_name) && nzchar(data_source$uploaded_name)) {
    return(tools::file_path_sans_ext(data_source$uploaded_name))
  }
  "raw_source"
}

stdz_source_domain <- function(data_source, profile) {
  examples <- stdz_available_examples()
  if (!is.null(data_source$example_key) && data_source$example_key %in% examples$key) {
    domain <- examples$target_domain[match(data_source$example_key, examples$key)]
    if (!is.na(domain) && nzchar(domain)) return(domain)
  }
  if (!is.null(profile$overview)) {
    hit <- profile$overview$value[profile$overview$metric == "inferred_domain"]
    if (length(hit) == 1 && nzchar(hit) && hit %in% unique(stdz_target_dictionary()$target_dataset)) return(hit)
  }
  "DM"
}

stdz_load_example_mapping_key <- function(data_source) {
  if (is.null(data_source$example_key) || !nzchar(data_source$example_key)) return(NULL)
  examples <- stdz_available_examples()
  row <- examples[examples$key == data_source$example_key, , drop = FALSE]
  if (nrow(row) != 1 || is.na(row$mapping_key) || !nzchar(row$mapping_key) || !file.exists(row$mapping_key)) return(NULL)
  stdz_read_mapping_file(row$mapping_key)
}

stdz_match_target <- function(source_variable, dictionary) {
  src <- stdz_clean_name(source_variable)
  target_clean <- stdz_clean_name(dictionary$target_variable)
  label_clean <- stdz_clean_name(dictionary$variable_label)
  exact <- which(src == target_clean)
  if (length(exact) > 0) {
    return(list(index = exact[[1]], confidence = "high", source = "exact target variable match"))
  }
  target_in_source <- vapply(target_clean, function(pattern) grepl(pattern, src, fixed = TRUE), logical(1))
  contains <- which(grepl(src, target_clean, fixed = TRUE) | target_in_source)
  if (length(contains) > 0 && nchar(src) >= 3) {
    return(list(index = contains[[1]], confidence = "medium", source = "name containment match"))
  }
  label_in_source <- vapply(label_clean, function(pattern) grepl(pattern, src, fixed = TRUE), logical(1))
  label_contains <- which(grepl(src, label_clean, fixed = TRUE) | label_in_source)
  if (length(label_contains) > 0 && nchar(src) >= 4) {
    return(list(index = label_contains[[1]], confidence = "medium", source = "label containment match"))
  }
  distances <- utils::adist(src, target_clean)
  best <- which.min(distances)
  threshold <- max(2, ceiling(nchar(src) * 0.35))
  if (length(best) == 1 && distances[[best]] <= threshold) {
    return(list(index = best, confidence = "low", source = "approximate name match"))
  }
  list(index = NA_integer_, confidence = "unmapped", source = "no reliable match")
}

stdz_cleaning_rule_for_variable <- function(variable_profile, target_variable) {
  rules <- character(0)
  if (identical(variable_profile$date_candidate, "yes") || grepl("DTC$|DT$|DATE", target_variable)) {
    rules <- c(rules, "parse mixed date to ISO 8601")
  }
  if (variable_profile$type %in% c("integer", "numeric")) {
    rules <- c(rules, "numeric cast and range review")
  }
  if (variable_profile$missing_n > 0) {
    rules <- c(rules, "review missing values")
  }
  if (length(rules) == 0) rules <- "trim whitespace and normalize case/terminology when applicable"
  paste(rules, collapse = "; ")
}

stdz_recommend_mapping <- function(profile, data_source) {
  req_cols <- c("variable", "type", "missing_n", "date_candidate")
  if (is.null(profile$variables) || !all(req_cols %in% names(profile$variables))) {
    stop("Profile variables are not available for mapping recommendation.", call. = FALSE)
  }
  key <- stdz_load_example_mapping_key(data_source)
  if (!is.null(key)) {
    key$confidence[key$confidence == ""] <- "reference"
    key$recommendation_source[key$recommendation_source == ""] <- "generated mapping key"
    return(key)
  }
  source_domain <- stdz_source_domain(data_source, profile)
  dictionary <- stdz_target_dictionary()
  dictionary <- dictionary[dictionary$target_dataset == source_domain, , drop = FALSE]
  source_table <- stdz_source_table_name(data_source)
  rows <- lapply(seq_len(nrow(profile$variables)), function(i) {
    variable_profile <- profile$variables[i, , drop = FALSE]
    hit <- stdz_match_target(variable_profile$variable, dictionary)
    if (is.na(hit$index)) {
      target <- data.frame(
        target_layer = "",
        target_dataset = source_domain,
        target_variable = "",
        variable_label = "",
        required = "no",
        stringsAsFactors = FALSE
      )
    } else {
      target <- dictionary[hit$index, , drop = FALSE]
    }
    data.frame(
      source_table = source_table,
      source_variable = variable_profile$variable,
      target_layer = target$target_layer,
      target_dataset = target$target_dataset,
      target_variable = target$target_variable,
      variable_label = target$variable_label,
      cleaning_rule = stdz_cleaning_rule_for_variable(variable_profile, target$target_variable),
      derivation_rule = ifelse(nzchar(target$target_variable), "copy after cleaning unless derivation is specified", ""),
      required = target$required,
      confidence = hit$confidence,
      recommendation_source = hit$source,
      notes = "",
      stringsAsFactors = FALSE
    )
  })
  stdz_standardize_mapping_columns(do.call(rbind, rows))
}

stdz_validate_mapping <- function(mapping, profile = NULL) {
  mapping <- stdz_standardize_mapping_columns(mapping)
  errors <- character(0)
  if (nrow(mapping) == 0) errors <- c(errors, "Mapping file contains no rows.")
  required <- c("source_variable", "target_dataset", "target_variable")
  missing_required <- required[vapply(mapping[required], function(x) all(!nzchar(x)), logical(1))]
  if (length(missing_required) > 0) {
    errors <- c(errors, paste("Required mapping content missing:", paste(missing_required, collapse = ", ")))
  }
  if (!is.null(profile$variables)) {
    unknown_source <- setdiff(mapping$source_variable[nzchar(mapping$source_variable)], profile$variables$variable)
    if (length(unknown_source) > 0) {
      errors <- c(errors, paste("Source variables not found in current profile:", paste(head(unknown_source, 8), collapse = ", ")))
    }
  }
  target_dict <- stdz_target_dictionary()
  target_key <- paste(target_dict$target_dataset, target_dict$target_variable, sep = ".")
  mapped <- mapping[nzchar(mapping$target_dataset) & nzchar(mapping$target_variable), , drop = FALSE]
  unknown_target <- setdiff(paste(mapped$target_dataset, mapped$target_variable, sep = "."), target_key)
  if (length(unknown_target) > 0) {
    errors <- c(errors, paste("Target variables not in local target dictionary:", paste(head(unknown_target, 8), collapse = ", ")))
  }
  list(
    ok = length(errors) == 0,
    errors = errors,
    warnings = character(0),
    mapping = mapping
  )
}

stdz_mapping_summary <- function(mapping) {
  if (is.null(mapping) || nrow(mapping) == 0) {
    return(data.frame(metric = "mapped rows", value = "0", stringsAsFactors = FALSE))
  }
  data.frame(
    metric = c("mapped rows", "source variables", "target datasets", "high/reference confidence", "unmapped rows"),
    value = c(
      format(nrow(mapping), big.mark = ","),
      format(length(unique(mapping$source_variable[nzchar(mapping$source_variable)])), big.mark = ","),
      paste(sort(unique(mapping$target_dataset[nzchar(mapping$target_dataset)])), collapse = ", "),
      format(sum(mapping$confidence %in% c("high", "reference")), big.mark = ","),
      format(sum(!nzchar(mapping$target_variable) | mapping$confidence == "unmapped"), big.mark = ",")
    ),
    stringsAsFactors = FALSE
  )
}
