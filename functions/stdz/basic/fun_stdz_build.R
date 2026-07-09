# =====================================================================
# Standards Build (Raw -> SDTM/ADaM MVP)
# Blocking fixes applied:
#   (1) derive USUBJID (= STUDYID-SITEID-SUBJID) and --SEQ
#   (4) date parsing returns per-value imputation/parse flags
# =====================================================================

# ---- date parsing -------------------------------------------------

# Back-compatible: ISO string or NA (still used by stdz_clean_value).
stdz_parse_mixed_date <- function(x) {
  stdz_parse_mixed_date_detail(x)$iso
}

# Deterministic, format-by-format parse that records HOW each value was
# resolved. flag in {"missing","iso","reformatted","unparseable"}.
# Assumption (documented for reviewers): ambiguous numeric slash dates are
# read as US "%m/%d/%Y" because that format is tried before day-first.
stdz_parse_mixed_date_detail <- function(x) {
  raw <- trimws(as.character(x))
  raw[!nzchar(raw)] <- NA_character_
  n <- length(raw)
  iso <- rep(NA_character_, n)
  flag <- ifelse(is.na(raw), "missing", "unparseable")
  # Each format is gated by a regex that must match the WHOLE string before
  # parsing. R's strptime is otherwise lenient (it ignores trailing
  # characters), so "12/08/1950" under %Y/%m/%d would silently become year
  # 12. Anchoring removes that class of silent mis-parse; strings that match
  # a format's shape but are not valid calendar dates (e.g. "31/02/2013")
  # stay "unparseable" rather than being coerced.
  specs <- list(
    list(re = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",      fmt = "%Y-%m-%d", iso = TRUE),
    list(re = "^[0-9]{4}/[0-9]{2}/[0-9]{2}$",      fmt = "%Y/%m/%d", iso = FALSE),
    list(re = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$",  fmt = "%m/%d/%Y", iso = FALSE),
    list(re = "^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", fmt = "%d-%b-%Y", iso = FALSE),
    list(re = "^[0-9]{1,2}[A-Za-z]{3}[0-9]{4}$",   fmt = "%d%b%Y",   iso = FALSE),
    list(re = "^[0-9]{8}$",                         fmt = "%Y%m%d",   iso = FALSE)
  )
  for (s in specs) {
    todo <- which(flag == "unparseable" & !is.na(raw) & grepl(s$re, raw))
    if (length(todo) == 0) next
    d <- suppressWarnings(as.Date(raw[todo], format = s$fmt))
    ok <- !is.na(d)
    if (any(ok)) {
      iso[todo[ok]] <- format(d[ok], "%Y-%m-%d")
      flag[todo[ok]] <- if (isTRUE(s$iso)) "iso" else "reformatted"
    }
  }
  list(iso = iso, flag = flag)
}

stdz_is_date_target <- function(target_variable, cleaning_rule) {
  rule <- toupper(paste(cleaning_rule, collapse = " "))
  # Precise date signal: the target name looks like a date, or the rule
  # explicitly references dates. Bare "ISO" is NOT used, because non-date
  # rules legitimately mention ISO (e.g. "map to ISO 3166 country code"),
  # which previously mis-routed such columns into date parsing.
  grepl("DTC$|DT$|DATE", toupper(target_variable)) ||
    grepl("ISO ?8601|DATE|DTC|MIXED DATE", rule)
}

stdz_is_adam_dataset <- function(name) {
  grepl("^AD", toupper(name))
}

# Natural (content) keys for a domain: variables that identify a record
# independently of how it was derived. Deliberately EXCLUDES --SEQ, which is
# a derived, dataset-internal sequence and therefore not a valid key for
# comparing against an externally produced dataset.
stdz_natural_keys <- function(target, cols) {
  candidates <- c(
    paste0(target, "STDTC"), paste0(target, "DTC"),
    paste0(target, "TESTCD"), paste0(target, "TEST"),
    paste0(target, "TERM"), paste0(target, "TRT"),
    paste0(target, "SPID"),
    "PARAMCD", "PARAM", "VISIT", paste0(target, "TPT"), "ADT", "STARTDT"
  )
  intersect(candidates, cols)
}

# ---- value cleaning (unchanged behaviour) -------------------------

stdz_clean_value <- function(x, target_variable, cleaning_rule) {
  rule <- toupper(paste(cleaning_rule, collapse = " "))
  out <- x
  if (stdz_is_date_target(target_variable, cleaning_rule)) {
    return(stdz_parse_mixed_date(out))
  }
  if (grepl("YES/NO|YES NO", rule) || target_variable %in% c("DTHFL", "AESER", "RSACPTFL", "VSBLFL")) {
    y <- toupper(trimws(as.character(out)))
    y[y %in% c("YES", "Y", "TRUE", "1")] <- "Y"
    y[y %in% c("NO", "N", "FALSE", "0")] <- "N"
    y[!nzchar(y)] <- NA_character_
    return(y)
  }
  if (grepl("NUMERIC", rule) || target_variable %in% c("EXDOSE", "AVAL", "AGE")) {
    return(suppressWarnings(as.numeric(out)))
  }
  y <- trimws(as.character(out))
  y[!nzchar(y)] <- NA_character_
  if (grepl("CASE|TERMINOLOGY|NORMALIZE|UNIT|ROUTE|FLAG", rule)) y <- toupper(y)
  y
}

# ---- dataset build ------------------------------------------------

stdz_build_dataset <- function(raw_data, mapping, target_dataset, study_id) {
  rows <- mapping[mapping$target_dataset == target_dataset & nzchar(mapping$target_variable), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)

  out <- data.frame(.row_id = seq_len(nrow(raw_data)), stringsAsFactors = FALSE)
  date_flags <- list()
  for (i in seq_len(nrow(rows))) {
    source_variable <- rows$source_variable[[i]]
    target_variable <- rows$target_variable[[i]]
    if (!source_variable %in% names(raw_data)) next
    rule <- rows$cleaning_rule[[i]]
    if (stdz_is_date_target(target_variable, rule)) {
      detail <- stdz_parse_mixed_date_detail(raw_data[[source_variable]])
      out[[target_variable]] <- detail$iso
      date_flags[[target_variable]] <- detail$flag
    } else {
      out[[target_variable]] <- stdz_clean_value(raw_data[[source_variable]], target_variable, rule)
    }
  }
  out$.row_id <- NULL

  is_adam <- stdz_is_adam_dataset(target_dataset)

  # STUDYID for every layer (SDTM and ADaM need it as a merge key)
  if (!"STUDYID" %in% names(out)) out$STUDYID <- study_id
  # DOMAIN only for SDTM domains
  if (!is_adam && !"DOMAIN" %in% names(out)) out$DOMAIN <- target_dataset

  # (Fix 1) derive USUBJID = STUDYID-SITEID-SUBJID when a mapping supplied
  # SUBJID but not USUBJID (the DM case). Findings/interventions domains
  # already carry a mapped USUBJID and are left untouched.
  if (!"USUBJID" %in% names(out) && "SUBJID" %in% names(out)) {
    subj <- trimws(as.character(out$SUBJID))
    stud <- trimws(as.character(out$STUDYID))
    if ("SITEID" %in% names(out)) {
      site <- trimws(as.character(out$SITEID))
      usub <- ifelse(is.na(site) | !nzchar(site),
                     paste(stud, subj, sep = "-"),
                     paste(stud, site, subj, sep = "-"))
    } else {
      usub <- paste(stud, subj, sep = "-")
    }
    usub[is.na(subj) | !nzchar(subj)] <- NA_character_
    out$USUBJID <- usub
  }

  # (Fix 1) derive --SEQ for SDTM findings/events/interventions domains
  # (DM has one record per subject and no DMSEQ; ADaM excluded).
  # --SEQ is assigned AFTER ordering each subject's records by natural content
  # keys, so the same raw data always yields the same --SEQ regardless of the
  # order the records arrive in. Numbering by raw row position would make the
  # key non-reproducible, which is not acceptable for a submission artefact.
  seq_var <- paste0(target_dataset, "SEQ")
  seq_domains <- c("AE", "EX", "LB", "VS", "RS", "DS", "CM", "MH")
  if (!is_adam && target_dataset %in% seq_domains &&
      !seq_var %in% names(out) && "USUBJID" %in% names(out)) {
    nat <- stdz_natural_keys(target_dataset, names(out))
    ord_args <- c(
      list(as.character(out$USUBJID)),
      lapply(nat, function(v) out[[v]]),
      list(seq_len(nrow(out)))  # stable tiebreak
    )
    ord <- do.call(order, c(ord_args, list(na.last = TRUE)))
    out <- out[ord, , drop = FALSE]
    date_flags <- lapply(date_flags, function(f) f[ord])
    row.names(out) <- NULL
    grp <- as.character(out$USUBJID)
    grp[is.na(grp)] <- "__NA__"
    out[[seq_var]] <- as.integer(stats::ave(seq_len(nrow(out)), grp, FUN = seq_along))
  }

  # order key variables first
  key_order <- intersect(c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", seq_var), names(out))
  out <- out[, c(key_order, setdiff(names(out), key_order)), drop = FALSE]
  out <- out[, unique(names(out)), drop = FALSE]

  attr(out, "date_flags") <- date_flags
  out
}

stdz_build_tlf_ready <- function(datasets) {
  rows <- lapply(names(datasets), function(name) {
    data <- datasets[[name]]
    data.frame(
      dataset = name,
      rows = nrow(data),
      columns = ncol(data),
      subjects = if ("USUBJID" %in% names(data)) length(unique(stats::na.omit(data$USUBJID))) else if ("SUBJID" %in% names(data)) length(unique(stats::na.omit(data$SUBJID))) else NA_integer_,
      key_endpoint = if (name == "ADTTE" && "CNSR" %in% names(data)) paste0("events=", sum(data$CNSR == 0, na.rm = TRUE), "; censored=", sum(data$CNSR == 1, na.rm = TRUE)) else "",
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame(dataset = character(), rows = integer(), columns = integer(), subjects = integer(), key_endpoint = character()))
  }
  do.call(rbind, rows)
}

stdz_build_traceability <- function(mapping, datasets) {
  built <- do.call(rbind, lapply(names(datasets), function(name) {
    data.frame(target_dataset = name, target_variable = names(datasets[[name]]), stringsAsFactors = FALSE)
  }))
  trace <- merge(
    mapping,
    built,
    by = c("target_dataset", "target_variable"),
    all.y = TRUE,
    sort = FALSE
  )
  trace$status <- ifelse(
    nzchar(trace$source_variable), "mapped",
    ifelse(trace$target_variable %in% c("STUDYID", "DOMAIN"), "system-added", "derived")
  )
  trace[, c("source_table", "source_variable", "target_layer", "target_dataset", "target_variable", "cleaning_rule", "derivation_rule", "status")]
}

stdz_build_standards <- function(data_source, study, mapping) {
  if (is.null(data_source$path) || !nzchar(data_source$path)) {
    stop("No raw/source data path is available for Standards Build.", call. = FALSE)
  }
  mapping <- stdz_standardize_mapping_columns(mapping)
  validation <- stdz_validate_mapping(mapping, NULL)
  if (!isTRUE(validation$ok)) {
    stop(paste(validation$errors, collapse = " "), call. = FALSE)
  }
  raw_data <- stdz_read_table(data_source$path)
  missing_source <- setdiff(mapping$source_variable[nzchar(mapping$source_variable)], names(raw_data))
  if (length(missing_source) > 0) {
    stop(paste("Source variables missing from raw data:", paste(head(missing_source, 10), collapse = ", ")), call. = FALSE)
  }
  targets <- sort(unique(mapping$target_dataset[nzchar(mapping$target_dataset) & nzchar(mapping$target_variable)]))
  datasets <- list()
  date_flag_rows <- list()
  for (target in targets) {
    built <- stdz_build_dataset(raw_data, mapping, target, study$study_id)
    if (is.null(built)) next
    flags <- attr(built, "date_flags")
    if (!is.null(flags) && length(flags) > 0) {
      for (v in names(flags)) {
        f <- flags[[v]]
        date_flag_rows[[length(date_flag_rows) + 1]] <- data.frame(
          dataset = target,
          variable = v,
          total = length(f),
          iso = sum(f == "iso"),
          reformatted = sum(f == "reformatted"),
          unparseable = sum(f == "unparseable"),
          missing = sum(f == "missing"),
          stringsAsFactors = FALSE
        )
      }
    }
    attr(built, "date_flags") <- NULL
    datasets[[target]] <- built
  }
  if (length(datasets) == 0) {
    stop("No target datasets were generated from the current mapping.", call. = FALSE)
  }
  date_flags <- if (length(date_flag_rows) > 0) {
    do.call(rbind, date_flag_rows)
  } else {
    data.frame(dataset = character(), variable = character(), total = integer(),
               iso = integer(), reformatted = integer(), unparseable = integer(),
               missing = integer(), stringsAsFactors = FALSE)
  }
  list(
    datasets = datasets,
    tlf_ready = stdz_build_tlf_ready(datasets),
    traceability = stdz_build_traceability(mapping, datasets),
    date_flags = date_flags,
    summary = data.frame(
      metric = c("source", "built datasets", "mapping rows", "raw rows", "date variables checked"),
      value = c(data_source$label, paste(names(datasets), collapse = ", "), nrow(mapping), nrow(raw_data), nrow(date_flags)),
      stringsAsFactors = FALSE
    )
  )
}
