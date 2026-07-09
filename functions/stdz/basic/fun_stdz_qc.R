# =====================================================================
# QC & Traceability
# Blocking fixes applied:
#   (1) USUBJID is now a required variable (no waiver); missing required
#       key variables are a hard FAIL, not a warning.
#   (3) reference difference is a KEY-BASED join (USUBJID[/--SEQ] or SUBJID),
#       not a positional row-by-row comparison.
#   (4) source date parse outcomes (incl. unparseable values) are surfaced
#       as explicit QC checks.
# =====================================================================

stdz_qc_expected_path <- function(data_source) {
  if (is.null(data_source$example_key) || !nzchar(data_source$example_key)) return(NA_character_)
  examples <- stdz_available_examples()
  row <- examples[examples$key == data_source$example_key, , drop = FALSE]
  if (nrow(row) != 1 || is.na(row$expected_reference) || !nzchar(row$expected_reference)) return(NA_character_)
  if (!file.exists(row$expected_reference)) return(NA_character_)
  row$expected_reference[[1]]
}

stdz_qc_add_check <- function(dataset, check, status, detail) {
  data.frame(
    dataset = dataset,
    check = check,
    status = status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

stdz_qc_required_check <- function(name, data) {
  dictionary <- stdz_target_dictionary()
  required <- unique(dictionary$target_variable[dictionary$target_dataset == name & dictionary$required == "yes"])
  # USUBJID is derived in Standards Build; it is NOT waived here.
  missing <- setdiff(required, names(data))
  if (length(missing) == 0) {
    stdz_qc_add_check(name, "required variables", "pass", "Required MVP variables (including USUBJID) are present.")
  } else {
    stdz_qc_add_check(name, "required variables", "fail", paste("Missing required variable(s):", paste(missing, collapse = ", ")))
  }
}

stdz_qc_dataset_checks <- function(name, data) {
  is_adam <- grepl("^AD", toupper(name))
  checks <- list()
  checks[[length(checks) + 1]] <- if (nrow(data) > 0) {
    stdz_qc_add_check(name, "row count", "pass", sprintf("%s rows.", format(nrow(data), big.mark = ",")))
  } else {
    stdz_qc_add_check(name, "row count", "fail", "Dataset has zero rows.")
  }
  checks[[length(checks) + 1]] <- stdz_qc_required_check(name, data)

  # STUDYID present for every layer
  checks[[length(checks) + 1]] <- if ("STUDYID" %in% names(data)) {
    stdz_qc_add_check(name, "STUDYID", "pass", "STUDYID is present.")
  } else {
    stdz_qc_add_check(name, "STUDYID", "fail", "STUDYID is missing.")
  }
  # DOMAIN only expected for SDTM domains
  if (!is_adam) {
    checks[[length(checks) + 1]] <- if ("DOMAIN" %in% names(data) && all(stats::na.omit(data$DOMAIN) == name)) {
      stdz_qc_add_check(name, "DOMAIN", "pass", "DOMAIN is present and consistent.")
    } else {
      stdz_qc_add_check(name, "DOMAIN", "fail", "DOMAIN is missing or inconsistent.")
    }
  }
  # USUBJID completeness (no missing keys)
  if ("USUBJID" %in% names(data)) {
    missing_key <- sum(is.na(data$USUBJID) | !nzchar(as.character(data$USUBJID)))
    checks[[length(checks) + 1]] <- if (missing_key == 0) {
      stdz_qc_add_check(name, "USUBJID completeness", "pass", "No missing USUBJID keys.")
    } else {
      stdz_qc_add_check(name, "USUBJID completeness", "fail", sprintf("%s records with missing USUBJID.", missing_key))
    }
  }

  duplicate_n <- if (nrow(data) > 0) sum(duplicated(data)) else 0
  checks[[length(checks) + 1]] <- if (duplicate_n == 0) {
    stdz_qc_add_check(name, "duplicate rows", "pass", "No duplicate rows detected.")
  } else {
    stdz_qc_add_check(name, "duplicate rows", "warn", sprintf("%s duplicate rows detected.", format(duplicate_n, big.mark = ",")))
  }

  date_vars <- names(data)[grepl("DTC$|DT$|DATE", names(data))]
  for (var in date_vars) {
    vals <- stats::na.omit(as.character(data[[var]]))
    bad <- vals[nzchar(vals) & is.na(suppressWarnings(as.Date(vals, format = "%Y-%m-%d")))]
    checks[[length(checks) + 1]] <- if (length(bad) == 0) {
      stdz_qc_add_check(name, paste("date format", var), "pass", "All non-missing values are ISO 8601 dates.")
    } else {
      stdz_qc_add_check(name, paste("date format", var), "warn", sprintf("%s non-ISO values detected.", length(bad)))
    }
  }
  yn_vars <- intersect(names(data), c("DTHFL", "AESER", "RSACPTFL", "VSBLFL"))
  for (var in yn_vars) {
    vals <- stats::na.omit(as.character(data[[var]]))
    bad <- vals[nzchar(vals) & !vals %in% c("Y", "N")]
    checks[[length(checks) + 1]] <- if (length(bad) == 0) {
      stdz_qc_add_check(name, paste("Y/N terminology", var), "pass", "Values are Y/N or missing.")
    } else {
      stdz_qc_add_check(name, paste("Y/N terminology", var), "warn", sprintf("Unexpected values: %s", paste(head(unique(bad), 5), collapse = ", ")))
    }
  }
  do.call(rbind, checks)
}

# (Fix 4) surface source-date parse outcomes recorded during Standards Build.
stdz_qc_date_parse_checks <- function(build) {
  df <- build$date_flags
  if (is.null(df) || nrow(df) == 0) return(NULL)
  do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, , drop = FALSE]
    status <- if (r$unparseable > 0) "warn" else "pass"
    stdz_qc_add_check(
      r$dataset,
      paste("date parse", r$variable),
      status,
      sprintf("iso=%d; reformatted=%d; unparseable=%d; missing=%d (of %d).",
              r$iso, r$reformatted, r$unparseable, r$missing, r$total)
    )
  }))
}

# (Fix 3) choose a shared key to align built vs expected records.
# Missing-value semantics for QC comparison: NA and "" both mean "no value".
stdz_cmp_norm <- function(x) {
  v <- trimws(as.character(x))
  v[!nzchar(v)] <- NA_character_
  v
}
stdz_cmp_equal <- function(a, b) {
  a <- stdz_cmp_norm(a); b <- stdz_cmp_norm(b)
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}
# Key normalization is deliberately MORE tolerant than value comparison:
# a join key must survive cosmetic differences (case, padding) so that records
# can be aligned at all, whereas value comparison stays case-sensitive so that
# genuine controlled-terminology defects are still reported.
stdz_key_norm <- function(x) {
  v <- toupper(trimws(as.character(x)))
  v[is.na(v)] <- ""
  v
}

# (Fix 3) choose a shared key to align built vs expected records.
# --SEQ is NEVER used: it is derived inside Standards Build, so it cannot be
# assumed to agree with an externally produced reference. Candidates are the
# full natural-key set, then each natural key on its own, then USUBJID, then
# SUBJID; we take the candidate that is unique in both frames and, subject to
# that, aligns the most records.
stdz_diff_keys <- function(built, expected, target) {
  shared <- intersect(names(built), names(expected))
  nat <- stdz_natural_keys(target, shared)
  candidates <- list()
  if (length(nat) > 1) candidates[[length(candidates) + 1]] <- c("USUBJID", nat)
  for (nk in nat) candidates[[length(candidates) + 1]] <- c("USUBJID", nk)
  candidates <- c(candidates, list("USUBJID", "SUBJID"))

  best <- character(0); best_score <- c(-1, -1)
  for (k in candidates) {
    if (!all(k %in% shared)) next
    bk <- do.call(paste, c(lapply(k, function(x) stdz_key_norm(built[[x]])), sep = "||"))
    ek <- do.call(paste, c(lapply(k, function(x) stdz_key_norm(expected[[x]])), sep = "||"))
    uniq <- as.integer(anyDuplicated(bk) == 0L && anyDuplicated(ek) == 0L)
    matched <- length(intersect(unique(bk), unique(ek)))
    score <- c(uniq, matched)
    if (score[1] > best_score[1] || (score[1] == best_score[1] && score[2] > best_score[2])) {
      best <- k; best_score <- score
    }
  }
  best
}

stdz_qc_reference_diff <- function(build, data_source) {
  expected_path <- stdz_qc_expected_path(data_source)
  if (is.na(expected_path)) {
    return(data.frame(
      comparison = "expected reference",
      status = "not available",
      detail = "No expected reference fixture is available for this source.",
      stringsAsFactors = FALSE
    ))
  }
  expected <- stdz_read_table(expected_path)
  target <- toupper(sub(".*_expected_", "", tools::file_path_sans_ext(basename(expected_path))))
  if (!target %in% names(build$datasets)) {
    return(data.frame(
      comparison = "expected reference",
      status = "warn",
      detail = sprintf("Expected reference target %s was not built.", target),
      stringsAsFactors = FALSE
    ))
  }
  built <- build$datasets[[target]]
  keys <- stdz_diff_keys(built, expected, target)
  if (length(keys) == 0) {
    return(data.frame(
      comparison = "expected reference",
      status = "warn",
      detail = "No shared key (USUBJID/--SEQ or SUBJID) is available to align built vs expected records.",
      stringsAsFactors = FALSE
    ))
  }

  bk <- do.call(paste, c(lapply(keys, function(k) stdz_key_norm(built[[k]])), sep = "||"))
  ek <- do.call(paste, c(lapply(keys, function(k) stdz_key_norm(expected[[k]])), sep = "||"))
  dup_b <- sum(duplicated(bk)); dup_e <- sum(duplicated(ek))
  keep_b <- !duplicated(bk)
  keep_e <- !duplicated(ek)
  built2 <- built[keep_b, , drop = FALSE]; bk2 <- bk[keep_b]
  exp2 <- expected[keep_e, , drop = FALSE]; ek2 <- ek[keep_e]

  common_keys <- intersect(bk2, ek2)
  built_only <- setdiff(bk2, ek2)
  expected_only <- setdiff(ek2, bk2)
  # Derived --SEQ is excluded from value comparison for the same reason it is
  # excluded from the key: it is generated here, not carried from the source.
  compare_cols <- setdiff(intersect(names(built2), names(exp2)),
                          c(keys, paste0(target, "SEQ")))

  if (length(common_keys) == 0 || length(compare_cols) == 0) {
    return(data.frame(
      comparison = c("key alignment", "comparable columns"),
      status = c(ifelse(length(common_keys) > 0, "pass", "warn"),
                 ifelse(length(compare_cols) > 0, "info", "warn")),
      detail = c(
        sprintf("key=%s; matched=%d; built-only=%d; expected-only=%d",
                paste(keys, collapse = "+"), length(common_keys), length(built_only), length(expected_only)),
        paste(compare_cols, collapse = ", ")
      ),
      stringsAsFactors = FALSE
    ))
  }

  bi <- match(common_keys, bk2)
  ei <- match(common_keys, ek2)
  col_rate <- vapply(compare_cols, function(cc) {
    mean(stdz_cmp_equal(built2[bi, cc], exp2[ei, cc]))
  }, numeric(1))
  total <- length(compare_cols) * length(common_keys)
  matches <- round(sum(col_rate) * length(common_keys))
  worst <- names(sort(col_rate))[seq_len(min(4, length(col_rate)))]
  worst_txt <- paste(sprintf("%s=%.0f%%", worst, 100 * col_rate[worst]), collapse = "; ")

  data.frame(
    comparison = c("key", "matched keys", "built-only keys", "expected-only keys",
                   "duplicate keys dropped", "comparable columns", "cell match rate",
                   "lowest-matching columns"),
    status = c("info", "info",
               ifelse(length(built_only) == 0, "pass", "warn"),
               ifelse(length(expected_only) == 0, "pass", "warn"),
               ifelse(dup_b + dup_e == 0, "pass", "warn"),
               "info",
               ifelse(total > 0 && matches / total >= 0.8, "pass", "warn"),
               "info"),
    detail = c(
      paste(keys, collapse = "+"),
      as.character(length(common_keys)),
      as.character(length(built_only)),
      as.character(length(expected_only)),
      sprintf("built=%d; expected=%d", dup_b, dup_e),
      paste(compare_cols, collapse = ", "),
      sprintf("%d/%d matched cells over key-aligned records (%.1f%%).", matches, total, 100 * matches / total),
      worst_txt
    ),
    stringsAsFactors = FALSE
  )
}

stdz_run_qc <- function(build, data_source) {
  if (is.null(build) || is.null(build$datasets)) {
    stop("Standards Build output is required before QC.", call. = FALSE)
  }
  checks <- do.call(rbind, lapply(names(build$datasets), function(name) {
    stdz_qc_dataset_checks(name, build$datasets[[name]])
  }))
  date_checks <- stdz_qc_date_parse_checks(build)
  if (!is.null(date_checks)) checks <- rbind(checks, date_checks)

  diff <- stdz_qc_reference_diff(build, data_source)
  status <- if (any(checks$status == "fail")) "fail" else if (any(checks$status == "warn") || any(diff$status == "warn")) "warn" else "pass"
  summary <- data.frame(
    metric = c("qc status", "datasets checked", "rule checks", "failures", "warnings", "reference comparison"),
    value = c(
      status,
      paste(names(build$datasets), collapse = ", "),
      nrow(checks),
      sum(checks$status == "fail"),
      sum(checks$status == "warn") + sum(diff$status == "warn"),
      diff$status[[1]]
    ),
    stringsAsFactors = FALSE
  )
  report <- rbind(
    data.frame(section = "rule_check", checks, stringsAsFactors = FALSE),
    data.frame(
      section = "reference_diff",
      dataset = "",
      check = diff$comparison,
      status = diff$status,
      detail = diff$detail,
      stringsAsFactors = FALSE
    )
  )
  list(
    status = status,
    summary = summary,
    checks = checks,
    diff = diff,
    traceability = build$traceability,
    report = report
  )
}
