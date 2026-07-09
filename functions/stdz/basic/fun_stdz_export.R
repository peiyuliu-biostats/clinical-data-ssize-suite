stdz_export_manifest <- function(data_source, study, build, qc) {
  data.frame(
    item = c(
      "study_id", "phase", "target_standard", "source_label", "source_path",
      "built_datasets", "qc_status", "export_date"
    ),
    value = c(
      study$study_id,
      study$phase,
      study$standard,
      data_source$label,
      data_source$path,
      paste(names(build$datasets), collapse = ", "),
      qc$status,
      format(Sys.Date(), "%Y-%m-%d")
    ),
    stringsAsFactors = FALSE
  )
}

stdz_write_export_files <- function(root, data_source, study, mapping, build, qc) {
  dirs <- file.path(root, c("datasets", "specifications", "qc", "traceability", "tlf"))
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  for (name in names(build$datasets)) {
    write.csv(build$datasets[[name]], file.path(root, "datasets", paste0(tolower(name), ".csv")), row.names = FALSE, na = "")
  }
  write.csv(stdz_standardize_mapping_columns(mapping), file.path(root, "specifications", "mapping_current.csv"), row.names = FALSE, na = "")
  write.csv(qc$report, file.path(root, "qc", "qc_report.csv"), row.names = FALSE, na = "")
  write.csv(qc$checks, file.path(root, "qc", "rule_checks.csv"), row.names = FALSE, na = "")
  write.csv(qc$diff, file.path(root, "qc", "reference_diff.csv"), row.names = FALSE, na = "")
  write.csv(qc$traceability, file.path(root, "traceability", "traceability.csv"), row.names = FALSE, na = "")
  write.csv(build$tlf_ready, file.path(root, "tlf", "tlf_ready_summary.csv"), row.names = FALSE, na = "")
  write.csv(build$summary, file.path(root, "build_summary.csv"), row.names = FALSE, na = "")
  write.csv(qc$summary, file.path(root, "qc_summary.csv"), row.names = FALSE, na = "")
  write.csv(stdz_export_manifest(data_source, study, build, qc), file.path(root, "manifest.csv"), row.names = FALSE, na = "")
  list.files(root, recursive = TRUE, full.names = FALSE)
}

stdz_create_export_package <- function(file, data_source, study, mapping, build, qc) {
  if (is.null(build) || is.null(qc)) {
    stop("Build and QC outputs are required before export.", call. = FALSE)
  }
  root <- file.path(tempdir(), paste0("stdz_export_", as.integer(Sys.time())))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  files <- stdz_write_export_files(root, data_source, study, mapping, build, qc)
  stdz_zip_files(file, root, files)
  invisible(file)
}

stdz_zip_files <- function(file, root, files) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = file, files = files, root = root, mode = "mirror")
  } else {
    utils::zip(zipfile = file, files = files, flags = "-q")
  }
  if (!file.exists(file) || file.info(file)$size <= 0) {
    stop("Could not create ZIP export package in this R environment.", call. = FALSE)
  }
  invisible(file)
}
