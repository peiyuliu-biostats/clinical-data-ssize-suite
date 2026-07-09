initial_stdz_rv <- function() {
  reactiveValues(
    mode = "example",
    study = list(
      study_id = "DEMO-001",
      phase = "Phase II",
      standard = "SDTM/ADaM MVP"
    ),
    data_source = list(
      mode = "example",
      example_key = "phuse_tdf_dm",
      label = "PHUSE TDF DM raw-like SAS dataset",
      path = "inst/example_data/raw/phuse_tdf_dm.sas7bdat",
      uploaded_path = NULL,
      uploaded_name = NULL
    ),
    profile = NULL,
    profile_error = NULL,
    profile_source_id = NULL,
    mapping = NULL,
    mapping_error = NULL,
    mapping_upload_error = NULL,
    mapping_source = NULL,
    mapping_validation = NULL,
    build = NULL,
    build_error = NULL,
    qc = NULL,
    qc_error = NULL,
    stage = list(
      profile = FALSE,
      mapping = FALSE,
      build = FALSE,
      qc = FALSE
    )
  )
}
