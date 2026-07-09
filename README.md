# Clinical Trial Data Standards & Sample Size Suite

This Shiny project is the first scaffold for a two-dashboard clinical trial tool:

- Data Standardization: raw/source-like data profiling, mapping, standards build, QC/traceability, and export.
- Sample Size: endpoint-based sample-size design, scenario comparison, sensitivity analysis, methods, and reporting.

The UI follows the professional two-column pattern used in the early-phase adaptive dose optimization app: left-side settings and right-side analysis panels.

## Step 1 Local Validation

Open `clinical-data-ssize-suite.Rproj` in RStudio, then run:

```r
shiny::runApp()
```

Expected result:

- Pastel-purple header.
- White left navigation.
- `Data Standardization` and `Sample Size` dashboard pages.
- Each dashboard has a grey `Settings` panel on the left and a white `Analysis Panels` tab box on the right.

## Step 2 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Data Standardization`:

1. Keep `Data source = Load example dataset`.
2. Confirm the staged example selector only lists raw/source-like examples, not pharmaverse SDTM/ADaM standard datasets.
3. Confirm the selector includes the PHUSE raw-like dataset plus generated messy raw examples for DM, AE, EX, LB, VS, RS, and ADTTE.
4. Select `PHUSE TDF DM raw-like SAS dataset`.
5. Confirm `Profile` shows:
   - Overview table with 100 rows and 6 columns.
   - Inferred domain `DM`.
   - Variable profile for the source-like SAS variables.
6. Select `Generated messy raw adverse events from pharmaverse SDTM AE`.
7. Confirm `Profile` shows a CSV source with 1,196 rows and 11 columns.
8. Confirm the sidebar note reports available SDTM/ADaM standard reference datasets for Standards Build/QC and sample-size estimation.
9. Switch to `Mapping`; it should be unlocked after a successful Profile.
10. `Standards Build`, `QC & Traceability`, and `Export` should remain locked until later stages are implemented.

To regenerate the messy raw examples, run:

```r
source("scripts/generate_messy_raw_examples.R")
```

The script writes generated raw CSV files to `inst/example_data/raw/`, with mapping keys, expected standard references, and metadata under `inst/example_data/raw/metadata/`.

The pharmaverse SDTM/ADaM files are retained as standard reference datasets. They should not appear directly in the raw/source staged example selector.

Expected raw/source-like staged examples:

  - `inst/example_data/raw/phuse_tdf_dm.sas7bdat`
  - `inst/example_data/raw/raw_dm_demographics.csv`
  - `inst/example_data/raw/raw_ae_crf.csv`
  - `inst/example_data/raw/raw_ex_dosing_log.csv`
  - `inst/example_data/raw/raw_lb_vendor.csv`
  - `inst/example_data/raw/raw_vs_clinic.csv`
  - `inst/example_data/raw/raw_rs_oncology.csv`
  - `inst/example_data/raw/raw_tte_events.csv`

Expected reference-only files include:

  - `inst/example_data/standard/sdtm_dm.csv`
  - `inst/example_data/standard/adam_adsl.csv`

Previous scaffold expectation, now corrected:

  - Do not profile `pharmaverse SDTM DM` as the default raw example.
  - Do not treat standard SDTM/ADaM outputs as messy source input.

## Step 3 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Data Standardization`:

1. Keep `Data source = Load example dataset`.
2. Select `Generated messy raw adverse events from pharmaverse SDTM AE`.
3. Confirm `Profile` is successful.
4. Open `Mapping`.
5. Confirm `Mapping source` shows a valid recommended mapping.
6. Confirm `Mapping summary` shows 11 mapped rows and target dataset `AE`.
7. Confirm the recommendation table contains source variables such as `verbatim_term`, `severity`, `serious`, `ae_start_date`, with target variables such as `AETERM`, `AESEV`, `AESER`, `AESTDTC`.
8. Click `Download blank template`; confirm it downloads a CSV with the required mapping columns.
9. Click `Download current mapping`; confirm it downloads the current recommended mapping.
10. Upload `inst/example_data/raw/metadata/raw_ae_crf_mapping_key.csv`; confirm Mapping remains valid.
11. Upload `inst/templates/mapping_template.csv`; confirm the file is accepted and standardized to the app mapping columns.
12. Switch to `Standards Build`; it should be unlocked after a valid Mapping stage.

## Step 4 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Data Standardization`:

1. Select `PHUSE TDF DM raw-like SAS dataset`.
2. Confirm `Profile` and `Mapping` are valid.
3. Confirm Profile/Mapping status appears as a compact line rather than a large stage card.
4. Confirm the Mapping recommendation table supports both horizontal and vertical scrolling.
5. Open `Standards Build`.
6. Click `Build standards outputs`.
7. Confirm the build summary shows built dataset `DM`.
8. Confirm `Built dataset preview` shows mapped DM columns and system-added `STUDYID`/`DOMAIN`.
9. Confirm `Traceability` shows source variables mapped to target variables plus system-added variables.
10. Repeat with `Generated messy raw adverse events from pharmaverse SDTM AE`; expected built dataset is `AE`.
11. Repeat with `Generated messy raw time-to-event extract from pharmaverse ADaM ADTTE`; expected built dataset is `ADTTE` and TLF-ready summary should show event/censor counts.

## Step 5 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Data Standardization`:

1. Select `Generated messy raw adverse events from pharmaverse SDTM AE`.
2. Confirm `Profile` and `Mapping` are valid.
3. Open `Standards Build` and click `Build standards outputs`.
4. Confirm built dataset `AE`, TLF-ready summary, and traceability are shown.
5. Open `QC & Traceability`.
6. Click `Run QC checks`.
7. Confirm QC summary, rule checks, reference difference check, and traceability are shown.
8. Confirm QC status is `pass` or `warn`; warnings are expected for generated examples because duplicate/noisy rows were intentionally added.
9. Click `Download QC report`; confirm the CSV contains `rule_check` and `reference_diff` sections.
10. Confirm `Export` unlocks after QC has no hard failure.

## Step 6 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Data Standardization`:

1. Select `Generated messy raw adverse events from pharmaverse SDTM AE`.
2. Run Profile, Mapping, Standards Build, and QC.
3. Open `Export`.
4. Confirm the package manifest is shown.
5. Click `Download delivery package (.zip)`.
6. Confirm the ZIP contains:
   - `datasets/ae.csv`
   - `specifications/mapping_current.csv`
   - `qc/qc_report.csv`
   - `qc/rule_checks.csv`
   - `qc/reference_diff.csv`
   - `traceability/traceability.csv`
   - `tlf/tlf_ready_summary.csv`
   - `manifest.csv`
7. Confirm individual downloads work for built datasets, mapping, QC report, and traceability.

## Step 7 Local Validation

Run:

```r
shiny::runApp()
```

Then check `Sample Size`:

1. Set `Input mode = Estimate from data`.
2. Select an endpoint in Settings:
   - Binary uses staged `adam_adrs_onco.csv`.
   - Continuous uses staged `adam_adsl.csv`.
   - Time-to-event uses staged `adam_adtte_onco.csv`.
3. Open the `Data` tab.
4. Click `Estimate assumptions`.
5. Confirm estimated assumptions, detail table, and provenance are shown.
6. Click `Apply estimates to Design`.
7. Confirm Settings returns to `Manual assumptions`, the endpoint/design inputs are populated from the estimate, and the `Design` tab shows a computed sample-size result.
8. Confirm `Methods` and `Report` show provenance from staged ADaM-like data.

Current Step 7 scope is staged ADaM-like data only. User-uploaded sample-size estimation files are the next small increment.
