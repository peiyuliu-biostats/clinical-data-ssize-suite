# Data Resource Manifest

This project uses public, non-patient-identifying professional test data and terminology sources for the first version of the data standardization and sample-size dashboards.

## Prepared Locally

### pharmaversesdtm

- Source: https://github.com/pharmaverse/pharmaversesdtm
- Local archive: `inst/references/pharmaverse/pharmaversesdtm-main.zip`
- Local extracted source: `inst/references/pharmaverse/pharmaversesdtm-main/`
- License reported by GitHub: Apache-2.0.
- Purpose in this app:
  - SDTM-like standard reference datasets.
  - Dataset metadata/specification reference.
  - Example standard data for `Standards Build`, `QC & Traceability`, and sample-size `Estimate from data`.
  - Not listed in the raw/source staged example selector for the `Profile` tab.
- App-ready extracted files:
  - `inst/example_data/standard/sdtm_dm.csv`
  - `inst/example_data/standard/sdtm_ae.csv`
  - `inst/example_data/standard/sdtm_ex.csv`
  - `inst/example_data/standard/sdtm_lb.csv`
  - `inst/example_data/standard/sdtm_vs.csv`
  - `inst/example_data/standard/sdtm_rs_onco.csv`
  - `inst/references/pharmaverse/sdtms-specs.json`

### pharmaverseadam

- Source: https://github.com/pharmaverse/pharmaverseadam
- Local archive: `inst/references/pharmaverse/pharmaverseadam-main.zip`
- Local extracted source: `inst/references/pharmaverse/pharmaverseadam-main/`
- License reported by GitHub: Apache-2.0.
- Purpose in this app:
  - ADaM-like standard reference datasets.
  - ADaM dataset metadata/specification reference.
  - Estimate-from-data inputs for response and time-to-event sample-size workflows.
  - Not listed in the raw/source staged example selector for the `Profile` tab.
- App-ready extracted files:
  - `inst/example_data/standard/adam_adsl.csv`
  - `inst/example_data/standard/adam_adae.csv`
  - `inst/example_data/standard/adam_adtte_onco.csv`
  - `inst/example_data/standard/adam_adrs_onco.csv`
  - `inst/references/pharmaverse/adams-specs.json`
  - `inst/references/pharmaverse/adams-specs.xlsx`

### PHUSE TestDataFactory

- Source: https://github.com/phuse-org/TestDataFactory
- Local app-ready raw-like example:
  - `inst/example_data/raw/phuse_tdf_dm.sas7bdat`
- Source file: `https://raw.githubusercontent.com/phuse-org/TestDataFactory/main/Data/dm.sas7bdat`
- Purpose in this app:
  - Public source-like SAS dataset for upload/profile testing.
  - Demonstrates SAS dataset upload support.
- Note:
  - Full repository archive download was attempted but did not complete reliably in this environment. Use `scripts/download_public_resources.ps1` or browser download if the full PHUSE archive is needed later.

### Generated messy raw examples

- Generator: `scripts/generate_messy_raw_examples.R`
- Source standard datasets:
  - `inst/example_data/standard/sdtm_dm.csv`
  - `inst/example_data/standard/sdtm_ae.csv`
  - `inst/example_data/standard/sdtm_ex.csv`
  - `inst/example_data/standard/sdtm_lb.csv`
  - `inst/example_data/standard/sdtm_vs.csv`
  - `inst/example_data/standard/sdtm_rs_onco.csv`
  - `inst/example_data/standard/adam_adtte_onco.csv`
- Local app-ready raw/source-like examples:
  - `inst/example_data/raw/raw_dm_demographics.csv`
  - `inst/example_data/raw/raw_ae_crf.csv`
  - `inst/example_data/raw/raw_ex_dosing_log.csv`
  - `inst/example_data/raw/raw_lb_vendor.csv`
  - `inst/example_data/raw/raw_vs_clinic.csv`
  - `inst/example_data/raw/raw_rs_oncology.csv`
  - `inst/example_data/raw/raw_tte_events.csv`
- Manifest:
  - `inst/example_data/raw/generated_raw_manifest.csv`
- Metadata and validation fixtures:
  - `inst/example_data/raw/metadata/*_mapping_key.csv`
  - `inst/example_data/raw/metadata/*_expected_*.csv`
  - `inst/example_data/raw/metadata/*_metadata.json`
- Purpose in this app:
  - Representative messy raw/source-like examples for profiling, mapping, standards build, QC, and later sample-size estimate-from-data workflows.
  - Deterministic reverse-standardization examples with traceability to public pharmaverse SDTM/ADaM test data.

## Public Sources Not Fully Downloaded Here

### NCI EVS CDISC Controlled Terminology

- Index: https://evs.nci.nih.gov/ftp1/CDISC/
- SDTM terminology folder: https://evs.nci.nih.gov/ftp1/CDISC/SDTM/
- ADaM terminology folder: https://evs.nci.nih.gov/ftp1/CDISC/ADaM/
- Purpose in this app:
  - Controlled terminology checks and value mapping.
  - QC warnings for invalid standard terms.
- Note:
  - Direct scripted download returned HTTP 403 from this environment, although the public index is visible. Download manually with a browser if needed:
    - `SDTM Terminology.xls` or `SDTM Terminology.txt`
    - `ADaM Terminology.xls` or `ADaM Terminology.txt`

### CDISC SDTMIG / ADaMIG

- Source: https://www.cdisc.org/standards
- Purpose in this app:
  - Authoritative implementation guide text and variable requirements.
  - Regulatory-grade interpretation of SDTM/ADaM structures.
- Note:
  - CDISC implementation guides commonly require a CDISC account/license acceptance. They should be downloaded by the user and placed under `inst/references/cdisc/` if full IG text is needed.

## Templates Prepared Locally

- `inst/templates/mapping_template.csv`
- `inst/templates/scenario_template.csv`

These templates are starter files for app upload/download workflows. They are not source data and can be edited as the app evolves.
