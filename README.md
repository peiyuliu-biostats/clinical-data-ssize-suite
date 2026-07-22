# Clinical Trial Data Standards & Sample Size Suite

An R Shiny application for clinical trials that combines two connected tasks:

- **Data standardization** — profile raw data, map it to SDTM/ADaM, build datasets, run QC, and export.
- **Sample size & power** — closed-form calculators for binary, continuous, and time-to-event endpoints. Assumptions can be typed manually or estimated from standardized ADaM-like data.

Full workflow and formulas are documented in the app's **Methods Guide** tab.

## Install

Requires R (≥ 4.0).

```r
install.packages(c("shiny", "shinydashboard"))
# optional, per feature/format:
install.packages(c("survival", "haven", "readxl", "zip"))
```

## Run

```r
shiny::runApp(".")
```

Staged example data under `inst/` lets you try the full workflow without uploading files.

## Author

**Peiyu Liu** — Department of Biostatistics, University of Florida
[peiyu.liu.stats@gmail.com](mailto:peiyu.liu.stats@gmail.com)
