# Clinical Trial Data Standards & Sample Size Suite

An R Shiny application that supports two connected tasks in clinical trial work:

1. **Data standardization** — turning raw clinical data into SDTM/ADaM-like datasets, with profiling, mapping, building, quality control, and export.
2. **Sample-size and power calculation** — endpoint-specific, closed-form (or exact-search) calculators for binary, continuous, and time-to-event endpoints.

The two tasks are connected: the assumptions of the sample-size calculation can be estimated directly from standardized ADaM-like data, so that the same design engine is used for both manual and data-driven inputs.

Repository: <https://github.com/peiyuliu-biostats/clinical-data-ssize-suite>

---

## Contents

- [Features](#features)
- [Application structure](#application-structure)
- [Statistical methods](#statistical-methods)
- [Installation](#installation)
- [Running the application](#running-the-application)
- [Project layout](#project-layout)
- [Data sources](#data-sources)
- [Limitations](#limitations)
- [References](#references)
- [Author](#author)

---

## Features

**Data Standardization**

- **Profile** — read a raw table and summarize variable types, missing values, unique values, date candidates, inferred domain, and readiness checks.
- **Mapping** — map source variables to SDTM/ADaM targets from a local dictionary, with automatic recommendations (exact, name-containment, label-containment, and approximate edit-distance matching) and confidence labels.
- **Standards Build** — apply the mapping, parse dates to ISO 8601 with per-value flags, normalize Y/N and numeric fields, derive `USUBJID = STUDYID-SITEID-SUBJID`, and assign a reproducible `--SEQ`.
- **QC & Traceability** — rule-based checks (required variables, `STUDYID`, `DOMAIN`, `USUBJID` completeness, duplicates, ISO dates, Y/N terminology), date-parse reporting, and a key-based comparison against an expected reference.
- **Export** — write datasets, mapping specification, QC report, traceability, TLF-ready summary, and a manifest to a single ZIP package.

**Sample Size**

- **Design** — compute the required sample size (or number of events) for the selected endpoint, with a full table of traceable assumptions.
- **Scenarios** — evaluate a table of parameter sets (uploaded or generated as base / optimistic / conservative) with the same engine.
- **Sensitivity** — vary one parameter over a grid and recompute required N or events, with table and plot.
- **Methods** — show the live formula, hypothesis, and resolved inputs for the current design.
- **Report** — assemble a protocol-ready summary that can be exported as text or CSV.
- **ADaM estimation** — estimate endpoint assumptions from example, built, or uploaded ADaM-like data (ADRS, ADSL, ADTTE).

---

## Application structure

```
Data Standardization : Profile -> Mapping -> Standards Build -> QC & Traceability -> Export
Sample Size          : Design  -> Scenarios -> Sensitivity   -> Methods           -> Report
```

A complete description of the workflow and every formula is available in the **Methods Guide** tab inside the running application.

---

## Statistical methods

Let `Φ` be the standard normal CDF and `Φ⁻¹` its quantile function. For significance level `α` and power `1 − β`, define `a = α` for a one-sided test and `a = α/2` for a two-sided test, `z_a = Φ⁻¹(1 − a)`, and `z_β = Φ⁻¹(power)`. The allocation ratio is `k = n_t / n_c`. Every raw sample size is inflated for dropout as `n / (1 − d)` and rounded up.

The tested effect `δ` depends on the objective, given a raw difference `Δ` and margin `m`:
`equality: δ = Δ`, `superiority: δ = Δ − m`, `non-inferiority: δ = Δ + m`, `equivalence: δ = m − |Δ|`.
For time-to-event endpoints, only the equality objective is implemented.

### Binary endpoint

- **One-sample, normal approximation:**
  `n = [ z_a·√(p0(1−p0)) + z_β·√(p1(1−p1)) ]² / (p1 − p0)²`
- **One-sample, exact binomial:** the smallest `n` whose exact binomial test reaches the target power under `p1`; because exact power is not monotone in `n`, the next `w` sizes (confirmation window, default 1) must also hold.
- **Two-sample, normal approximation:**
  `n_c = (z_a + z_β)²·[ p_c(1−p_c) + p_t(1−p_t)/k ] / (p_t − p_c)²`, `n_t = k·n_c`.

### Continuous endpoint

Solved exactly via the noncentral t distribution. Power at `n` is `P(T > t_crit)` with noncentrality `λ`:

- one-sample / paired: `df = n − 1`, `λ = d·√n`, `d = |δ| / σ_eff`;
- paired: `σ_eff = σ·√(2(1 − ρ))`;
- two-sample: `df = n_c + n_t − 2`, `λ = (|δ|/σ)·√(n_c·n_t/(n_c + n_t))`.

### Time-to-event endpoint

- **One-sample (Schoenfeld):** `d = (z_a + z_β)² / (ln HR)²`. If medians are supplied, `HR = M_ref / M_new` under an exponential model.
- **Two-sample (log-rank):** `d = (z_a + z_β)²·(1 + k)² / (k·(ln HR)²)`, translated to total `N = d / P(event)`. `P(event)` is supplied or derived under an exponential model with uniform accrual, using Simpson's rule over `[f, a + f]`.

### Estimation from ADaM-like data

- **Binary (ADRS):** response rate per arm = responders / n.
- **Continuous (ADSL):** pooled SD `s_p = √( ((n_c−1)s_c² + (n_t−1)s_t²) / (n_c + n_t − 2) )`.
- **Time-to-event (ADTTE):** exponential hazard `λ = (events + 0.5) / total_time` (0.5 continuity correction), median `= ln 2 / λ`, `HR = λ_t / λ_c`, event probability `= total events / total subjects`.

---

## Installation

The suite requires **R (≥ 4.0)**.

Required packages:

```r
install.packages(c("shiny", "shinydashboard"))
```

Optional packages, needed only for specific file formats or features:

```r
install.packages(c(
  "survival",  # ADTTE survival estimation
  "haven",     # reading .sas7bdat / .xpt
  "readxl",    # reading .xlsx
  "zip"        # robust ZIP export
))
```

---

## Running the application

From the repository root:

```r
shiny::runApp(".")
```

Or from a terminal:

```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

On start-up, the application sources every `.R` file under `functions/` and loads the two dashboard sections. Staged example data under `inst/` lets you validate the full workflow without uploading any data.

---

## Project layout

```
app.R                       Application entry point (UI, server, Methods Guide)
functions/
  shared/basic/             Shared UI helpers and CSS
  stdz/
    basic/                  Profiling, mapping, build, QC, export logic
    data/                   Standardization reactive-value initializer
    panel/                  Standardization UI modules
  ssize/
    basic/                  Sample-size engine, ADaM estimation, batch/sensitivity
    data/                   Sample-size reactive-value initializer
    panel/                  Sample-size UI modules
inst/
  example_data/             Staged raw, SDTM, and ADaM example datasets
  references/               pharmaverse / PHUSE reference material
```

All functions in `functions/*/basic/` are pure (free of Shiny), so they can be unit-tested directly.

---

## Data sources

The staged example datasets under `inst/example_data/` are derived from public
resources, including pharmaverse example SDTM/ADaM data and the PHUSE Test Data
Factory. They are provided for local validation and demonstration only.

---

## Limitations

Each calculation is valid only under its stated model (normal approximation or exact binomial for proportions, noncentral t for means, exponential survival with proportional hazards for time-to-event). The two-sample survival sample size depends on the assumed event-probability model. The engine does not model interim analyses, group-sequential boundaries, stratification, competing risks, clustering, or covariate adjustment. Results are intended to support design decisions and should be confirmed with an independent method before use in a study protocol.

---

## References

- Schoenfeld DA. Sample-size formula for the proportional-hazards regression model. *Biometrics.* 1983;39(2):499–503.
- Chow SC, Shao J, Wang H, Lokhnygina Y. *Sample Size Calculations in Clinical Research.* 3rd ed. Chapman and Hall/CRC; 2018.
- Julious SA. *Sample Sizes for Clinical Trials.* Chapman and Hall/CRC; 2010.
- CDISC. Study Data Tabulation Model (SDTM) and Analysis Data Model (ADaM) Implementation Guides. <https://www.cdisc.org>.

---

## Author

**Peiyu Liu**
Department of Biostatistics, University of Florida
Contact: [peiyu.liu.stats@gmail.com](mailto:peiyu.liu.stats@gmail.com)

Questions and suggestions are welcome.
