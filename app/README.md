# App — DQ DualSight (Single-File Shiny)

This folder contains the Shiny application as a single file: `app.R`.

## Run locally

From the project root (one level up):

```r
source('install_packages.R')   # one-time setup
source('run_app.R')            # or shiny::runApp('app', launch.browser = TRUE)
```

## Expected inputs

The app expects a rectangular table with columns that can be mapped to a standard schema via **Step 2** (mapping). Typical fields:

- `patient_id`
- `gender`
- `age` **or** `birth_date`
- `admission_date`, `discharge_date`
- `icd` (ICD-10-GM)
- `ops` (OPS)
- `anamnese` (free text, optional)

## Workflow (tabs)

1. **Load Data** — upload and preview.  
2. **Map Columns** — align your headers to the app schema.  
3. **Define Custom Checks** — compose checks tailored to your study.  
4. **Select Checks** — toggle built-in and custom rules.  
5. **Results & Data Fitness** — review issues; download CSV reports.  
6. **Anonymization Metrics** — compare original vs anonymized datasets.

## Dependencies

Installed by `install_packages.R`:

- shiny, bs4Dash, DT, readxl, jsonlite, stringr, dplyr, lubridate, data.table, shinyjs, shinyWidgets, rintrojs, waiter, shinyjqui

## Notes

- The anonymization module provides *fitness-for-use diagnostics*; it does **not** certify privacy compliance.
- Use de-identified or synthetic data for demonstrations and tests.
