# Data — Demo Inputs

This folder contains demonstration datasets for the DQ DualSight app.

- **`demo_original.csv`** — load in **Step 1** of the app.
- **`demo_anonymized.csv`** — load in **Step 6** for comparisons against the original.

## Sizes

- `demo_original.csv`: ~300 rows, 10 columns.
- `demo_anonymized.csv`: ~300 rows, 9 columns.

## Inferred schema (original)

| Column | Inferred type |
|---|---|
| patient_id | object |
| gender | object |
| age | int64 |
| birth_date | object |
| admission_date | object |
| discharge_date | object |
| icd | object |
| ops | object |
| anamnese | object |
| los_days | int64 |

## Inferred schema (anonymized)

| Column | Inferred type |
|---|---|
| gender | object |
| age | int64 |
| birth_date | object |
| admission_date | object |
| discharge_date | object |
| icd | object |
| ops | object |
| anamnese | object |
| los_days | int64 |
## Usage tips

- Ensure the column names you want to analyze are mapped correctly in **Step 2** of the app.
- If your real data uses different headers, mapping can adapt them to the app’s standard schema within the app visually.
- CSVs should be UTF‑8 encoded. If your files use semicolons, choose the appropriate separator in the loader.

> **Privacy note:** These demo files are intended for local testing and do not contain personally identifiable information in this template. Replace with your own de-identified data for real analyses.
