# DQ DualSight — Shiny App Template

A minimal, GitHub-ready folder that ships the **DQ DualSight** Shiny application (single-file `app.R`), demonstration datasets (original + anonymized), and the metadata concept (check catalog). You can clone this, run the app locally, and replace the demo inputs with your own data.

## What’s here

- **`app/`** — the Shiny app as a single file (`app.R`).
- **`data/`** — two CSVs you can load in Step 1 and Step 6 of the app.
- **`metadata/`** — the metadata concept, including the check catalog (Excel).
- Helper scripts: `install_packages.R`, `run_app.R`.
- Standard `LICENSE` and `.gitignore` for a clean repository.

## Quick start

1. Install R ≥ 4.2 and RStudio (optional).
2. Open R in this folder and run:

```r
source('install_packages.R')   # installs required CRAN packages (one time)
source('run_app.R')            # launches the app locally
# or: shiny::runApp('app', launch.browser = TRUE)
```

3. In the app, go to **Step 1** to load `data/demo_original.csv`.  
   Optionally go to **Step 6** to load `data/demo_anonymized.csv` for comparisons.

> **Note:** Files are only processed locally during your Shiny session. Use synthetic or de-identified data for testing.

## Folder structure

```
DQDualSight/
├─ README.md
├─ LICENSE
├─ .gitignore
├─ install_packages.R
├─ run_app.R
├─ app/
│  ├─ app.R
│  └─ README.md
├─ data/
│  ├─ demo_original.csv
│  ├─ demo_anonymized.csv
│  └─ README.md
└─ metadata/
   ├─ check_catalog.xlsx
   └─ README.md
```

## Demo data at a glance

- `demo_original.csv`: ~300 rows, 10 columns.
- `demo_anonymized.csv`: ~N/A rows, 0 columns.

See `data/README.md` for an inferred schema and usage notes.

## Metadata concept

`metadata/check_catalog.xlsx` documents the built-in and custom check logic.  
See `metadata/README.md` for how checks map to data elements and how to extend them.

## Citation

If you cite this tool in a manuscript, please credit the authors and reference the live demo (if used).

## License

Released under the MIT License (see `LICENSE`).

---

© 2025 Gaetan Kamdje Wabo, Piotr Sokolowski, Thomas Ganslandt, Fabian Siegel.
