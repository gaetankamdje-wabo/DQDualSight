###############################################################################
# DQ DualSight – Data Quality Assessment & Anonymization Impact Dashboard
# -----------------------------------------------------------------------------
# Short description
# An interactive R/Shiny dashboard (bs4Dash) to (i) assess data quality (DQ)
# across completeness, plausibility, temporal consistency, diagnosis–procedure
# consistency, and code integrity, and (ii) quantify the impact of de-
# identification on analytic utility through side-by-side comparisons of
# original vs anonymized datasets.
#
# Authors and affiliations
# Gaetan Kamdje Wabo, MSc; Piotr Sokolowski, MD; Thomas Ganslandt, MD;
# Fabian Siegel, MD
# Mannheim Institute for Intelligent Systems in Medicine (MIISM),
# Department of Biomedical Informatics, Medical Faculty Mannheim,
# Heidelberg University, Mannheim, Germany
# Correspondence: gaetankamdje.wabo@medma.uni-heidelberg.de
#
# Intended use / scope
# • Research data screening prior to cohort extraction, study handover, or
#   secondary analysis.
# • Local QA by clinical and data management teams, with optional assessment
#   of anonymization effects on key variables (distributional shifts, loss of
#   granularity, added missingness).
# • Not a medical device. No automated clinical decision support.
#
# Functional overview
# • Step 1 – Load data: CSV/TXT, Excel, JSON (FHIR demo stub). Preview rows.
# • Step 2 – Map columns: align user columns to the app’s standard schema
#   (patient_id, gender, age/birth_date, admission_date, discharge_date,
#   icd, ops, anamnese).
# • Step 3 – Define custom checks (“DQ Builder”): compose reusable, auditable
#   rules (AND/OR; string and numeric operators) tailored to a specific data-use
#   context; assign severity and documentation strings.
# • Step 4 – Select checks: toggle built-ins and custom rules, with long,
#   descriptive labels stating what each check flags and the required fields.
# • Step 5 – Results & data fitness: aggregate issues (counts, severity, %
#   affected), inspect detailed rows, download a CSV report, visualize category
#   distributions.
# • Step 6 – Anonymization metrics: load an anonymized counterpart; compare
#   numeric and categorical columns (min/max/mean/SD, KS p-values, distinct
#   counts; missingness, level overlap, chi-square). Export ad-hoc comparisons
#   and receive adaptive recommendations about fitness-for-use.
#
# Built-in checks (high-level)
# • Category 1: Completeness (e.g., admissions without ICD/OPS; concept-text
#   mentions without corresponding codes).
# • Category 2: Age-based plausibility (e.g., pediatric vs geriatric bounds).
# • Category 3: Gender-based plausibility (sex-specific diagnoses).
# • Category 4: Temporal consistency (e.g., discharge before admission).
# • Category 5: Diagnosis–procedure consistency (e.g., appendectomy without K35).
# • Category 6: Code integrity (ICD-10-GM 2025/OPS 2025 syntax, near-miss
#   suggestions, unspecified/retired/outdated code heuristics).
#
# Input expectations
# • Rectangular table with (ideally) the following columns:
#   patient_id, gender, age or birth_date, admission_date, discharge_date,
#   icd, ops, anamnese (free text).
# • Dates should be parseable to Date; numeric fields should be coercible where
#   relevant. Heterogeneous schemas are supported via Step 2 mapping.
#
# Outputs and exports
# • On-screen summaries (issue counts by check/category/severity).
# • Detailed, per-check issue listings for audit and remediation.
# • CSV downloads: full DQ report, ad-hoc anonymization comparison tables.
#
# Design notes for reproducibility and auditability
# • Each check has: category, check_id, short description (“what it flags”),
#   required_data_elements, and implementation detail. Custom rules keep a
#   stored expression and severity. Labels in Step 4 expose these elements.
# • The DQ Builder favors explicit, human-readable expressions to facilitate
#   review, reuse, and protocol-level documentation.
#
# Data protection, privacy, and ethics
# • Files are processed in-memory during a session; no PHI is intentionally
#   persisted by the app. Operators hosting a server instance act as data
#   controllers and must implement network, access, and logging safeguards.
# • The anonymization module is for “fitness for use” diagnostics only; it does
#   not certify compliance. Final privacy risk assessments remain the
#   responsibility of the data controller and DPO under applicable law (e.g.,
#   GDPR). Use synthetic data for demonstrations and unit tests.
#
# Validation and testing
# • Includes unit-style checks on ICD-10-GM/OPS syntax and “near-miss”
#   normalization; synthetic datasets recommended for regression tests of
#   built-in and custom rules. Visual smoke tests cover Step-wise workflow.
#
# Dependencies (R packages)
# shiny, bs4Dash, DT, readxl, jsonlite, stringr, dplyr, lubridate, rlang,
# data.table, shinyjs, shinyWidgets, rintrojs, waiter, shinyjqui (plus base stats).
# Minimum R version: 4.2+ (tested up to current releases).
#
# Quick start (local)
# install.packages(c("shiny","bs4Dash","DT","readxl","jsonlite","stringr",
#   "dplyr","lubridate","data.table","shinyjs","shinyWidgets","rintrojs",
#   "waiter","shinyjqui"))
# shiny::runApp("path/to/app")
#
# Keyboard shortcuts
# N/P = next/previous tab; '?' = guided tour; 'G' = toggle dark mode (keyboard).
#(JMIR Medical Informatics; manuscript in preparation)
# URL (demo): https://gaet.shinyapps.io/DQDualSight/
#
# License and copyright
# © 2025 Gaetan Kamdje Wabo et al. All rights reserved.
# License: MIT.
#
# Funding and competing interests
# Funding: Medical Informatics in Research and Care in University Medicine consortium (grants 01ZZ1801E and 01ZZ1801A)
# Competing interests: the authors declare none.
#
# Changelog / versioning
# Version: 1.0.0 (2025-09-10)
# • Initial public release with 6 DQ categories, custom rule builder, and
#   anonymization fitness module (numeric and categorical comparators).
###############################################################################

###############################################################################

library(shiny)
library(bs4Dash)
library(DT)
library(readxl)
library(jsonlite)
library(stringr)
library(dplyr)
library(lubridate)
library(rlang)
library(stats)
library(data.table)
library(shinyjs)
options(shiny.maxRequestSize = 2 * 1024^3)
###############################################################################
# ========================= MODERNIZATION PACK (drop-in) =========================
# RUNTIME DEPENDENCIES (commented install lines, if needed):
# install.packages(c("shinyWidgets","rintrojs","waiter","shinyjqui"))
library(shinyWidgets)
library(rintrojs)
library(waiter)
library(shinyjqui)

# ===================== ROBUST JSON / FHIR READERS (DROP-IN) =====================

# Safely flatten any list-of-lists to a rectangular data.frame
as_rectangular <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x))
  if (is.null(x))       return(data.frame())
  # If top-level has a common key that holds the records, descend into it
  for (k in c("data","results","items","records","entry","hits","rows")) {
    if (is.list(x) && !is.null(x[[k]])) return(as_rectangular(x[[k]]))
  }
  # List of homogeneous objects → rbind
  if (is.list(x)) {
    try({
      lst <- lapply(x, function(e) as.data.frame(jsonlite::flatten(e), stringsAsFactors = FALSE))
      df  <- data.table::rbindlist(lst, fill = TRUE)
      return(as.data.frame(df))
    }, silent = TRUE)
  }
  # Last resort
  as.data.frame(x, stringsAsFactors = FALSE)
}

# Read standard JSON OR NDJSON (JSON Lines) and return a rectangular data.frame
read_json_tabular <- function(path) {
  # 1) Try standard JSON
  df <- tryCatch({
    obj <- jsonlite::fromJSON(path, flatten = TRUE)
    as_rectangular(obj)
  }, error = function(e) NULL)
  
  # 2) Try NDJSON (streaming)
  if (is.null(df) || (!is.data.frame(df)) || nrow(df) == 0) {
    con <- file(path, open = "r")
    on.exit(close(con), add = TRUE)
    df <- tryCatch({
      jsonlite::stream_in(con, flatten = TRUE, verbose = FALSE)
    }, error = function(e) NULL)
  }
  
  # 3) Try JSON Lines by hand (each line is a JSON object)
  if (is.null(df) || (!is.data.frame(df)) || nrow(df) == 0) {
    lines <- readLines(path, warn = FALSE)
    recs <- lapply(lines[nzchar(lines)], function(z) {
      tryCatch(jsonlite::fromJSON(z, flatten = TRUE), error = function(e) NULL)
    })
    recs <- recs[!vapply(recs, is.null, logical(1))]
    if (length(recs)) {
      df <- as.data.frame(data.table::rbindlist(
        lapply(recs, function(e) as.data.frame(jsonlite::flatten(e), stringsAsFactors = FALSE)),
        fill = TRUE
      ))
    }
  }
  
  if (is.null(df) || (!is.data.frame(df)) || nrow(df) == 0) {
    stop("Could not parse JSON or NDJSON into a rectangular table.")
  }
  df
}

# ------- FHIR Bundle (JSON) → rectangular table (Patient + Encounter anchor) ------
# Supports resourceType="Bundle" with entry[].resource of types Patient|Encounter|Condition|Procedure
read_fhir_tabular <- function(path) {
  # Parse as raw list to keep the FHIR structure
  bundle <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(bundle)) stop("FHIR JSON not recognized.")
  # If it's already a resource, great; else expect Bundle with entry
  resources <- if (!is.null(bundle$entry)) {
    lapply(bundle$entry, `[[`, "resource")
  } else {
    list(bundle)
  }
  
  # Collect by type --------------------------------------------------------------
  patients   <- lapply(resources, function(r) if (identical(r$resourceType, "Patient"))   r else NULL)
  encounters <- lapply(resources, function(r) if (identical(r$resourceType, "Encounter")) r else NULL)
  conds      <- lapply(resources, function(r) if (identical(r$resourceType, "Condition")) r else NULL)
  procs      <- lapply(resources, function(r) if (identical(r$resourceType, "Procedure")) r else NULL)
  
  patients   <- patients[!vapply(patients, is.null, logical(1))]
  encounters <- encounters[!vapply(encounters, is.null, logical(1))]
  conds      <- conds[!vapply(conds, is.null, logical(1))]
  procs      <- procs[!vapply(procs, is.null, logical(1))]
  
  # Helpers to extract simple fields --------------------------------------------
  get_ref_id <- function(ref) {  # e.g., "Patient/123" -> "123"
    if (is.null(ref)) return(NA_character_)
    sub("^.*/", "", ref)
  }
  first_code <- function(coding) {  # first code from coding array
    if (is.null(coding) || !length(coding)) return(NA_character_)
    val <- coding[[1]]$code
    if (is.null(val)) NA_character_ else as.character(val)
  }
  first_text <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (!is.null(x$text)) return(as.character(x$text))
    NA_character_
  }
  
  # Build small tables -----------------------------------------------------------
  pat_df <- if (length(patients)) {
    data.frame(
      patient_id = vapply(patients, function(p) as.character(p$id %||% NA), character(1)),
      gender     = vapply(patients, function(p) as.character(p$gender %||% NA), character(1)),
      birth_date = vapply(patients, function(p) as.character(p$birthDate %||% NA), character(1)),
      stringsAsFactors = FALSE
    )
  } else data.frame(patient_id=character(), gender=character(), birth_date=character())
  
  enc_df <- if (length(encounters)) {
    data.frame(
      enc_id        = vapply(encounters, function(e) as.character(e$id %||% NA), character(1)),
      patient_id    = vapply(encounters, function(e) get_ref_id(e$subject$reference %||% NA), character(1)),
      admission_date= vapply(encounters, function(e) as.character(e$period$start %||% NA), character(1)),
      discharge_date= vapply(encounters, function(e) as.character(e$period$end   %||% NA), character(1)),
      reason_text   = vapply(encounters, function(e) first_text(e$reasonCode[[1]] %||% NULL), character(1)),
      stringsAsFactors = FALSE
    )
  } else data.frame(enc_id=character(), patient_id=character(),
                    admission_date=character(), discharge_date=character(), reason_text=character())
  
  cond_df <- if (length(conds)) {
    data.frame(
      patient_id = vapply(conds, function(cn) get_ref_id(cn$subject$reference %||% NA), character(1)),
      icd        = vapply(conds, function(cn) first_code((cn$code$coding %||% list())[1]), character(1)),
      an_text    = vapply(conds, function(cn) first_text(cn$code %||% NULL), character(1)),
      stringsAsFactors = FALSE
    )
  } else data.frame(patient_id=character(), icd=character(), an_text=character())
  
  proc_df <- if (length(procs)) {
    data.frame(
      patient_id = vapply(procs, function(pr) get_ref_id(pr$subject$reference %||% NA), character(1)),
      ops        = vapply(procs, function(pr) first_code((pr$code$coding %||% list())[1]), character(1)),
      stringsAsFactors = FALSE
    )
  } else data.frame(patient_id=character(), ops=character())
  
  # Aggregate condition/procedure codes by patient (semicolon-separated) --------
  if (nrow(cond_df)) {
    cond_df <- cond_df |>
      dplyr::group_by(patient_id) |>
      dplyr::summarise(
        icd = paste(unique(na.omit(icd)), collapse = "; "),
        anamnese = paste(unique(na.omit(an_text)), collapse = "; "),
        .groups = "drop"
      )
  }
  if (nrow(proc_df)) {
    proc_df <- proc_df |>
      dplyr::group_by(patient_id) |>
      dplyr::summarise(ops = paste(unique(na.omit(ops)), collapse = "; "), .groups = "drop")
  }
  
  # Join: Encounter is the primary (one row per encounter when available) -------
  if (nrow(enc_df)) {
    out <- enc_df |>
      dplyr::left_join(pat_df,  by = "patient_id") |>
      dplyr::left_join(cond_df, by = "patient_id") |>
      dplyr::left_join(proc_df, by = "patient_id") |>
      dplyr::mutate(
        anamnese = dplyr::coalesce(anamnese, reason_text)
      ) |>
      dplyr::select(patient_id, gender, birth_date,
                    admission_date, discharge_date, icd, ops, anamnese)
  } else {
    # Fallback: no Encounter -> one row per patient from Patient/Condition/Procedure
    out <- dplyr::full_join(pat_df, cond_df, by = "patient_id") |>
      dplyr::full_join(proc_df, by = "patient_id") |>
      dplyr::mutate(admission_date = NA_character_,
                    discharge_date = NA_character_) |>
      dplyr::select(patient_id, gender, birth_date,
                    admission_date, discharge_date, icd, ops, anamnese)
  }
  
  # Normalize date columns to Date where possible
  for (dc in c("birth_date","admission_date","discharge_date")) {
    if (dc %in% names(out)) {
      out[[dc]] <- suppressWarnings(as.Date(out[[dc]]))
    }
  }
  out
}
# ================== END ROBUST JSON / FHIR READERS (DROP-IN) ====================


# ---- UI injector: call inside bs4DashBody(...) once as modern_ui() ------------
modern_ui <- function(){
  tagList(
    useWaiter(),      # waiter overlay
    introjsUI(),      # guided tour
    # --- Global modern CSS tweaks & Step-4 collapse button black in light mode ---
    tags$style(HTML("
      :root { --brand:#2563eb; --brand-soft:rgba(37, 99, 235, .08); }
      .main-sidebar, .brand-link { font-weight:600; }
      .content-wrapper { background:#f7f8fb; }
      .card, .bs4-card { border-radius:16px!important; box-shadow:0 8px 24px rgba(0,0,0,.06)!important; border:0!important; }
      .card .card-title { font-weight:700; letter-spacing:.2px; }
      .btn { border-radius:12px!important; }
      .btn-primary { background:var(--brand)!important; border-color:var(--brand)!important; }
      .btn-primary:hover { filter:brightness(1.05); }
      .nav-sidebar > .nav-item > .nav-link.active { background:var(--brand-soft)!important; color:var(--brand)!important; font-weight:700; }

      /* Floating helper (help only; dark removed) */
      .fab-wrap { position:fixed; right:22px; bottom:20px; z-index:1031; }
      .fab-card { background:white; border-radius:16px; box-shadow:0 8px 24px rgba(0,0,0,.15); padding:10px 12px; display:flex; align-items:center; gap:10px; }
      .fab-btn { width:44px; height:44px; border-radius:50%; display:flex; align-items:center; justify-content:center; }

      /* Dark mode theme (kept for keyboard 'g' toggle only; no button shown) */
      body.dark-mode .content-wrapper { background:#0f172a; }
      body.dark-mode .card { background:#0b1220; color:#e5e7eb; box-shadow:0 10px 30px rgba(0,0,0,.35)!important; }
      body.dark-mode .main-sidebar, body.dark-mode .main-header { background:#0b1220!important; }
      body.dark-mode .nav-link, body.dark-mode .brand-link { color:#cbd5e1!important; }
      body.dark-mode .btn-primary { background:#3b82f6!important; border-color:#3b82f6!important; }
      body.dark-mode .table { color:#e5e7eb; }

      /* >>> Step 4 collapse button in black (light mode only) <<< */
      body:not(.dark-mode) #step4_scope .card .btn-tool,
      body:not(.dark-mode) #step4_scope .card .btn-tool .fa,
      body:not(.dark-mode) #step4_scope .card .btn-tool .fas,
      body:not(.dark-mode) #step4_scope .card .btn-tool .far {
        color:#000 !important; opacity:1 !important;
      }
    ")),
    
    # --- Floating helper: only the Help ('?') button now ---
    absolutePanel(
      class = "fab-wrap",
      fixed = TRUE, draggable = FALSE,
      div(
        class = "fab-card",
        actionButton("btn_help_tour", NULL, icon=icon("question"), class="btn btn-primary fab-btn", title="Quick tour ( ? )")
      )
    ),
    
    # --- Keyboard shortcuts (keep 'g' to toggle dark via keyboard only) ---
    tags$script(HTML("
      document.addEventListener('keydown', function(e){
        if (['INPUT','TEXTAREA'].includes((e.target||{}).tagName)) return;
        if (e.key === 'n' || e.key === 'N') { Shiny.setInputValue('__key_next', Math.random(), {priority:'event'}); }
        else if (e.key === 'p' || e.key === 'P') { Shiny.setInputValue('__key_prev', Math.random(), {priority:'event'}); }
        else if (e.key === '?') { Shiny.setInputValue('btn_help_tour', Math.random(), {priority:'event'}); }
        else if (e.key === 'g' || e.key === 'G') {
          const isDark = document.body.classList.contains('dark-mode');
          Shiny.setInputValue('dark_mode', !isDark, {priority:'event'});
        }
      });
      Shiny.addCustomMessageHandler('toggle-dark', function(isDark){
        document.body.classList.toggle('dark-mode', !!isDark);
      });
    "))
  )
}


## ======================= ICD-10-GM & OPS 2025 HELPERS =======================

# ICD-10-GM 2025: one letter + two digits; optional dot and 1–4 alphanumerics
.icd10_gm_regex <- function() stringr::regex("^[A-Z][0-9]{2}(\\.[0-9A-Z]{1,4})?$", ignore_case = TRUE)
icd10_gm_valid <- function(x) stringr::str_detect(x, .icd10_gm_regex())

# Normalizer for “near-miss” fixes: trim, uppercase, remove spaces, common O/0 and I/1 swaps
icd10_gm_normalize <- function(x) {
  y <- x
  y <- gsub("\\s+", "", y, perl = TRUE)
  y <- toupper(y)
  y <- chartr("OI", "01", y)     # common slips: O↔0, I↔1
  y
}

# If original is invalid but normalized becomes valid, propose it as a fix
icd10_gm_near_miss_suggest <- function(x) {
  y <- icd10_gm_normalize(x)
  y[!icd10_gm_valid(x) & icd10_gm_valid(y)]
}

# ICD “shape/length” sanity beyond pure regex:
#   - only [A-Z0-9.] allowed
#   - at most one dot
#   - stem len == 3, subpart len in 1..4 when present
#   - total length between 3 and 8 (e.g., A00 .. A00.9XXX)
icd10_gm_shape_bad <- function(x) {
  up <- icd10_gm_normalize(x)
  bad_charset   <- !stringr::str_detect(up, "^[A-Z0-9.]+$")
  too_many_dots <- stringr::str_count(up, fixed(".")) > 1
  parts <- strsplit(up, "\\.", fixed = FALSE)
  stem_ok <- vapply(parts, function(p) nchar(p[1]) == 3, logical(1))
  sub_ok  <- vapply(parts, function(p) if (length(p) == 2) nchar(p[2]) >= 1 && nchar(p[2]) <= 4 else TRUE, logical(1))
  len_ok  <- nchar(up) >= 3 & nchar(up) <= 8
  bad_charset | too_many_dots | (!stem_ok) | (!sub_ok) | (!len_ok)
}

# ICD “unspecified” flags (broader and GM-friendly):
#   - exact R99
#   - Z00.* (general examination without complaint/suspected/recorded diagnosis)
#   - any subcategory that ends with .9, .90, .99, .9X*, which is commonly “unspecified/NEC/NOS”
icd10_gm_unspecified <- function(x) {
  up <- icd10_gm_normalize(x)
  stringr::str_detect(
    up,
    "(^R99$)|(^Z00(\\.|$))|(\\.(9|90|99|9[A-Z0-9]{1,3})$)"
  )
}

# OPS 2025: one digit (1–9), hyphen, 2–3 digits, then 0–2 dot groups of 1–3 alphanumerics
.ops_regex <- function() stringr::regex("^[1-9]-[0-9]{2,3}(\\.[0-9A-Z]{1,3}){0,2}$", ignore_case = TRUE)
ops_valid <- function(x) stringr::str_detect(x, .ops_regex())

# Normalizer for OPS near-miss: unify dashes, trim, uppercase, O/0, I/1, add hyphen if missing
normalize_dash <- function(s) gsub("[\\u2010\\u2011\\u2012\\u2013\\u2014\\u2212]", "-", s) # hyphen variants
ops_normalize <- function(x) {
  y <- x
  y <- normalize_dash(y)
  y <- gsub("\\s+", "", y, perl = TRUE)
  y <- toupper(y)
  y <- chartr("OI", "01", y)
  # if missing hyphen after first digit, insert it (e.g., "5820.1" -> "5-820.1")
  y <- ifelse(!grepl("^[0-9]-", y) & grepl("^[0-9]", y),
              paste0(substr(y, 1, 1), "-", substr(y, 2, nchar(y))),
              y)
  y
}

ops_near_miss_suggest <- function(x) {
  y <- ops_normalize(x)
  y[!ops_valid(x) & ops_valid(y)]
}

ops_shape_bad <- function(x) {
  y  <- ops_normalize(x)
  bad_charset <- !stringr::str_detect(y, "^[0-9A-Z.-]+$")
  # exactly one hyphen, at most two dots
  bad_hyphen  <- stringr::str_count(y, fixed("-")) != 1
  bad_dots    <- stringr::str_count(y, fixed(".")) > 2
  # rough total length guard (e.g., "5-820.2ab" ~ up to ~10–12 chars typical)
  bad_len     <- nchar(y) < 4 | nchar(y) > 12
  bad_charset | bad_hyphen | bad_dots | bad_len
}

# Heuristic OPS “unspecified” flags:
#   - common placeholders at the end like ".9", ".99", ".X", ".XX"
ops_unspecified <- function(x) {
  y <- ops_normalize(x)
  stringr::str_detect(y, "\\.(9|90|99|X|XX)$")
}

## =================== DROP-IN REPLACEMENTS (CATEGORY 6) ===================


cat6_1_block <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_1" %in% selected_checks && "icd" %in% names(d)) {
    idx <- which(!icd10_gm_valid(d$icd))
    add_issue("cat6_1", pid_or_row[idx],
              "ICD not matching ICD-10-GM 2025 syntax",
              "Invalid or incorrectly formatted ICD-10-GM code (letter+2 digits; optional . and 1–4 alphanumerics).",
              "Medium")
  }
}

cat6_3_block <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_3" %in% selected_checks && "icd" %in% names(d)) {
    idx_raw  <- which(!icd10_gm_valid(d$icd))
    if (length(idx_raw)) {
      sug <- icd10_gm_near_miss_suggest(d$icd[idx_raw])
      keep <- which(!is.na(sug) & icd10_gm_valid(sug))
      if (length(keep)) {
        ix <- idx_raw[keep]
        add_issue("cat6_3", pid_or_row[ix],
                  "ICD potential typo (ICD-10-GM 2025 near-miss)",
                  paste0("Original: ", d$icd[ix], " → Suggested: ", sug[keep]),
                  "Medium")
      }
    }
  }
}

cat6_7_block <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_7" %in% selected_checks && "icd" %in% names(d)) {
    idx <- which(icd10_gm_shape_bad(d$icd))
    add_issue("cat6_7", pid_or_row[idx],
              "ICD length/shape out of range (ICD-10-GM 2025)",
              "Disallowed characters, too many dots, wrong stem/subpart length, or implausible total length.",
              "Medium")
  }
}

cat6_11_block <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_11" %in% selected_checks && "icd" %in% names(d)) {
    idx <- which(icd10_gm_unspecified(d$icd))
    add_issue("cat6_11", pid_or_row[idx],
              "Unspecific ICD (ICD-10-GM 2025)",
              "General/unspecified usage such as R99, Z00.*, or subcategories ending .9/.90/.99 (review clinical adequacy).",
              "Low")
  }
}

# OPS structure check (cat6_8) upgraded to OPS 2025
cat6_8_block <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_8" %in% selected_checks && "ops" %in% names(d)) {
    idx <- which(!ops_valid(d$ops))
    add_issue("cat6_8", pid_or_row[idx],
              "OPS invalid structure (OPS 2025)",
              "Expected: digit 1–9, hyphen, 2–3 digits, optional one or two dot groups of 1–3 alphanumerics.",
              "Medium")
  }
}

#  OPS near-miss suggestions piggybacked into cat6_8 (kept as “info”)
cat6_8_nearmiss_note <- function(d, selected_checks, pid_or_row, add_issue) {
  if ("cat6_8" %in% selected_checks && "ops" %in% names(d)) {
    idx_raw <- which(!ops_valid(d$ops))
    if (length(idx_raw)) {
      sug <- ops_near_miss_suggest(d$ops[idx_raw])
      keep <- which(!is.na(sug) & ops_valid(sug))
      if (length(keep)) {
        ix <- idx_raw[keep]
        add_issue("cat6_8", pid_or_row[ix],
                  "OPS potential typo (OPS 2025 near-miss)",
                  paste0("Original: ", d$ops[ix], " → Suggested: ", sug[keep]),
                  "Low")
      }
    }
  }
}

# ---- Server binder: call once at top of server(...) as modern_server(session) ---
modern_server <- function(input, output, session){
  # Dark mode toggler
  observeEvent(input$dark_mode, {
    session$sendCustomMessage('toggle-dark', isTRUE(input$dark_mode))
  })
  # Guided tour (targets are the sidebar menu links by tabName)
  observeEvent(input$btn_help_tour, ignoreInit = TRUE, {
    introjs(session, options = list(
      showStepNumbers = TRUE, exitOnOverlayClick = TRUE, nextLabel = 'Next', prevLabel = 'Back',
      steps = list(
        list(intro = 'Welcome! Quick tour of the dashboard. Use N/P keys to move across steps.'),
        list(element = 'a[data-value=\"tab_overview\"]',       intro = 'Overview: purpose & how to use.', position = 'right'),
        list(element = 'a[data-value=\"tab_load_data\"]',      intro = 'Step 1: Load your dataset.', position = 'right'),
        list(element = 'a[data-value=\"tab_map_columns\"]',    intro = 'Step 2: Map columns for checks.', position = 'right'),
        list(element = 'a[data-value=\"tab_custom_checks\"]',  intro = 'Step 3: Build custom rules.', position = 'right'),
        list(element = 'a[data-value=\"tab_checks\"]',         intro = 'Step 4: Select checks to run.', position = 'right'),
        list(element = 'a[data-value=\"tab_results\"]',        intro = 'Step 5: Review issues & download reports.', position = 'right'),
        list(element = 'a[data-value=\"tab_anonym\"]',         intro = 'Step 6: Compare anonymized vs original.', position = 'right')
      )
    ))
  })
  # Keyboard navigation
  observeEvent(input$`__key_next`, {
    order <- c('tab_overview','tab_load_data','tab_map_columns','tab_custom_checks','tab_checks','tab_results','tab_anonym')
    cur   <- isolate(input$tabs %||% 'tab_overview')
    i     <- match(cur, order)
    if (!is.na(i) && i < length(order)) updateTabItems(session, 'tabs', order[i+1])
  })
  observeEvent(input$`__key_prev`, {
    order <- c('tab_overview','tab_load_data','tab_map_columns','tab_custom_checks','tab_checks','tab_results','tab_anonym')
    cur   <- isolate(input$tabs %||% 'tab_overview')
    i     <- match(cur, order)
    if (!is.na(i) && i > 1) updateTabItems(session, 'tabs', order[i-1])
  })
}
# ======================= END MODERNIZATION PACK (drop-in) =======================

# ============================== APP FOOTER (drop-in) ==============================

# 1) Footer factory: picks native bs4Dash footer if available, else a portable HTML footer
app_footer <- local({
  have_bs4_footer <- ("bs4DashFooter" %in% getNamespaceExports("bs4Dash"))
  if (have_bs4_footer) {
    function() {
      bs4Dash::bs4DashFooter(
        left  = htmltools::HTML("&copy; 2025&nbsp;Gaetan Kamdje Wabo et&nbsp;al."),
        right = htmltools::tagList(
          htmltools::div("Mannheim Institute for Intelligent Systems in Medicine (MIISM)"),
          htmltools::div("Department of Biomedical Informatics"),
          htmltools::div("Medical Faculty of Mannheim, Heidelberg University")
        )
      )
    }
  } else {
    # Fallback footer rendered inside the body (sticky bottom)
    function() {
      htmltools::tags$footer(
        class = "app-footer",
        htmltools::div(
          class = "container-fluid d-flex justify-content-between flex-wrap gap-2 align-items-center",
          htmltools::div(htmltools::HTML("&copy; 2025&nbsp;Gaetan Kamdje Wabo et&nbsp;al.")),
          htmltools::div("Mannheim Institute for Intelligent Systems in Medicine (MIISM) · Department of Biomedical Informatics · Medical Faculty of Mannheim, Heidelberg University")
        )
      )
    }
  }
})

# 2) Minimal CSS (safe to include always). If the native footer is used, this is harmless.
footer_css <- htmltools::tags$style(htmltools::HTML("
  .app-footer{
    position: sticky; bottom: 0; width: 100%;
    z-index: 1029; padding: 10px 16px;
    background: #f8fafc; color: #334155;
    border-top: 1px solid rgba(0,0,0,.06);
    font-size: 0.875rem;
  }
  body.dark-mode .app-footer{
    background: #0b1220; color: #cbd5e1; border-top-color: rgba(255,255,255,.08);
  }
"))

# (Optional) If your floating help/dark-mode FAB overlaps the footer, lift it slightly:
# Add this to your existing CSS in `modern_ui()` OR uncomment below to apply globally.
footer_css <- htmltools::tagList(
  footer_css,
  htmltools::tags$style(htmltools::HTML(".fab-wrap{ bottom: 84px !important; }"))
)
# ============================ END APP FOOTER (drop-in) ============================


###############################################################################

###############################################################################
# A) PRODUCTION HARDENING HELPERS (safe DT, waiters, safe notifications)
###############################################################################
options(shiny.sanitize.errors = TRUE)  # do not leak errors to end users

safely_dt <- function(df, options = list(pageLength = 10, scrollX = TRUE)) {
  tryCatch(
    DT::datatable(df, options = options),
    error = function(e) {
      DT::datatable(
        data.frame(Message = paste("Render error:", e$message), check.names = FALSE),
        options = list(dom = "t")
      )
    }
  )
}

with_waiter <- function(expr, label = "Working…") {
  w <- waiter::Waiter$new(html = tagList(waiter::spin_fading_circles(), h4(label)))
  w$show()
  on.exit(w$hide(), add = TRUE)
  force(expr)
}

safe_notify <- function(msg, type = c("message","warning","error")) {
  type <- match.arg(type)
  try(showNotification(msg, type = type), silent = TRUE)
}

`%null%` <- function(x, y) if (is.null(x)) y else x

# ---------------------- GENDER STANDARDIZATION HELPERS ----------------------
parse_csv_values <- function(s) {
  if (is.null(s) || length(s) == 0) return(character(0))
  vals <- unlist(strsplit(s, ",", fixed = TRUE))
  vals <- tolower(trimws(as.character(vals)))
  unique(vals[nzchar(vals)])
}

standardize_gender_vec <- function(x, map) {
  sx <- tolower(trimws(as.character(x)))
  male_set   <- parse_csv_values(map$male  %||% "")
  female_set <- parse_csv_values(map$female %||% "")
  out <- sx
  if (length(male_set))   out[sx %in% male_set]   <- "male"
  if (length(female_set)) out[sx %in% female_set] <- "female"
  out
}

# ===================== KS helpers (DROP-IN replacement) =====================

# P-Wert robust formatieren (Unterlauf zeigen statt "0e+00")
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < .Machine$double.xmin) {
    paste0("< ", format(.Machine$double.xmin, scientific = TRUE))
  } else {
    format(p, digits = 17, scientific = TRUE, trim = TRUE)
  }
}

# Asymptotischer 2-Stichproben-KS-Test + Effektmaße
ks_test_asymp <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  # KS braucht Variabilität auf beiden Seiten
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(list(p.value = NA_real_, statistic = NA_real_, n_eff = NA_real_, z = NA_real_))
  }
  res <- tryCatch(stats::ks.test(x, y, exact = FALSE), error = function(e) NULL)
  if (is.null(res)) {
    return(list(p.value = NA_real_, statistic = NA_real_, n_eff = NA_real_, z = NA_real_))
  }
  D <- unname(res$statistic)
  n_eff <- length(x) * length(y) / (length(x) + length(y))
  z <- sqrt(n_eff) * D
  list(p.value = unname(res$p.value), statistic = D, n_eff = n_eff, z = z)
}

# Behalte API-Namen bei, damit bestehende Aufrufe weiter funktionieren
ks_pvalue_pair <- function(x, y) {
  ks_test_asymp(x, y)$p.value
}
# (fmt_sci darf bleiben, wird aber für KS nicht mehr verwendet)
# ============================================================================ 

# --- Chi-square & KS interpretation helpers ------------------------------------

# Cramér's V from a 2D contingency table
cramers_v_tbl <- function(tbl) {
  if (length(dim(tbl)) != 2L || any(dim(tbl) < 2)) {
    return(list(v = NA_real_, p = NA_real_, chi2 = NA_real_, df = NA_real_, n = sum(tbl)))
  }
  chi <- suppressWarnings(tryCatch(chisq.test(tbl, correct = FALSE), error = function(e) NULL))
  if (is.null(chi)) {
    return(list(v = NA_real_, p = NA_real_, chi2 = NA_real_, df = NA_real_, n = sum(tbl)))
  }
  n  <- sum(tbl)
  r  <- nrow(tbl)
  c  <- ncol(tbl)
  v  <- sqrt(as.numeric(chi$statistic) / (n * min(r - 1, c - 1)))
  list(v = as.numeric(v), p = as.numeric(chi$p.value), chi2 = as.numeric(chi$statistic),
       df = as.numeric(chi$parameter), n = n)
}

# Simple bands for effect sizes (used in interpretation box)
ks_band <- function(D) {
  if (is.na(D)) return("n/a")
  if (D < 0.05) "small" else if (D < 0.10) "noticeable" else "substantial"
}
cramer_band <- function(v) {
  if (is.na(v)) return("n/a")
  if (v < 0.10) "small" else if (v < 0.30) "medium" else if (v < 0.50) "large" else "very large"
}
###############################################################################


###############################################################################
# B) LONG, DESCRIPTIVE LABELS FOR EVERY CHECK
#    - concise "what it flags" + explicit required columns
#    - used by Step 4 to show HTML labels
###############################################################################
CHECK_LOGIC <- list(
  # ---------------------------- CAT 1: Completeness ---------------------------
  cat1_1  = list(what="Admission present but ICD missing/empty.", needs=c("admission_date","icd")),
  cat1_2  = list(what="Anamnese mentions surgery ('surg|chirurg'); OPS missing. Warn only if >10% missing in those rows.", needs=c("anamnese","ops")),
  cat1_3  = list(what="Anamnese mentions diabetes ('diab'); ICD does not match ^E1[0-4].", needs=c("anamnese","icd")),
  cat1_4  = list(what="Anamnese mentions heart terms ('heart|herz|cardio'); ICD does not start with I.", needs=c("anamnese","icd")),
  cat1_5  = list(what="Anamnese mentions chemotherapy; OPS missing.", needs=c("anamnese","ops")),
  cat1_6  = list(what="Anamnese mentions COPD; ICD does not start with J44.", needs=c("anamnese","icd")),
  cat1_7  = list(what="Anamnese mentions radiology ('radiol|röntgen'); OPS missing.", needs=c("anamnese","ops")),
  cat1_8  = list(what="Anamnese mentions allergy; ICD does not start with T78.", needs=c("anamnese","icd")),
  cat1_9  = list(what="Anamnese mentions dialysis; OPS missing.", needs=c("anamnese","ops")),
  cat1_10 = list(what="Anamnese mentions hypertension; ICD does not start with I10.", needs=c("anamnese","icd")),
  cat1_11 = list(what="Anamnese mentions endoscopy; OPS missing.", needs=c("anamnese","ops")),
  cat1_12 = list(what="Anamnese mentions stroke; ICD not I63/I64.", needs=c("anamnese","icd")),
  cat1_13 = list(what="Anamnese mentions infection; ICD does not start with B.", needs=c("anamnese","icd")),
  cat1_14 = list(what="Anamnese mentions prosthesis; OPS missing.", needs=c("anamnese","ops")),
  cat1_15 = list(what="Anamnese mentions depression; ICD not F32–F33.", needs=c("anamnese","icd")),
  cat1_16 = list(what="Admission present but both ICD and OPS missing.", needs=c("admission_date","icd","ops")),
  # ------------------------ CAT 2: Age-based plausibility ---------------------
  cat2_1  = list(what="ICD C61 (prostate cancer) in age < 15.", needs=c("age","icd")),
  cat2_2  = list(what="ICD F00/G30 (Alzheimer) in age < 30.", needs=c("age","icd")),
  cat2_3  = list(what="ICD F80–F89 (child dev. disorders) in age > 70.", needs=c("age","icd")),
  cat2_4  = list(what="ICD M80 (osteoporosis) in age < 18.", needs=c("age","icd")),
  cat2_5  = list(what="ICD B05 (measles) in age > 60.", needs=c("age","icd")),
  cat2_6  = list(what="ICD O60–O75 (birth-related) coded for male.", needs=c("gender","icd")),
  cat2_7  = list(what="ICD N95 (menopause) coded for male.", needs=c("gender","icd")),
  cat2_8  = list(what="ICD L70 (teen acne) in age < 1.", needs=c("age","icd")),
  cat2_9  = list(what="ICD H35.3 (macular degeneration) in age < 30.", needs=c("age","icd")),
  cat2_10 = list(what="ICD G80 (infantile CP) in adults (age > 21).", needs=c("age","icd")),
  cat2_11 = list(what="ICD O14 (preeclampsia) coded for male.", needs=c("gender","icd")),
  cat2_12 = list(what="ICD M08 (juvenile arthritis) in age > 70.", needs=c("age","icd")),
  cat2_13 = list(what="ICD E29 (testosterone deficiency) coded for female.", needs=c("gender","icd")),
  cat2_14 = list(what="ICD C62 (testicular tumor) coded for female.", needs=c("gender","icd")),
  cat2_15 = list(what="ICD E30.0 (delayed puberty) in age > 60.", needs=c("age","icd")),
  # ----------------------- CAT 3: Gender-based plausibility -------------------
  cat3_1  = list(what="ICD N83 (ovarian cyst) coded for male.", needs=c("gender","icd")),
  cat3_2  = list(what="ICD N41 (prostatitis) coded for female.", needs=c("gender","icd")),
  cat3_3  = list(what="Pregnancy codes (Oxx) coded for male.", needs=c("gender","icd")),
  cat3_4  = list(what="ICD C62 (testicular cancer) coded for female.", needs=c("gender","icd")),
  cat3_5  = list(what="ICD N80 (endometriosis) coded for male.", needs=c("gender","icd")),
  cat3_6  = list(what="ICD N52 (erectile dysfunction) coded for female.", needs=c("gender","icd")),
  cat3_7  = list(what="ICD C53 (cervical cancer) coded for male.", needs=c("gender","icd")),
  cat3_8  = list(what="ICD E28.1 (testosterone excess) coded for female.", needs=c("gender","icd")),
  cat3_9  = list(what="ICD N92/N93 (menstrual disorders) coded for male.", needs=c("gender","icd")),
  cat3_10 = list(what="ICD C50 (breast cancer) coded for male (rare; review).", needs=c("gender","icd")),
  cat3_11 = list(what="ICD N47 (phimosis) coded for female.", needs=c("gender","icd")),
  cat3_12 = list(what="ICD N76 (vulvitis) coded for male.", needs=c("gender","icd")),
  cat3_13 = list(what="ICD Pxx (birth injuries) flagged in male baby (naïve).", needs=c("gender","icd")),
  cat3_14 = list(what="ICD Q53 (cryptorchidism) coded for female.", needs=c("gender","icd")),
  cat3_15 = list(what="ICD O21 (hyperemesis gravidarum) coded for male.", needs=c("gender","icd")),
  # ----------------------------- CAT 4: Temporal ------------------------------
  cat4_1  = list(what="(Placeholder) Procedure date < admission.", needs=c("ops","admission_date")),
  cat4_2  = list(what="Discharge date is before admission date.", needs=c("admission_date","discharge_date")),
  cat4_3  = list(what="(Placeholder) Diagnosis date after discharge.", needs=c("icd","discharge_date")),
  cat4_4  = list(what="Duplicate same-day admissions per patient.", needs=c("patient_id","admission_date")),
  cat4_5  = list(what="(Placeholder) Impossible procedure date (non-leap Feb 29).", needs=c("ops")),
  cat4_6  = list(what="Admission date lies in the future.", needs=c("admission_date")),
  cat4_7  = list(what="(Placeholder) Overlapping stays without transfer.", needs=c("admission_date","discharge_date","patient_id")),
  cat4_8  = list(what="Same-day discharge with OPS present (potentially unrealistic).", needs=c("admission_date","discharge_date","ops")),
  cat4_9  = list(what="(Placeholder) Diagnosis coded months after discharge.", needs=c("icd","discharge_date")),
  cat4_10 = list(what="(Placeholder) Procedure on public holiday.", needs=c("ops")),
  cat4_11 = list(what="(Placeholder) Anamnese references future date.", needs=c("anamnese","discharge_date")),
  cat4_12 = list(what="Admission date is before birth date.", needs=c("admission_date","birth_date")),
  cat4_13 = list(what="(Placeholder) Multiple OPS with identical timestamps.", needs=c("ops")),
  cat4_14 = list(what="(Placeholder) Missing procedure time.", needs=c("ops")),
  cat4_15 = list(what="(Duplicate of cat4_2) Discharge < admission.", needs=c("admission_date","discharge_date")),
  # -------------------- CAT 5: Diagnosis–Procedure consistency ----------------
  cat5_1  = list(what="Appendectomy OPS but no ICD K35 (appendicitis).", needs=c("ops","icd")),
  cat5_2  = list(what="Knee replacement OPS but no ICD M17 (knee arthrosis).", needs=c("ops","icd")),
  cat5_3  = list(what="Chemotherapy OPS but no cancer/neoplasm ICD (C or D0–D4).", needs=c("ops","icd")),
  cat5_4  = list(what="Heart catheter OPS but no cardiology ICD (Ixx).", needs=c("ops","icd")),
  cat5_5  = list(what="Dialysis OPS but no ICD N18 (CKD).", needs=c("ops","icd")),
  cat5_6  = list(what="C-section OPS coded for male.", needs=c("ops","gender")),
  cat5_7  = list(what="Cataract OPS but no ICD H25/H26.", needs=c("ops","icd")),
  cat5_8  = list(what="Gastric bypass OPS but no ICD E66 (obesity).", needs=c("ops","icd")),
  cat5_9  = list(what="Hysterectomy OPS but missing/implausible GYN ICD.", needs=c("ops","icd")),
  cat5_10 = list(what="Transfusion OPS but no anemia ICD (D50–D64).", needs=c("ops","icd")),
  cat5_11 = list(what="Knee arthroscopy OPS but no knee ICD (S83/M23/M17).", needs=c("ops","icd")),
  cat5_12 = list(what="Radiology OPS but missing reason ICD.", needs=c("ops","icd")),
  cat5_13 = list(what="Skin graft OPS but no wound/burn ICD (T2*/S0*/burn).", needs=c("ops","icd")),
  cat5_14 = list(what="Upper GI endoscopy OPS but no GI ICD (Kxx).", needs=c("ops","icd")),
  cat5_15 = list(what="Pacemaker OPS but no arrhythmia ICD (I44–I49).", needs=c("ops","icd")),
  # --------------------------- CAT 6: Code integrity --------------------------
  cat6_1  = list(what="ICD does not match ^[A-Z]\\d{2}(\\.\\d{1,2})?$", needs=c("icd")),
  cat6_2  = list(what="OPS appears retired (heuristic).", needs=c("ops")),
  cat6_3  = list(what="ICD potential typo (fails same pattern as cat6_1).", needs=c("icd")),
  cat6_4  = list(what="Likely ICD-9 usage (numeric forms).", needs=c("icd")),
  cat6_5  = list(what="Placeholder/fake ICD (xxx, zzz, 'fake').", needs=c("icd")),
  cat6_6  = list(what="Numeric ICD-9 style code in ICD-10 environment.", needs=c("icd")),
  cat6_7  = list(what="ICD length/shape out of range.", needs=c("icd")),
  cat6_8  = list(what="OPS invalid structure ^\\d-\\d\\d(\\.\\d{1,2})?$", needs=c("ops")),
  cat6_9  = list(what="Foreign code system marker 'z9' (naïve).", needs=c("icd","ops")),
  cat6_10 = list(what="Diagnosis code string appears in OPS column (mismatch).", needs=c("ops")),
  cat6_11 = list(what="Unspecific ICD (R99, Z00).", needs=c("icd")),
  cat6_12 = list(what="(Placeholder) ICD not matching facility specialty.", needs=c("icd")),
  cat6_13 = list(what="ICD tagged with 'old_' (outdated).", needs=c("icd")),
  cat6_14 = list(what="'veraltet' / 'icd9' markers (outdated).", needs=c("icd")),
  cat6_15 = list(what="OPS indicates external facility procedure.", needs=c("ops"))
)

label_for_check <- function(id, description, category) {
  meta <- CHECK_LOGIC[[id]]
  if (is.null(meta)) {
    return(HTML(
      sprintf("<div><b>%s</b><br><small>%s — Generic category rule. See documentation.</small></div>",
              htmltools::htmlEscape(description), htmltools::htmlEscape(category))
    ))
  }
  needs <- paste(meta$needs, collapse = ", ")
  HTML(
    sprintf(
      "<div><b>%s</b><br><small>%s<br><i>Requires:</i> %s</small></div>",
      htmltools::htmlEscape(description),
      htmltools::htmlEscape(meta$what),
      htmltools::htmlEscape(needs)
    )
  )
}


###############################################################################
# C) REPLACE Step 4 CHECK UI: use long labels + keep cards expanded
#    (Replace your existing observeEvent(rv$checks_available, ...) block)
###############################################################################
build_checks_ui <- function(checks_def) {
  catnames <- unique(checks_def$category)
  lapply(catnames, function(catname) {
    cat_checks <- subset(checks_def, category == catname)
    bs4Card(
      width = 12,
      title = catname,
      closable = FALSE,
      status = "secondary",
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      !!!lapply(seq_len(nrow(cat_checks)), function(i) {
        rowi <- cat_checks[i, ]
        switch_id <- paste0("check_switch_", rowi$check_id)
        fluidRow(
          column(
            12,
            checkboxInput(
              inputId = switch_id,
              label   = label_for_check(rowi$check_id, rowi$description, rowi$category),
              value   = TRUE,
              width   = "100%"
            )
          )
        )
      })
    )
  })
}


###############################################################################
# D) ADD “Go to Step 6” BUTTON HANDLER (server side)
###############################################################################
go_to_step6 <- function(session) updateTabItems(session, "tabs", "tab_anonym")


###############################################################################
# E) PATCHES: wrap heavy operations with waiters to avoid UI freezes/crashes
###############################################################################
wrap_load_data <- function(code) with_waiter(code, "Loading data…")
wrap_run_checks <- function(code) with_waiter(code, "Running checks…")
wrap_load_anon <- function(code) with_waiter(code, "Loading anonymized data…")


###############################################################################


###############################################################################
# GUIDANCE FOR CLINICIANS / MEDICAL DOCTORS:
#
# - In Steps 1–5, you can load data, map columns, define or pick checks, and see
#   the results (which highlight potential data quality issues).
# - In Step 6, you can load a separate anonymized version of the same data
#   structure and get metrics to see how anonymization might distort
#   distributions or remove essential info (gender shift, age suppression, etc.).
###############################################################################

ui <- bs4DashPage(
  title = "DQ DualSight",
  footer  = app_footer(), 
  header = bs4DashNavbar(
    title = tags$span(
      style="font-size:1.2em; font-weight:bold;",
      "DQ DualSight Dashboard"
    )
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "Navigation",
    brandColor = "primary",
    
    bs4SidebarMenu(
      id = "tabs",  # <-- new: stable ID for programmatic tab switching
      bs4SidebarMenuItem("Overview", tabName="tab_overview", icon=icon("info-circle")),
      bs4SidebarMenuItem("1. Load Data", tabName="tab_load_data", icon=icon("upload")),
      bs4SidebarMenuItem("2. Map Columns", tabName="tab_map_columns", icon=icon("project-diagram")),
      bs4SidebarMenuItem("3. Define Custom Checks", tabName="tab_custom_checks", icon=icon("plus-circle")),
      bs4SidebarMenuItem("4. Select Checks", tabName="tab_checks", icon=icon("check-square")),
      bs4SidebarMenuItem("5. Results & Data Fitness", tabName="tab_results", icon=icon("chart-bar")),
      bs4SidebarMenuItem("6. Anonymization Metrics", tabName="tab_anonym", icon=icon("user-secret"))
    )
  ),
  
  body = bs4DashBody(
    useShinyjs(), 
    modern_ui(),
    footer_css, 
    tabItems(
      
      # OVERVIEW
      tabItem(
        tabName = "tab_overview",
        fluidRow(
          bs4Card(
            width = 12,
            title = "Welcome to DQ DualSight - An intuitive Tool for Data Quality Assessment and Anonymization Impact Evaluation",
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            
            h4("What this app does (in one minute)"),
            tags$ul(
              tags$li(tags$b("Finds quality issues"), " in extracted hospital data before handing them over for research (ICD-10, OPS, dates, age/sex plausibility, etc.)."),
              tags$li(tags$b("Shows what’s affected"), " and how many records each issue touches, so you can prioritise fixes."),
              tags$li(tags$b("(Optional) Compares anonymized data"), " to see if utility or data fitness was harmed (via ad-hoc comparisons of original and anonymized data elements).")
            ),
            
            h4("Who is this for?"),
            p("Clinicians, study nurses, data managers, medical informatics teams—", 
              "including non-technical users. If you can upload a file and pick columns, you can use this."),
            
            h4("What you need"),
            tags$ul(
              tags$li("A table with typical columns such as patient_id, gender, age/birth_date, admission/discharge, ICD, OPS, notes."),
              tags$li("CSV, Excel, or JSON are supported.")
            ),
            
            h4("How to use (quick workflow)"),
            tags$ol(
              tags$li(tags$b("Step 1 – Load data:"), " upload your file and preview the first rows."),
              tags$li(tags$b("Step 2 – Map columns:"), " tell the app which column is ICD, OPS, gender, dates, etc."),
              tags$li(tags$b("Step 3 – Data Use-specific DQ-rules:"), " build your own logic like “age > 120” or “notes text contains ‘stroke’ but no I63/I64”."),
              tags$li(tags$b("Step 4 – Pick checks:"), " toggle built-in checks by topic (completeness, plausibility, temporal, etc.)."),
              tags$li(tags$b("Step 5 – See results:"), " review counts, severity, and download a CSV report."),
              tags$li(tags$b("Step 6 – Anonymization (optional):"), 
                      " load an anonymized version of your data file and run ad-hoc column comparisons to judge data fitness.")
            ),
            
            h4("Concrete example (typical hospital cohort)"),
            tags$ul(
              tags$li(tags$b("Goal:"), " Identify questionable codes in a cardiology cohort."),
              tags$li(tags$b("Data:"), " Columns like ", code("patient_id, gender, age, admission_date, discharge_date, icd, ops, anamnese")),
              tags$li(tags$b("What you’ll see:"), " e.g., ",
                      "“Pregnancy code in male”, “Discharge before admission”, “Pacemaker without I44–I49”, ",
                      "“Chemo OPS without cancer ICD”, “Stroke mentioned in notes but no I63/I64”.")
            ),
            
            h4("Tip"),
            p("Use the keyboard: ", code("N"), " next tab, ", code("P"), " previous, ", code("?"), " guided tour, ", code("G"), " dark mode.")
          )
        )
      ),
      
      # STEP 1: LOAD DATA
      tabItem(
        tabName = "tab_load_data",
        fluidRow(
          bs4Card(
            width = 4,
            title = "Step 1: Load Data",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            
            radioButtons(
              inputId = "file_type",
              label   = "Select file type:",
              choices = c("CSV/TXT","Excel","JSON","FHIR"),
              selected = "CSV/TXT"
            ),
            
            conditionalPanel(
              condition = "input.file_type=='CSV/TXT'",
              fileInput("file_csv","Upload CSV/TXT:", accept=c(".csv",".txt")),
              checkboxInput("header_csv","Header in first row?", TRUE),
              radioButtons("sep_csv","Separator", 
                           choices = c("Comma"=",", "Semicolon"=";", "Tab"="\t"),
                           selected = ",")
            ),
            
            conditionalPanel(
              condition = "input.file_type=='Excel'",
              fileInput("file_excel","Upload Excel:", accept=c(".xlsx",".xls")),
              numericInput("excel_sheet","Sheet number:", value=1, min=1)
            ),
            
            conditionalPanel(
              condition = "input.file_type=='JSON'",
              fileInput("file_json","Upload JSON:", accept=c(".json"))
            ),
            
            conditionalPanel(
              condition = "input.file_type=='FHIR'",
              fileInput("file_fhir","Upload FHIR (JSON/XML):", accept=c(".json",".xml")),
              helpText("Demo only; real usage would parse FHIR properly.")
            ),
            
            # IMPORTANT: This button ONLY loads and previews; it will NOT navigate.
            actionButton("btn_load_data", "Load Data", class="btn btn-primary"),
            
            tags$hr(),
            # NEW: user-controlled navigation to Step 2
            actionButton("btn_next_to_mapping", "Continue to Step 2: Map Columns",
                         class="btn btn-success", width = "100%")
          ),
          
          bs4Card(
            width = 8,
            title = "Preview of Loaded Data",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("data_preview")
          )
        )
      ),
      
      # STEP 2: MAP COLUMNS
      tabItem(
        tabName="tab_map_columns",
        fluidRow(
          bs4Card(
            width=12,
            title="Step 2: Map Your Columns",
            status="primary",
            solidHeader=TRUE,
            collapsible=FALSE,
            
            p("Match your dataset’s columns to the standard fields recognized by the checks. 
               If a field doesn't exist, select 'None'. Proper mapping ensures accurate checks."),
            
            uiOutput("ui_column_mapping"),
            br(),
            actionButton("btn_save_mapping","Save Mapping & Continue", class="btn btn-success")
          )
        )
      ),
      
      # STEP 3: DEFINE CUSTOM CHECKS
      tabItem(
        tabName="tab_custom_checks",
        fluidRow(
          bs4Card(
            width=12,
            title="Step 3: Define Custom Checks (Specific to your data use)",
            status="info",
            solidHeader=TRUE,
            collapsible=FALSE,
            
            p("Add your own data quality rules with AND/OR conditions. 
               Click 'Generate Expression' to preview the logic, name/describe it, 
               then add it to your checks list."),
            
            fluidRow(
              column(3, selectInput("rule_column","Column:", choices=c("Please load & map data first"))),
              column(3, selectInput(
                "rule_operator","Operator:",
                choices = c("==","!=","<",">","<=",">=",
                            "contains","not contains",
                            "starts_with","not starts_with",
                            "ends_with","not ends_with")
              )
              ),
              column(3,
                     radioButtons("rule_value_type","Compare to:",
                                  choices=c("Literal Value"="literal","Another Column"="column")),
                     uiOutput("ui_rule_value_input")
              ),
              column(3, selectInput("rule_combine","Combine with:",choices=c("AND","OR")))
            ),
            
            fluidRow(
              column(12,
                     actionButton("btn_add_condition","Add Condition", class="btn btn-primary"),
                     br(),br(),
                     DTOutput("conditions_table"),
                     br(),
                     actionButton("btn_generate_expression","Generate Expression", class="btn btn-info"),
                     br(),br(),
                     textAreaInput("visual_expression","Generated R Expression:","",rows=2)
              )
            ),
            
            hr(),
            
            fluidRow(
              column(4, textInput("custom_check_name","Check Name:","")),
              column(4, selectInput("custom_check_severity","Severity:",
                                    choices=c("Low","Medium","High","Critical"))),
              column(4, textInput("custom_check_description","Short Description:",""))
            ),
            
            fluidRow(
              column(12,
                     actionButton("btn_add_custom_check","Add Custom Check", class="btn btn-success"),
                     br(),br(),
                     h4("Currently Defined Custom Checks:"),
                     DTOutput("custom_checks_table")
              )
            ),
            
            br(),
            actionButton("btn_go_step_4","Proceed to Step 4", class="btn btn-success")
          )
        )
      ),
      
      # STEP 4: SELECT CHECKS
      tabItem(
        tabName="tab_checks",
        div(id = "step4_scope",  
            fluidRow(
              bs4Card(
                width=12,
                title="Step 4: Select Checks",
                status="primary",
                solidHeader=TRUE,
                collapsible=FALSE,
                
                p("All checks are grouped by category. Expand each group to see 
               individual checks, then check or uncheck. Custom checks appear at the bottom."),
                
                # ---- helper buttons ------------------------------------------------
                fluidRow(
                  column(6, actionButton("btn_select_all_checks",   "Select all",
                                         icon = icon("check-square"),
                                         class = "btn btn-info btn-block")),
                  column(6, actionButton("btn_deselect_all_checks", "Deselect all",
                                         icon = icon("square"),
                                         class = "btn btn-info btn-block"))
                ),
                br(),
                # ---- end helper buttons --------------------------------------------
                
                uiOutput("ui_built_in_checks"),
                hr(),
                h4("Custom Checks:"),
                uiOutput("ui_custom_checks"),
                
                br(),
                actionButton("btn_run_checks","Run Selected Checks & View Results", class="btn btn-success")
              )
            )
        )
      ),
      
      # STEP 5: RESULTS
      tabItem(
        tabName="tab_results",
        fluidRow(
          bs4ValueBox(
            width=4,
            value=textOutput("vb_total_checks"),
            subtitle="Checks Performed",
            icon=icon("tasks"),
            color="primary"
          ),
          bs4ValueBox(
            width=4,
            value=textOutput("vb_total_issues"),
            subtitle="Total Issues Found",
            icon=icon("exclamation-triangle"),
            color="danger"
          ),
          bs4ValueBox(
            width=4,
            value=textOutput("vb_records_affected"),
            subtitle="Records Affected",
            icon=icon("user-injured"),
            color="warning"
          )
        ),
        
        fluidRow(
          bs4Card(
            width=6,
            title="Results Overview",
            status="primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            DTOutput("check_results_table")
          ),
          bs4Card(
            width=6,
            title="Severity Distribution",
            status="primary",
            solidHeader=TRUE,
            collapsible=TRUE,
            plotOutput("summary_plot")
          )
        ),
        
        fluidRow(
          bs4Card(
            width=6,
            title="Category Distribution",
            status="info",
            solidHeader=TRUE,
            collapsible=TRUE,
            plotOutput("category_plot")
          ),
          bs4Card(
            width=6,
            title="Detailed Issues by Check",
            status="warning",
            solidHeader=TRUE,
            collapsible=TRUE,
            
            selectInput("detail_check_select","Select a check for details:",choices=c(),width="100%"),
            DTOutput("detailed_issues_table"),
            br(),
            downloadButton("download_full_report","Download Full Report")
          )
        ),
        
        fluidRow(
          bs4Card(
            width = 12,
            title = "Analysis Options",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            p("Re-run checks if you updated mapping/custom checks, or reset to start over. 
				You can also jump to Step 6 to evaluate anonymization impact."),
            div(
              class = "d-flex gap-2",
              actionButton("btn_update_analysis","Update Analysis (Re-check)", class="btn btn-primary"),
              actionButton("btn_reset_analysis","Reset Entire Analysis", class="btn btn-danger"),
              actionButton("btn_go_step6","Go to Step 6: Anonymization Impact", class="btn btn-success")
            )
          )
        )
      ),
      
      # STEP 6: ANONYMIZATION METRICS & EXTENDED ANALYSIS
      tabItem(
        tabName = "tab_anonym",
        
        # Load anonymized data
        fluidRow(
          bs4Card(
            width = 12,
            title = "Step 6: Data Anonymization Metrics",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            
            p("Upload an anonymized version of your dataset to compare distributions and 
				 suppression. This helps check if anonymization is distorting crucial 
				 aspects (gender distribution, ages, etc.)."),
            
            p("This mirrors the Step 1 loader to avoid format issues."),
            
            radioButtons(
              inputId = "anon_file_type",
              label   = "Select anonymized file type:",
              choices = c("CSV/TXT","Excel","JSON","FHIR"),
              selected = "CSV/TXT"
            ),
            
            conditionalPanel(
              condition = "input.anon_file_type=='CSV/TXT'",
              fileInput("file_anonym_csv","Upload CSV/TXT:", accept=c(".csv",".txt")),
              checkboxInput("anon_header_csv","Header in first row?", TRUE),
              radioButtons("anon_sep_csv","Separator",
                           choices = c("Comma"=",", "Semicolon"=";", "Tab"="\t"),
                           selected = ",")
            ),
            
            conditionalPanel(
              condition = "input.anon_file_type=='Excel'",
              fileInput("file_anonym_excel","Upload Excel:", accept=c(".xlsx",".xls")),
              numericInput("anon_excel_sheet","Sheet number:", value=1, min=1)
            ),
            
            conditionalPanel(
              condition = "input.anon_file_type=='JSON'",
              fileInput("file_anonym_json","Upload JSON:", accept=c(".json"))
            ),
            
            conditionalPanel(
              condition = "input.anon_file_type=='FHIR'",
              fileInput("file_anonym_fhir","Upload FHIR (JSON/XML):", accept=c(".json",".xml")),
              helpText("Demo only; real usage would parse FHIR resources.")
            ),
            
            actionButton("btn_load_anonym","Load Anonymized Data & Compare", class="btn btn-info"),
            br(), br()
            
          )
        ),
        
        # View anonymized data
        fluidRow(
          bs4Card(
            width=12,
            title="View Anonymized Data",
            status="secondary",
            solidHeader=TRUE,
            collapsible=TRUE,
            DTOutput("anon_data_view")
          )
        ),
        
        # Column presence comparison
        fluidRow(
          bs4Card(
            width=12,
            title="Column Comparison: Original vs. Anonymized",
            status="info",
            solidHeader=TRUE,
            collapsible=TRUE,
            DTOutput("column_comparison")
          )
        ),
        
        # ----------  Ad-hoc Column Comparison (WITH download + recommendations) ----------
        fluidRow(
          bs4Card(
            width      = 12,
            title      = "Ad-hoc Column Comparison",
            status     = "warning",
            solidHeader= TRUE,
            collapsible= TRUE,
            
            p("Pick any column from the original dataset and any column from the ",
              "anonymized dataset. The app chooses the correct metric set ",
              "(numeric vs. categorical) automatically."),
            
            fluidRow(
              column(4, selectInput("compare_orig_col",
                                    "Column in ORIGINAL data:",
                                    choices = c(), width = "100%")),
              column(4, selectInput("compare_anon_col",
                                    "Column in ANONYMIZED data:",
                                    choices = c(), width = "100%")),
              column(4, 
                     br(),
                     actionButton("btn_compare_cols",
                                  "Run comparison",
                                  class = "btn btn-primary", width = "100%"))
            ),
            
            br(),
            htmlOutput("ks_exact_value"),
            br(),
            
            br(),
            DTOutput("adhoc_compare_table"),
            br(),
            h4("Short interpretation"),
            htmlOutput("adhoc_interpret_box"),
            br(),
            
            # NEW: Download button for the comparison result
            downloadButton("download_adhoc_csv", "Download comparison as CSV"),
            br(), br(),
            
            # NEW: Dynamic recommendations output
            h4("Recommendations: Data Fitness & Next Actions"),
            htmlOutput("adhoc_reco")
          )
        )
        # ---------------------------------------------------------------------
      )
    )
  )
)

server <- function(input, output, session){
  
  modern_server(input, output, session)
  
  make_checks_def <- function() {
    data.frame(
      check_id = c(
        # Category 1
        "cat1_1","cat1_2","cat1_3","cat1_4","cat1_5","cat1_6","cat1_7","cat1_8",
        "cat1_9","cat1_10","cat1_11","cat1_12","cat1_13","cat1_14","cat1_15","cat1_16",
        # Category 2
        "cat2_1","cat2_2","cat2_3","cat2_4","cat2_5","cat2_6","cat2_7","cat2_8","cat2_9",
        "cat2_10","cat2_11","cat2_12","cat2_13","cat2_14","cat2_15",
        # Category 3
        "cat3_1","cat3_2","cat3_3","cat3_4","cat3_5","cat3_6","cat3_7","cat3_8","cat3_9",
        "cat3_10","cat3_11","cat3_12","cat3_13","cat3_14","cat3_15",
        # Category 4
        "cat4_1","cat4_2","cat4_3","cat4_4","cat4_5","cat4_6","cat4_7","cat4_8","cat4_9",
        "cat4_10","cat4_11","cat4_12","cat4_13","cat4_14","cat4_15",
        # Category 5
        "cat5_1","cat5_2","cat5_3","cat5_4","cat5_5","cat5_6","cat5_7","cat5_8","cat5_9",
        "cat5_10","cat5_11","cat5_12","cat5_13","cat5_14","cat5_15",
        # Category 6
        "cat6_1","cat6_2","cat6_3","cat6_4","cat6_5","cat6_6","cat6_7","cat6_8","cat6_9",
        "cat6_10","cat6_11","cat6_12","cat6_13","cat6_14","cat6_15"
      ),
      category = c(
        rep("Completeness of Coding",16),
        rep("Age-based Plausibility",15),
        rep("Gender-based Plausibility",15),
        rep("Temporal Consistency",15),
        rep("Diagnosis–Procedure Consistency",15),
        rep("Code Integrity (ICD/OPS)",15)
      ),
      description = c(
        # cat1
        "No ICD for admitted patient",
        "OPS missing for >10% 'surgery'",
        "Diabetes mention but no E10-E14",
        "Heart disease mention but no Ixx",
        "Chemotherapy mention but no OPS code",
        "COPD mention but no J44",
        "Radiology mention but no OPS",
        "Allergy mention but no T78",
        "Dialysis mention but no OPS",
        "Hypertension mention but no I10",
        "Endoscopy mention but no OPS",
        "Stroke mention but no I63/I64",
        "Infection mention but no Bxx",
        "Prosthesis mention but no OPS",
        "Depression mention but no F32-F33",
        "Admission but no ICD/OPS at all",
        # cat2
        "Prostate cancer <15y",
        "Alzheimer <30y",
        "Child dev. disorder 70+",
        "Osteoporosis <18y",
        "Measles >60y",
        "Birth ICD in male",
        "Menopause ICD in male",
        "Teen acne in neonate",
        "Macular degeneration in <30y",
        "Infantile CP in adult",
        "Preeclampsia in male",
        "Juvenile arthritis in elderly",
        "Testosterone deficiency in female",
        "Testicular tumor in female",
        "Delayed puberty in 60+",
        # cat3
        "Ovarian cyst in male",
        "Prostatitis in female",
        "Pregnancy in male",
        "Testicular cancer in female",
        "Endometriosis in male",
        "Erectile dysfunction in female",
        "Cervical cancer in male",
        "Testosterone excess in female",
        "Menstrual disorder in male",
        "Breast cancer in male",
        "Phimosis in female",
        "Vulvitis in male",
        "Birth injuries (Pxx) in male baby",
        "Cryptorchidism in female",
        "Hyperemesis grav. in male",
        # cat4
        "Procedure date < admission (placeholder)",
        "Discharge < admission date",
        "Diagnosis date after discharge (placeholder)",
        "Multiple admissions same date/patient",
        "Impossible procedure date (non-leap Feb 29)",
        "Admission date in future",
        "Overlapping stays w/o transfer (placeholder)",
        "Same-day discharge for complex procedure",
        "Diagnosis coded months post discharge",
        "Procedure on holiday (placeholder)",
        "Anamnese references future date (placeholder)",
        "Admission < birth date",
        "Multiple OPS identical timestamp (placeholder)",
        "Missing procedure time (placeholder)",
        "Discharge < admission (duplicate check)",
        # cat5
        "Appendectomy w/o K35",
        "Knee replacement w/o M17",
        "Chemotherapy w/o cancer ICD",
        "Heart catheter w/o Ixx",
        "Dialysis w/o N18",
        "C-section in male",
        "Cataract op w/o H25/H26",
        "Gastric bypass w/o E66",
        "Hysterectomy w/o GYN ICD",
        "Transfusion w/o anemia ICD",
        "Knee arthroscopy w/o knee ICD",
        "Radiological OPS w/o ICD reason",
        "Skin graft w/o wound/burn ICD",
        "Upper GI endoscopy w/o Kxx",
        "Pacemaker w/o I44–I49",
        # cat6
        "ICD not matching ^[A-Z]\\d{2}(\\.\\d{1,2})?$",
        "OPS referencing retired code",
        "ICD potential typo mismatch",
        "Likely ICD-9 usage",
        "Fake ICD code (xxx,zzz)",
        "Numeric ICD-9 code in ICD-10 env",
        "ICD length out of range",
        "OPS invalid structure",
        "Foreign code system (Z9...)",
        "Diagnosis code in OPS col or vice versa",
        "Unspecific ICD (R99,Z00)",
        "ICD not matching facility specialty",
        "ICD marked 'old_' => outdated",
        "Veraltete ICD coding",
        "OPS for external facility procedure"
      ),
      enabled = TRUE,
      stringsAsFactors = FALSE
    )
  }
  
  ############################################################################
  # REACTIVE STORAGE
  ############################################################################
  rv <- reactiveValues(
    raw_data=NULL,
    colnames_all=NULL,
    mapping=list(),
    mapped_data=NULL,
    checks_available=NULL,
    issues=NULL,
    
    custom_checks=data.frame(
      check_id = character(),
      description = character(),
      expression_raw = character(),
      severity = character(),
      stringsAsFactors = FALSE
    ),
    
    conditions_list=data.frame(
      combine=character(),
      column=character(),
      operator=character(),
      value=character(),
      stringsAsFactors=FALSE
    ),
    
    anonym_data=NULL,  # We'll store the anonymized data here
    cat_card_ids = character(),
    adhoc_compare_df   = NULL,  # last comparison table (as shown to user)
    adhoc_compare_meta = NULL,   # small list with context to drive recommendations
    # >>> NEW: defaults for gender value mapping
    gender_map = list(
      male   = c("m", "male", "mann", "männlich", "1"),
      female = c("f", "female", "frau", "weiblich", "2")
    )
  )
  rv$checks_available <- make_checks_def()
  
  ############################################################################
  # STEP 1 - LOAD DATA
  ############################################################################
  observeEvent(input$btn_load_data,{
    wrap_load_data({
      req(input$file_type)
      rv$raw_data     <- NULL
      rv$colnames_all <- NULL
      
      if (input$file_type == "CSV/TXT") {
        req(input$file_csv)
        df <- tryCatch({
          data.table::fread(
            file = input$file_csv$datapath,
            header = input$header_csv,
            sep    = input$sep_csv,
            data.table = TRUE,
            showProgress = TRUE
          )
        }, error = function(e) NULL)
        
        if (is.null(df)) {
          showNotification("Error reading CSV/TXT file.", type="error")
          return()
        }
        rv$raw_data <- df
        
      } else if (input$file_type == "Excel") {
        req(input$file_excel)
        df <- tryCatch({
          readxl::read_excel(input$file_excel$datapath, sheet = input$excel_sheet)
        }, error = function(e) NULL)
        if (is.null(df)) {
          showNotification("Error reading Excel file.", type="error")
          return()
        }
        rv$raw_data <- as.data.frame(df)
        
      } else if (input$file_type == "JSON") {
        req(input$file_json)
        df <- tryCatch({
          read_json_tabular(input$file_json$datapath)
        }, error = function(e) {
          showNotification(paste("JSON parsing error:", e$message), type = "error")
          NULL
        })
        if (is.null(df)) return()
        rv$raw_data <- as.data.frame(df)
        
      } else if (input$file_type == "FHIR") {
        req(input$file_fhir)
        df <- tryCatch({
          read_fhir_tabular(input$file_fhir$datapath)  # expects FHIR JSON Bundle
        }, error = function(e) {
          showNotification(paste("FHIR parsing error:", e$message), type = "error")
          NULL
        })
        if (is.null(df)) return()
        rv$raw_data <- as.data.frame(df)
        
      }
      
      if (!is.null(rv$raw_data)) {
        rv$colnames_all <- colnames(rv$raw_data)
        output$data_preview <- renderDT({
          datatable(rv$raw_data, options = list(pageLength = 5, scrollX = TRUE))
        })
        showNotification(sprintf("Data loaded! %d rows.", nrow(rv$raw_data)), type = "message")
        # NOTE: do NOT navigate automatically anymore.
      }
    })
  })
  #here 
  ############################################################################
  # Moving to step 2
  observeEvent(input$btn_next_to_mapping, {
    if (is.null(rv$raw_data) || nrow(rv$raw_data) == 0) {
      showNotification("Please load a dataset first.", type = "warning")
    } else {
      updateTabItems(session, "tabs", "tab_map_columns")
    }
  })
  
  ############################################################################
  # STEP 2 - MAP COLUMNS
  ############################################################################
  auto_map_columns <- function(df_colnames){
    lc <- tolower(df_colnames)
    guess <- function(pat){
      hits <- grepl(pat, lc)
      if(any(hits)) df_colnames[hits][1] else NULL
    }
    list(
      patient_id     = guess("patientid|pat_id"),
      icd            = guess("^icd$"),
      ops            = guess("^ops$"),
      gender         = guess("gender|sex|geschlecht"),
      admission_date = guess("admission|aufnahme"),
      discharge_date = guess("discharge|entlassung"),
      age            = guess("^age$|^alter$"),
      birth_date     = guess("birth|geburt"),
      anamnese       = guess("anamnese|text|befund")
    )
  }
  
  output$ui_column_mapping <- renderUI({
    req(rv$colnames_all)
    if (length(rv$mapping) == 0) {
      rv$mapping <- auto_map_columns(rv$colnames_all)
    }
    
    cands <- c("None", rv$colnames_all)
    m <- rv$mapping
    
    # safe defaults for the textInputs
    male_defaults   <- paste((rv$gender_map$male   %||% character(0)), collapse = ", ")
    female_defaults <- paste((rv$gender_map$female %||% character(0)), collapse = ", ")
    
    tagList(
      fluidRow(
        column(3, selectInput("map_patient_id","Patient ID:",      cands, selected = m$patient_id     %||% "None")),
        column(3, selectInput("map_icd",       "ICD Code:",        cands, selected = m$icd            %||% "None")),
        column(3, selectInput("map_ops",       "OPS Code:",        cands, selected = m$ops            %||% "None")),
        column(3, selectInput("map_gender",    "Gender:",          cands, selected = m$gender         %||% "None"))
      ),
      fluidRow(
        column(3, selectInput("map_admission_date","Admission Date:", cands, selected = m$admission_date %||% "None")),
        column(3, selectInput("map_discharge_date","Discharge Date:", cands, selected = m$discharge_date %||% "None")),
        column(3, selectInput("map_age",          "Age:",             cands, selected = m$age            %||% "None")),
        column(3, selectInput("map_birth_date",   "Birth Date:",      cands, selected = m$birth_date     %||% "None"))
      ),
      fluidRow(
        column(3, selectInput("map_anamnese","Anamnesis / Notes:", cands, selected = m$anamnese %||% "None"))
      ),
      
      # ⬇️ put the gender mapping UI here so it's reactive
      conditionalPanel(
        condition = "input.map_gender && input.map_gender !== 'None'",
        tags$hr(),
        h5("Gender value mapping (standardize to male/female)"),
        helpText("Enter the raw values from your Gender column that mean male/female (comma-separated, case-insensitive)."),
        fluidRow(
          column(
            6,
            textInput(
              "map_gender_male_values",
              "Values meaning 'male':",
              value = male_defaults
            )
          ),
          column(
            6,
            textInput(
              "map_gender_female_values",
              "Values meaning 'female':",
              value = female_defaults
            )
          )
        )
      )
    )
  })
  
  
  observeEvent(input$btn_save_mapping,{
    if(is.null(rv$raw_data) || nrow(rv$raw_data)==0){
      showNotification("No data loaded. Please go back to Step 1.",type="error")
      return()
    }
    rv$mapping <- list(
      patient_id     = if(input$map_patient_id=="None") NULL else input$map_patient_id,
      icd            = if(input$map_icd=="None") NULL else input$map_icd,
      ops            = if(input$map_ops=="None") NULL else input$map_ops,
      gender         = if(input$map_gender=="None") NULL else input$map_gender,
      admission_date = if(input$map_admission_date=="None") NULL else input$map_admission_date,
      discharge_date = if(input$map_discharge_date=="None") NULL else input$map_discharge_date,
      age            = if(input$map_age=="None") NULL else input$map_age,
      birth_date     = if(input$map_birth_date=="None") NULL else input$map_birth_date,
      anamnese       = if(input$map_anamnese=="None") NULL else input$map_anamnese
    )
    
    # >>> NEW: persist the value-lists (kept OUTSIDE rv$mapping to avoid rename loops)
    if (!is.null(input$map_gender) && input$map_gender != "None") {
      rv$gender_map <- list(
        male   = parse_csv_values(input$map_gender_male_values %||% ""),
        female = parse_csv_values(input$map_gender_female_values %||% "")
      )
    }
    
    showNotification("Mapping saved. Moving to Step 3...", type="message")
    updateTabItems(session, "tabs", "tab_custom_checks")
  })
  
  ############################################################################
  # STEP 3 - DEFINE CUSTOM CHECKS
  ############################################################################
  output$ui_rule_value_input <- renderUI({
    if(input$rule_value_type=="literal"){
      textInput("rule_value_literal","Value:","")
    } else {
      req(rv$colnames_all)
      selectInput("rule_value_column","Compare to Column:", rv$colnames_all)
    }
  })
  
  observe({
    req(rv$colnames_all)
    updateSelectInput(session,"rule_column",choices=rv$colnames_all)
  })
  
  observeEvent(input$btn_add_condition,{
    req(input$rule_column, input$rule_operator)
    val_str <- NULL
    if(input$rule_value_type=="literal"){
      req(input$rule_value_literal)
      val_str <- input$rule_value_literal
    } else {
      req(input$rule_value_column)
      val_str <- paste0("::COL::",input$rule_value_column)
    }
    
    new_cond <- data.frame(
      combine=input$rule_combine,
      column=input$rule_column,
      operator=input$rule_operator,
      value=val_str,
      stringsAsFactors=FALSE
    )
    
    rv$conditions_list <- rbind(rv$conditions_list, new_cond)
  })
  
  output$conditions_table <- renderDT({
    if(nrow(rv$conditions_list)==0){
      datatable(data.frame(Message="No conditions yet."),options=list(dom='t'))
    } else {
      datatable(rv$conditions_list, options=list(pageLength=5,scrollX=TRUE))
    }
  })
  
  observeEvent(input$btn_generate_expression,{
    if(nrow(rv$conditions_list)==0){
      updateTextAreaInput(session,"visual_expression",value="")
      return()
    }
    
    build_str_op <- function(op, left, right_expr) {
      neg <- grepl("^not\\s", op)
      base <- sub("^not\\s+", "", op)
      fun <- switch(base,
                    "contains"     = "str_detect",
                    "starts_with"  = "str_starts",
                    "ends_with"    = "str_ends",
                    stop("Unsupported string operator")
      )
      paste0(if (neg) "!" else "", fun, "(", left, ", ", right_expr, ")")
    }
    
    expr_parts <- character()
    for(i in seq_len(nrow(rv$conditions_list))){
      rowi <- rv$conditions_list[i,]
      col_i <- rowi$column
      op_i  <- rowi$operator
      val_i <- rowi$value
      
      is_string_op <- op_i %in% c("contains","not contains","starts_with","not starts_with","ends_with","not ends_with")
      
      if(startsWith(val_i,"::COL::")){
        c2 <- sub("^::COL::","",val_i)
        if (is_string_op) {
          cond_str <- build_str_op(op_i, col_i, c2)
        } else {
          cond_str <- paste0(col_i," ",op_i," ",c2)
        }
      } else {
        numv <- suppressWarnings(as.numeric(val_i))
        if(!is.na(numv) && grepl("^[0-9.+-]+$", val_i)){
          vfinal <- val_i
        } else {
          vfinal <- paste0("'", val_i, "'")
        }
        if (is_string_op) {
          cond_str <- build_str_op(op_i, col_i, vfinal)
        } else {
          cond_str <- paste0(col_i," ",op_i," ",vfinal)
        }
      }
      
      if(i==1){
        expr_parts <- c(cond_str)
      } else {
        comb <- toupper(rowi$combine)
        if(comb=="OR"){
          expr_parts[length(expr_parts)] <- paste0("(", expr_parts[length(expr_parts)],") | (",cond_str,")")
        } else {
          expr_parts[length(expr_parts)] <- paste0("(", expr_parts[length(expr_parts)],") & (",cond_str,")")
        }
      }
    }
    
    final_expr <- expr_parts[length(expr_parts)]
    updateTextAreaInput(session,"visual_expression",value=final_expr)
  })
  
  
  observeEvent(input$btn_add_custom_check, {
    expr_raw <- input$visual_expression
    cname    <- input$custom_check_name
    sev      <- input$custom_check_severity
    desc     <- input$custom_check_description
    
    if(trimws(cname) == "" || trimws(expr_raw) == ""){
      showNotification("Need a check name and a valid expression.", type = "error")
      return()
    }
    cid <- paste0("custom_", gsub("\\s+", "_", tolower(cname)), "_", as.integer(Sys.time()))
    
    new_entry <- data.frame(
      check_id = cid,
      description = ifelse(trimws(desc) == "", cname, desc),
      expression_raw = expr_raw,
      severity = sev,
      stringsAsFactors = FALSE
    )
    rv$custom_checks <- rbind(rv$custom_checks, new_entry)
    
    # Clear inputs after adding
    rv$conditions_list <- rv$conditions_list[0,]
    updateTextAreaInput(session, "visual_expression", value = "")
    updateTextInput(session, "custom_check_name", value = "")
    updateTextInput(session, "custom_check_description", value = "")
    showNotification("Custom check added!", type = "message")
  })
  
  
  output$custom_checks_table <- renderDT({
    df <- rv$custom_checks
    if(nrow(df) == 0){
      datatable(data.frame(Message="No custom checks."), options=list(dom='t'))
    } else {
      df$Delete <- sapply(df$check_id, function(id){
        as.character(
          actionButton(
            inputId = paste0("del_custom_", id),
            label = "Delete",
            icon = icon("trash"),
            onclick = 'Shiny.setInputValue(\"last_deleted\",  this.id, {priority: "event"})'
          )
        )
      })
      datatable(
        df[, c("check_id", "description", "expression_raw", "severity", "Delete")],
        escape = FALSE,
        options = list(pageLength=5, scrollX=TRUE),
        rownames = FALSE
      )
    }
  }, server = FALSE)
  
  observeEvent(input$btn_go_step_4,{
    updateTabItems(session,"tabs","tab_checks")
  })
  
  ############################################################################
  # STEP 4: SELECT CHECKS/ UI: rebuild when catalog changes
  ############################################################################
  observeEvent(rv$checks_available, ignoreInit = FALSE, {
    req(rv$checks_available)
    output$ui_built_in_checks <- renderUI({
      do.call(tagList, build_checks_ui(rv$checks_available))
    })
  })
  
  ###############
  observeEvent(input$tabs, {
    if (identical(input$tabs, "tab_checks") &&
        (is.null(rv$checks_available) || nrow(rv$checks_available) == 0)) {
      rv$checks_available <- make_checks_def()
    }
  })
  #############
  observe({
    if (nrow(rv$custom_checks) == 0) {
      output$ui_custom_checks <- renderUI({
        tags$em("No custom checks defined.")
      })
    } else {
      all_custom_ui <- lapply(seq_len(nrow(rv$custom_checks)), function(i) {
        rowi <- rv$custom_checks[i, ]
        switch_id <- paste0("check_switch_custom_", rowi$check_id)
        card_id <- paste0("card_custom_", rowi$check_id)  # Define card_id here
        
        bs4Card(
          id = card_id,
          width = 12,
          title = rowi$description,
          closable = FALSE,
          status = "secondary",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          
          p(strong("Expression: "), rowi$expression_raw),
          p(strong("Severity: "), rowi$severity),
          
          checkboxInput(switch_id, "Run this custom check?", value = TRUE, width = "200px")
        )
      })
      
      output$ui_custom_checks <- renderUI({
        tagList(all_custom_ui)
      })
    }
  })
  
  gather_selected_checks <- function(){
    selected_ids <- character(0)
    df_checks <- rv$checks_available
    if(!is.null(df_checks) && NROW(df_checks) > 0){
      for(i in seq_len(nrow(df_checks))){
        sid <- paste0("check_switch_", df_checks$check_id[i])
        if (isTRUE(input[[sid]])) selected_ids <- c(selected_ids, df_checks$check_id[i])
      }
    }
    if(nrow(rv$custom_checks) > 0){
      for(i in seq_len(nrow(rv$custom_checks))){
        sid <- paste0("check_switch_custom_", rv$custom_checks$check_id[i])
        if (isTRUE(input[[sid]])) selected_ids <- c(selected_ids, rv$custom_checks$check_id[i])
      }
    }
    selected_ids
  }
  
  
  unify_data <- function(df, mapping_list){
    df_out <- df
    
    # 1) Rename columns according to mapping (if provided)
    if (length(mapping_list)) {
      for (std_col in names(mapping_list)) {
        user_col <- mapping_list[[std_col]]
        if (!is.null(user_col) && user_col %in% colnames(df_out)) {
          colnames(df_out)[colnames(df_out) == user_col] <- std_col
        }
      }
    }
    
    # 2) Standardize gender values once (after renaming)
    if ("gender" %in% names(df_out)) {
      df_out$gender <- standardize_gender_vec(df_out$gender, rv$gender_map)
    }
    
    df_out
  }
  
  
  ############################################################################
  # RUN ALL CHECKS (STEP 4 -> STEP 5)
  ############################################################################
  run_all_checks <- function(){
    if(is.null(rv$raw_data) || nrow(rv$raw_data)==0){
      showNotification("No data loaded. Please upload data first.", type="error")
      return()
    }
    data_unified <- unify_data(rv$raw_data, rv$mapping)
    
    for(dc in c("admission_date","discharge_date","birth_date")){
      if(dc %in% names(data_unified)){
        data_unified[[dc]] <- suppressWarnings(as.Date(data_unified[[dc]]))
      }
    }
    if("age" %in% names(data_unified)){
      data_unified$age <- suppressWarnings(as.numeric(data_unified$age))
    }
    
    rv$mapped_data <- data_unified
    
    selected_checks <- gather_selected_checks()
    if(length(selected_checks)==0){
      showNotification("No checks selected. Please select checks in Step 4.", type="error")
      return()
    }
    
    # Prepare data frame for issues
    all_issues <- data.frame(
      check_id=character(),
      patient_id=character(),
      issue=character(),
      details=character(),
      severity=character(),
      stringsAsFactors=FALSE
    )
    add_issue <- function(check_id, pid_vec, issue, details, severity){
      if(length(pid_vec)==0) return()
      new_df <- data.frame(
        check_id=check_id,
        patient_id=as.character(pid_vec),
        issue=issue,
        details=details,
        severity=severity,
        stringsAsFactors=FALSE
      )
      all_issues <<- rbind(all_issues, new_df)
    }
    
    # THE COMPLETE CATEGORY-BASED CHECKS LOGIC (cat1_1..cat6_15)
    d <- data_unified
    
    # Normalize common text columns once (pre-lowercase)
    if ("icd" %in% names(d)) d$icd <- tolower(as.character(d$icd))
    if ("ops" %in% names(d)) d$ops <- tolower(as.character(d$ops))
    if ("anamnese" %in% names(d)) d$anamnese <- tolower(as.character(d$anamnese))
    
    pid_or_row <- if("patient_id" %in% names(d)) d$patient_id else seq_len(nrow(d))
    
    has_keyword <- function(textcol, kw) grepl(kw, textcol, perl = TRUE)
    has_icd     <- function(icdcol,  pat) grepl(pat, icdcol,  perl = TRUE)
    has_ops     <- function(opscol,  pat) grepl(pat, opscol,  perl = TRUE)
    
    
    # ----------------------- CATEGORY 1 (Completeness) -----------------------
    if("cat1_1" %in% selected_checks && all(c("admission_date","icd") %in% names(d))){
      idx <- which(!is.na(d$admission_date) & (is.na(d$icd) | d$icd==""))
      add_issue("cat1_1", pid_or_row[idx],
                "Missing ICD for admitted patient",
                "Patient has admission_date but ICD is empty",
                "High")
    }
    if("cat1_2" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_surg <- which(has_keyword(d$anamnese,"surg|chirurg"))
      idx_missing <- idx_surg[which(is.na(d$ops[idx_surg]) | d$ops[idx_surg]=="")]
      if(length(idx_surg)>0 && length(idx_missing) > 0.1*length(idx_surg)){
        add_issue("cat1_2", pid_or_row[idx_missing],
                  "OPS missing for 'surgery' mention",
                  "More than 10% lacking OPS among references to 'surgery'",
                  "Medium")
      }
    }
    if("cat1_3" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_diab <- which(has_keyword(d$anamnese,"diab"))
      idx_prob <- idx_diab[ which(!str_detect(tolower(d$icd[idx_diab]), "^e1[0-4]")) ]
      add_issue("cat1_3", pid_or_row[idx_prob],
                "Diabetes mention but no E10-E14",
                "Anamnese references diabetes but no ICD E10-E14",
                "High")
    }
    if("cat1_4" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_heart <- which(has_keyword(d$anamnese,"heart|herz|cardio"))
      idx_prob <- idx_heart[ which(!str_detect(tolower(d$icd[idx_heart]), "^i")) ]
      add_issue("cat1_4", pid_or_row[idx_prob],
                "Heart disease mention but no Ixx ICD",
                "Likely missing ICD for cardiac condition",
                "Medium")
    }
    if("cat1_5" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_chemo <- which(has_keyword(d$anamnese,"chemo"))
      idx_prob <- idx_chemo[ which(is.na(d$ops[idx_chemo]) | d$ops[idx_chemo]=="") ]
      add_issue("cat1_5", pid_or_row[idx_prob],
                "Chemo mention but missing OPS",
                "Likely missing chemotherapy procedure coding",
                "High")
    }
    if("cat1_6" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_copd <- which(has_keyword(d$anamnese,"copd"))
      idx_prob <- idx_copd[ which(!str_detect(tolower(d$icd[idx_copd]), "^j44")) ]
      add_issue("cat1_6", pid_or_row[idx_prob],
                "COPD mention but no J44",
                "Likely missing COPD diagnosis",
                "High")
    }
    if("cat1_7" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_rad <- which(has_keyword(d$anamnese,"radiol|röntgen"))
      idx_prob <- idx_rad[ which(is.na(d$ops[idx_rad]) | d$ops[idx_rad]=="") ]
      add_issue("cat1_7", pid_or_row[idx_prob],
                "Radiology mention but no OPS",
                "Missing imaging procedure code",
                "Medium")
    }
    if("cat1_8" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_allerg <- which(has_keyword(d$anamnese,"allerg"))
      idx_prob <- idx_allerg[ which(!str_detect(tolower(d$icd[idx_allerg]), "^t78")) ]
      add_issue("cat1_8", pid_or_row[idx_prob],
                "Allergy mention but no T78 ICD",
                "Possible missing allergy diagnosis",
                "Medium")
    }
    if("cat1_9" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_dial <- which(has_keyword(d$anamnese,"dialysis|dialyse"))
      idx_prob <- idx_dial[ which(is.na(d$ops[idx_dial]) | d$ops[idx_dial]=="") ]
      add_issue("cat1_9", pid_or_row[idx_prob],
                "Dialysis mention but no OPS",
                "Likely missing dialysis procedure code",
                "Medium")
    }
    if("cat1_10" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_htn <- which(has_keyword(d$anamnese,"hyperton|hypertens"))
      idx_prob <- idx_htn[ which(!str_detect(tolower(d$icd[idx_htn]), "^i10")) ]
      add_issue("cat1_10", pid_or_row[idx_prob],
                "Hypertension mention but no I10",
                "Likely missing essential HTN code",
                "Medium")
    }
    if("cat1_11" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_endo <- which(has_keyword(d$anamnese,"endosc"))
      idx_prob <- idx_endo[ which(is.na(d$ops[idx_endo]) | d$ops[idx_endo]=="") ]
      add_issue("cat1_11", pid_or_row[idx_prob],
                "Endoscopy mention but no OPS",
                "Missing endoscopic procedure code",
                "Medium")
    }
    if("cat1_12" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_strk <- which(has_keyword(d$anamnese,"stroke|schlaganfall"))
      idx_prob <- idx_strk[ which(!str_detect(tolower(d$icd[idx_strk]), "^i63|^i64")) ]
      add_issue("cat1_12", pid_or_row[idx_prob],
                "Stroke mention but no I63/I64",
                "Likely missing stroke diagnosis code",
                "High")
    }
    if("cat1_13" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_inf <- which(has_keyword(d$anamnese,"infect"))
      idx_prob <- idx_inf[ which(!str_detect(tolower(d$icd[idx_inf]), "^b")) ]
      add_issue("cat1_13", pid_or_row[idx_prob],
                "Infection mention but no Bxx",
                "Likely missing infection code",
                "Medium")
    }
    if("cat1_14" %in% selected_checks && all(c("anamnese","ops") %in% names(d))){
      idx_prost <- which(has_keyword(d$anamnese,"prosthe"))
      idx_prob <- idx_prost[ which(is.na(d$ops[idx_prost]) | d$ops[idx_prost]=="") ]
      add_issue("cat1_14", pid_or_row[idx_prob],
                "Prosthesis mention but no OPS",
                "Likely missing implant procedure code",
                "Medium")
    }
    if("cat1_15" %in% selected_checks && all(c("anamnese","icd") %in% names(d))){
      idx_depr <- which(has_keyword(d$anamnese,"depress"))
      idx_prob <- idx_depr[ which(!str_detect(tolower(d$icd[idx_depr]), "^f3[2-3]")) ]
      add_issue("cat1_15", pid_or_row[idx_prob],
                "Depression mention but no F32-F33",
                "Likely missing major depression code",
                "Medium")
    }
    if("cat1_16" %in% selected_checks && all(c("admission_date","icd","ops") %in% names(d))){
      idx <- which(!is.na(d$admission_date) & ( (is.na(d$icd)|d$icd=="") & (is.na(d$ops)|d$ops=="") ))
      add_issue("cat1_16", pid_or_row[idx],
                "Admission but no ICD/OPS at all",
                "Major documentation gap: no codes at all",
                "High")
    }
    
    # ----------------------- CATEGORY 2 (Age-based) -----------------------
    if("cat2_1" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age < 15 & has_icd(d$icd,"C61"))
      add_issue("cat2_1", pid_or_row[idx],
                "Prostate cancer in <15y",
                "ICD=C61 but age<15",
                "High")
    }
    if("cat2_2" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age < 30 & has_icd(d$icd,"F00|G30"))
      add_issue("cat2_2", pid_or_row[idx],
                "Alzheimer code in <30y",
                "ICD=F00/G30 but age<30",
                "High")
    }
    if("cat2_3" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age > 70 & has_icd(d$icd,"F8[0-9]"))
      add_issue("cat2_3", pid_or_row[idx],
                "Child dev. disorder in 70+",
                "ICD=F80-F89 but age>70",
                "Medium")
    }
    if("cat2_4" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age < 18 & has_icd(d$icd,"M80"))
      add_issue("cat2_4", pid_or_row[idx],
                "Osteoporosis <18y",
                "ICD=M80 but age<18",
                "Medium")
    }
    if("cat2_5" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age > 60 & has_icd(d$icd,"B05"))
      add_issue("cat2_5", pid_or_row[idx],
                "Measles in >60y",
                "ICD=B05 but age>60",
                "Medium")
    }
    if("cat2_6" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & str_detect(tolower(d$icd), "^o(6[0-9]|7[0-5])"))
      add_issue("cat2_6", pid_or_row[idx],
                "Birth-related ICD in male",
                "ICD=O60-O75 but gender=male",
                "High")
    }
    if("cat2_7" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"N95"))
      add_issue("cat2_7", pid_or_row[idx],
                "Menopause ICD in male",
                "ICD=N95 but gender=male",
                "High")
    }
    if("cat2_8" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age < 1 & has_icd(d$icd,"L70"))
      add_issue("cat2_8", pid_or_row[idx],
                "Teen acne in neonate",
                "ICD=L70 but age<1mo",
                "Medium")
    }
    if("cat2_9" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age < 30 & has_icd(d$icd,"H35\\.3"))
      add_issue("cat2_9", pid_or_row[idx],
                "Macular degeneration <30y",
                "ICD=H35.3 but age<30",
                "Medium")
    }
    if("cat2_10" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age > 21 & has_icd(d$icd,"G80"))
      add_issue("cat2_10", pid_or_row[idx],
                "Infantile CP in adult",
                "ICD=G80 but age>21",
                "Medium")
    }
    if("cat2_11" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"O14"))
      add_issue("cat2_11", pid_or_row[idx],
                "Preeclampsia in male",
                "ICD=O14 but gender=male",
                "High")
    }
    if("cat2_12" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age > 70 & has_icd(d$icd,"M08"))
      add_issue("cat2_12", pid_or_row[idx],
                "Juvenile arthritis in 70+",
                "ICD=M08 but age>70",
                "Medium")
    }
    if("cat2_13" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"E29"))
      add_issue("cat2_13", pid_or_row[idx],
                "Testosterone deficiency in female",
                "ICD=E29 but gender=female",
                "Medium")
    }
    if("cat2_14" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"C62"))
      add_issue("cat2_14", pid_or_row[idx],
                "Testicular tumor in female",
                "ICD=C62 but gender=female",
                "High")
    }
    if("cat2_15" %in% selected_checks && all(c("icd","age") %in% names(d))){
      idx <- which(d$age > 60 & has_icd(d$icd,"E30\\.0"))
      add_issue("cat2_15", pid_or_row[idx],
                "Delayed puberty in 60+",
                "ICD=E30.0 but age>60",
                "Medium")
    }
    
    # ----------------------- CATEGORY 3 (Gender-based) -----------------------
    if("cat3_1" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"N83"))
      add_issue("cat3_1", pid_or_row[idx],
                "Ovarian cyst in male",
                "ICD=N83 but gender=male",
                "High")
    }
    if("cat3_2" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"N41"))
      add_issue("cat3_2", pid_or_row[idx],
                "Prostatitis in female",
                "ICD=N41 but gender=female",
                "High")
    }
    if("cat3_3" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & str_detect(d$icd, "^O"))
      add_issue("cat3_3", pid_or_row[idx],
                "Pregnancy code in male",
                "ICD=Oxx but gender=male",
                "High")
    }
    if("cat3_4" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"C62"))
      add_issue("cat3_4", pid_or_row[idx],
                "Testicular cancer in female",
                "ICD=C62 but gender=female",
                "High")
    }
    if("cat3_5" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"N80"))
      add_issue("cat3_5", pid_or_row[idx],
                "Endometriosis in male",
                "ICD=N80 but gender=male",
                "High")
    }
    if("cat3_6" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"N52"))
      add_issue("cat3_6", pid_or_row[idx],
                "Erectile dysfunction in female",
                "ICD=N52 but gender=female",
                "High")
    }
    if("cat3_7" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"C53"))
      add_issue("cat3_7", pid_or_row[idx],
                "Cervical cancer in male",
                "ICD=C53 but gender=male",
                "High")
    }
    if("cat3_8" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"E28\\.1"))
      add_issue("cat3_8", pid_or_row[idx],
                "Testosterone excess in female",
                "ICD=E28.1 but gender=female",
                "Medium")
    }
    if("cat3_9" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & str_detect(d$icd,"^(N92|N93)"))
      add_issue("cat3_9", pid_or_row[idx],
                "Menstrual disorder in male",
                "ICD=N92/N93 but gender=male",
                "High")
    }
    if("cat3_10" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"C50"))
      add_issue("cat3_10", pid_or_row[idx],
                "Breast cancer in male",
                "ICD=C50 but gender=male (rare, flagged for review)",
                "Medium")
    }
    if("cat3_11" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"N47"))
      add_issue("cat3_11", pid_or_row[idx],
                "Phimosis in female",
                "ICD=N47 but gender=female",
                "High")
    }
    if("cat3_12" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"N76"))
      add_issue("cat3_12", pid_or_row[idx],
                "Vulvitis in male",
                "ICD=N76 but gender=male",
                "High")
    }
    if("cat3_13" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & str_detect(tolower(d$icd), "^p"))
      add_issue("cat3_13", pid_or_row[idx],
                "Birth injuries (Pxx) in male baby",
                "ICD=Pxx - naive check for mismatch",
                "Medium")
    }
    if("cat3_14" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="female" & has_icd(d$icd,"Q53"))
      add_issue("cat3_14", pid_or_row[idx],
                "Cryptorchidism in female",
                "ICD=Q53 but gender=female",
                "High")
    }
    if("cat3_15" %in% selected_checks && all(c("icd","gender") %in% names(d))){
      idx <- which(tolower(d$gender)=="male" & has_icd(d$icd,"O21"))
      add_issue("cat3_15", pid_or_row[idx],
                "Hyperemesis grav. in male",
                "ICD=O21 but gender=male",
                "High")
    }
    
    # ----------------------- CATEGORY 4 (Temporal) -----------------------
    if("cat4_1" %in% selected_checks && all(c("ops","admission_date") %in% names(d))){
      # placeholder
    }
    if("cat4_2" %in% selected_checks && all(c("admission_date","discharge_date") %in% names(d))){
      idx <- which(d$discharge_date < d$admission_date)
      add_issue("cat4_2", pid_or_row[idx],
                "Discharge < admission date",
                "Temporal inconsistency",
                "Critical")
    }
    if("cat4_3" %in% selected_checks && all(c("admission_date","discharge_date") %in% names(d))){
      # placeholder
    }
    if("cat4_4" %in% selected_checks && "admission_date" %in% names(d)){
      if("patient_id" %in% names(d)){
        duplicated_combo <- duplicated(d[,c("patient_id","admission_date")])
        idx <- which(duplicated_combo)
        add_issue("cat4_4", pid_or_row[idx],
                  "Multiple admissions same date/patient",
                  "Possible duplicated record",
                  "Medium")
      }
    }
    if("cat4_5" %in% selected_checks && "ops" %in% names(d)){
      # placeholder
    }
    if("cat4_6" %in% selected_checks && "admission_date" %in% names(d)){
      idx <- which(d$admission_date > Sys.Date())
      add_issue("cat4_6", pid_or_row[idx],
                "Admission date in future",
                "Data entry or system clock error",
                "Medium")
    }
    if("cat4_7" %in% selected_checks && all(c("admission_date","discharge_date") %in% names(d))){
      # placeholder
    }
    if("cat4_8" %in% selected_checks && all(c("admission_date","discharge_date","ops") %in% names(d))){
      idx <- which(d$admission_date == d$discharge_date & (!is.na(d$ops) & d$ops!=""))
      add_issue("cat4_8", pid_or_row[idx],
                "Same-day discharge for complex procedure",
                "Potentially unrealistic timeline",
                "Low")
    }
    if("cat4_9" %in% selected_checks && all(c("admission_date","discharge_date","icd") %in% names(d))){
      # placeholder
    }
    if("cat4_10" %in% selected_checks && "ops" %in% names(d)){
      # placeholder
    }
    if("cat4_11" %in% selected_checks && all(c("anamnese","discharge_date") %in% names(d))){
      # placeholder
    }
    if("cat4_12" %in% selected_checks && all(c("admission_date","birth_date") %in% names(d))){
      idx <- which(d$admission_date < d$birth_date)
      add_issue("cat4_12", pid_or_row[idx],
                "Admission < birth date",
                "Impossible timeline: admission before birth",
                "Critical")
    }
    if("cat4_13" %in% selected_checks && "ops" %in% names(d)){
      # placeholder
    }
    if("cat4_14" %in% selected_checks && "ops" %in% names(d)){
      # placeholder
    }
    if("cat4_15" %in% selected_checks && all(c("admission_date","discharge_date") %in% names(d))){
      idx <- which(d$discharge_date < d$admission_date)
      add_issue("cat4_15", pid_or_row[idx],
                "Discharge < admission (duplicate check)",
                "German-labeled duplicate check of cat4_2",
                "Critical")
    }
    
    # ----------------------- CATEGORY 5 (Diagnosis–Procedure) -----------------------
    if("cat5_1" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_append <- which(has_ops(d$ops,"5-470|appendect"))
      idx_no_icd <- idx_append[which(!str_detect(tolower(d$icd[idx_append]), "k35"))]
      add_issue("cat5_1", pid_or_row[idx_no_icd],
                "Appendectomy but no K35",
                "Likely missing appendicitis diagnosis",
                "Medium")
    }
    if("cat5_2" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_knee_ops <- which(has_ops(d$ops,"5-82|knee replace"))
      idx_prob <- idx_knee_ops[ which(!str_detect(tolower(d$icd[idx_knee_ops]), "m17")) ]
      add_issue("cat5_2", pid_or_row[idx_prob],
                "Knee replacement but no M17",
                "Likely missing arthrosis code",
                "Medium")
    }
    if("cat5_3" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_chemo_ops <- which(has_ops(d$ops,"8-54|chemotherap"))
      idx_prob <- idx_chemo_ops[ which(!str_detect(tolower(d$icd[idx_chemo_ops]), "^c|^d[0-4]")) ]
      add_issue("cat5_3", pid_or_row[idx_prob],
                "Chemo OPS but no cancer ICD",
                "Likely missing malignant or neoplasm code",
                "High")
    }
    if("cat5_4" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_cardio_ops <- which(has_ops(d$ops,"8-83|heart cath"))
      idx_prob <- idx_cardio_ops[ which(!str_detect(tolower(d$icd[idx_cardio_ops]), "^i")) ]
      add_issue("cat5_4", pid_or_row[idx_prob],
                "Heart catheter w/o Ixx",
                "Likely missing cardiology code",
                "Medium")
    }
    if("cat5_5" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_dial_ops <- which(has_ops(d$ops,"8-85|dialysis"))
      idx_prob <- idx_dial_ops[ which(!str_detect(tolower(d$icd[idx_dial_ops]), "^n18")) ]
      add_issue("cat5_5", pid_or_row[idx_prob],
                "Dialysis but no N18",
                "Missing CKD or kidney insuff. code",
                "Medium")
    }
    if("cat5_6" %in% selected_checks && all(c("ops","gender") %in% names(d))){
      idx_csec_ops <- which(has_ops(d$ops,"5-74|c-section"))
      idx_prob <- idx_csec_ops[ which(tolower(d$gender[idx_csec_ops])=="male") ]
      add_issue("cat5_6", pid_or_row[idx_prob],
                "C-section in male",
                "OPS=5-74 but gender=male",
                "High")
    }
    if("cat5_7" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_cat_op <- which(has_ops(d$ops,"5-316|cataract"))
      idx_prob <- idx_cat_op[ which(!str_detect(tolower(d$icd[idx_cat_op]), "h25|h26")) ]
      add_issue("cat5_7", pid_or_row[idx_prob],
                "Cataract op but no H25/H26",
                "Likely missing cataract diagnosis",
                "Medium")
    }
    if("cat5_8" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_bypass <- which(has_ops(d$ops,"5-43|gastric bypass"))
      idx_prob <- idx_bypass[ which(!str_detect(tolower(d$icd[idx_bypass]), "e66")) ]
      add_issue("cat5_8", pid_or_row[idx_prob],
                "Gastric bypass w/o E66",
                "Missing obesity code",
                "Medium")
    }
    if("cat5_9" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_hyst <- which(has_ops(d$ops,"5-68|hysterect"))
      idx_prob <- idx_hyst[ which(!str_detect(tolower(d$icd[idx_hyst]), "n(8[0-9])|c5[3-8]")) ]
      add_issue("cat5_9", pid_or_row[idx_prob],
                "Hysterectomy w/o GYN ICD",
                "Likely missing or incomplete GYN code",
                "Medium")
    }
    if("cat5_10" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_trans <- which(has_ops(d$ops,"8-80|transfus"))
      idx_prob <- idx_trans[ which(!str_detect(tolower(d$icd[idx_trans]), "^d[5-6][0-9]")) ]
      add_issue("cat5_10", pid_or_row[idx_prob],
                "Transfusion w/o anemia ICD",
                "Missing D50-D64 or other transfusion rationale",
                "Medium")
    }
    if("cat5_11" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_knee_arthro <- which(has_ops(d$ops,"5-80|arthroscop"))
      idx_prob <- idx_knee_arthro[ which(!str_detect(tolower(d$icd[idx_knee_arthro]), "s83|m23|m17")) ]
      add_issue("cat5_11", pid_or_row[idx_prob],
                "Knee arthroscopy w/o knee ICD",
                "Missing S83/M23/M17 code",
                "Medium")
    }
    if("cat5_12" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_rad_ops <- which(has_ops(d$ops,"3-0|radiol|5-30"))
      idx_prob <- idx_rad_ops[ which(!str_detect(tolower(d$icd[idx_rad_ops]), "[ic][^ ]*")) ]
      add_issue("cat5_12", pid_or_row[idx_prob],
                "Radiological OPS w/o ICD reason",
                "Missing reason-for-exam code",
                "Medium")
    }
    if("cat5_13" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_skin_graft <- which(has_ops(d$ops,"5-90|skin graft"))
      idx_prob <- idx_skin_graft[ which(!str_detect(tolower(d$icd[idx_skin_graft]), "^t2|^s0|burn")) ]
      add_issue("cat5_13", pid_or_row[idx_prob],
                "Skin graft w/o wound/burn ICD",
                "Likely missing code for injury/burn",
                "Medium")
    }
    if("cat5_14" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_ugi <- which(has_ops(d$ops,"1-62|upper gi endosc"))
      idx_prob <- idx_ugi[ which(!str_detect(tolower(d$icd[idx_ugi]), "^k2|^k3|^k9")) ]
      add_issue("cat5_14", pid_or_row[idx_prob],
                "Upper GI endoscopy w/o Kxx",
                "Missing GI diagnosis code Kxx",
                "Medium")
    }
    if("cat5_15" %in% selected_checks && all(c("ops","icd") %in% names(d))){
      idx_pace <- which(has_ops(d$ops,"5-37|pacemaker"))
      idx_prob <- idx_pace[ which(!str_detect(tolower(d$icd[idx_pace]), "^i4[4-9]")) ]
      add_issue("cat5_15", pid_or_row[idx_prob],
                "Pacemaker w/o I44–I49",
                "Missing arrhythmia code",
                "Medium")
    }
    
    # ----------------------- CATEGORY 6 (Code Integrity) -----------------------
    cat6_1_block(d, selected_checks, pid_or_row, add_issue)
    
    if("cat6_2" %in% selected_checks && "ops" %in% names(d)){
      idx <- which(str_detect(d$ops, "^0-") | str_detect(d$ops,"retiredprocedure"))
      add_issue("cat6_2", pid_or_row[idx],
                "OPS referencing retired code",
                "Likely outdated OPS procedure code",
                "Medium")
    }
    cat6_3_block(d, selected_checks, pid_or_row, add_issue)
    
    if("cat6_4" %in% selected_checks && "icd" %in% names(d)){
      idx <- which(str_detect(d$icd,"ICD9|^9[0-9]|^\\d{3}\\.?\\d"))
      add_issue("cat6_4", pid_or_row[idx],
                "Likely ICD-9 usage",
                "ICD referencing older standard or numeric code",
                "Medium")
    }
    if("cat6_5" %in% selected_checks && "icd" %in% names(d)){
      idx <- which(str_detect(tolower(d$icd),"xxx|zzz|fake"))
      add_issue("cat6_5", pid_or_row[idx],
                "Fake ICD code (xxx,zzz...)",
                "Placeholder code found",
                "High")
    }
    if("cat6_6" %in% selected_checks && "icd" %in% names(d)){
      idx <- which(str_detect(tolower(d$icd),"^\\d{3}\\.?\\d"))
      add_issue("cat6_6", pid_or_row[idx],
                "Numeric ICD-9 code in ICD-10 environment",
                "ICD purely numeric form (like 250.0)",
                "Medium")
    }
    
    cat6_7_block(d, selected_checks, pid_or_row, add_issue)
    
    # OPS validator (structure) + optional near-miss hints
    cat6_8_block(d, selected_checks, pid_or_row, add_issue)
    cat6_8_nearmiss_note(d, selected_checks, pid_or_row, add_issue)
    
    if("cat6_9" %in% selected_checks && all(c("icd","ops") %in% names(d))){
      idx <- which(str_detect(tolower(d$icd),"z9") | str_detect(tolower(d$ops),"z9"))
      add_issue("cat6_9", pid_or_row[idx],
                "Foreign code system (z9...)",
                "Naive detection of 'z9' code",
                "Low")
    }
    if("cat6_10" %in% selected_checks && all(c("icd","ops") %in% names(d))){
      idx <- which(str_detect(tolower(d$ops),"icd"))
      add_issue("cat6_10", pid_or_row[idx],
                "Diagnosis code in OPS column (naive mismatch)",
                "OPS column has 'icd' pattern",
                "Medium")
    }
    
    cat6_11_block(d, selected_checks, pid_or_row, add_issue)
    
    if("cat6_12" %in% selected_checks && "icd" %in% names(d)){
      # placeholder for facility specialty mismatch
    }
    if("cat6_13" %in% selected_checks && "icd" %in% names(d)){
      idx <- which(str_detect(tolower(d$icd),"old_"))
      add_issue("cat6_13", pid_or_row[idx],
                "ICD marked 'old_' => outdated",
                "Code not updated to current standard",
                "Medium")
    }
    if("cat6_14" %in% selected_checks && "icd" %in% names(d)){
      idx <- which(str_detect(tolower(d$icd),"veraltet|icd9"))
      add_issue("cat6_14", pid_or_row[idx],
                "Veraltete ICD coding",
                "ICD flagged as outdated or 'veraltet'",
                "Medium")
    }
    if("cat6_15" %in% selected_checks && "ops" %in% names(d)){
      idx <- which(str_detect(tolower(d$ops),"opoutside|extfacility"))
      add_issue("cat6_15", pid_or_row[idx],
                "OPS for external facility procedure",
                "Naive detection of 'opoutside' or 'extfacility'",
                "Low")
    }
    
    #### End of the category-based checks logic ####
    # -----------------------------------------------------------------
    # CUSTOM CHECKS (vom Benutzer definiert)
    # -----------------------------------------------------------------
    if (nrow(rv$custom_checks) > 0) {
      # nur die Checks ausführen, die der Benutzer in Step 4 aktiviert hat
      custom_to_run <- rv$custom_checks %>% 
        dplyr::filter(check_id %in% selected_checks)
      
      for (i in seq_len(nrow(custom_to_run))) {
        cid   <- custom_to_run$check_id[i]
        desc  <- custom_to_run$description[i]
        sev   <- custom_to_run$severity[i]
        expr  <- custom_to_run$expression_raw[i]
        
        # Ausdruck im Kontext der (noch NICHT umbenannten) Original‑Daten auswerten
        idx <- tryCatch(
          {
            with(rv$raw_data, which(eval(parse(text = expr))))
          },
          error = function(e) integer(0)      # Syntax‑ oder Laufzeitfehler abfangen
        )
        
        add_issue(
          check_id = cid,
          pid_vec  = pid_or_row[idx],          # bereits zuvor definiert
          issue    = desc,
          details  = paste0("Custom check '", desc,
                            "' TRUE für ", length(idx), " Zeilen."),
          severity = sev
        )
      }
    }
    # ----------------------------------------------------------------
    
    rv$issues <- all_issues
    showNotification("Checks completed. Proceeding to Results...", type="message")
    updateTabItems(session, "tabs", "tab_results")
  }
  
  observeEvent(input$btn_run_checks,{
    wrap_run_checks({
      run_all_checks()
    })
  })
  
  ############################################################################
  # STEP 5: RESULTS
  ############################################################################
  output$vb_total_checks <- renderText({
    length(gather_selected_checks())
  })
  
  output$vb_total_issues <- renderText({
    req(rv$issues)
    nrow(rv$issues)
  })
  
  output$vb_records_affected <- renderText({
    req(rv$issues)
    length(unique(rv$issues$patient_id))
  })
  
  output$check_results_table <- renderDT({
    req(rv$issues)
    
    if(nrow(rv$issues)==0){
      return(datatable(data.frame(Message="No issues found."),options=list(dom='t')))
    }
    
    d_total <- nrow(rv$mapped_data)
    sumdf <- rv$issues %>%
      group_by(check_id, issue, severity) %>%
      summarize(num_records=n(), .groups="drop") %>%
      mutate(
        records_not_affected = d_total - num_records,
        pct_affected         = round(100 * num_records / d_total, 2),
        pct_not_affected     = round(100 - pct_affected, 2)
      )
    
    datatable(sumdf, options=list(pageLength=10,scrollX=TRUE))
  })
  
  output$summary_plot <- renderPlot({
    req(rv$issues)
    if(nrow(rv$issues)==0){
      plot.new()
      title("No issues found.")
      return()
    }
    sev_ct <- rv$issues %>% group_by(severity) %>% summarize(count=n(),.groups="drop")
    barplot(
      height=sev_ct$count,
      names.arg=sev_ct$severity,
      main="Issues by Severity",
      ylab="Number of Issues"
    )
  })
  
  output$category_plot <- renderPlot({
    req(rv$issues)
    if(nrow(rv$issues)==0){
      plot.new()
      title("No issues to summarize by category.")
      return()
    }
    c_map <- rv$checks_available %>% select(check_id, category)
    merged <- left_join(rv$issues, c_map, by="check_id")
    merged$category[is.na(merged$category)] <- "Custom"
    cat_ct <- merged %>% group_by(category) %>% summarize(count=n(),.groups="drop")
    
    barplot(
      height=cat_ct$count,
      names.arg=cat_ct$category,
      main="Issues by Category",
      ylab="Number of Issues",
      las=2
    )
  })
  
  observe({
    req(rv$issues)
    if(nrow(rv$issues)==0){
      updateSelectInput(session,"detail_check_select",choices=character(0))
      return()
    }
    built_in_map <- if (!is.null(rv$checks_available) && NROW(rv$checks_available)>0)
      setNames(rv$checks_available$description, rv$checks_available$check_id) else c()
    custom_map   <- if (nrow(rv$custom_checks)>0)
      setNames(rv$custom_checks$description, rv$custom_checks$check_id) else c()
    label_map    <- c(built_in_map, custom_map)
    cids <- unique(rv$issues$check_id)
    c_labels <- sapply(cids, function(x){ if(!is.null(label_map[[x]])) label_map[[x]] else x })
    updateSelectInput(session,"detail_check_select",choices=setNames(cids,c_labels))
  })
  
  
  output$detailed_issues_table <- renderDT({
    req(rv$issues)
    if(nrow(rv$issues)==0){
      return(datatable(data.frame(Message="No issues."),options=list(dom='t')))
    }
    req(input$detail_check_select)
    dsub <- rv$issues[rv$issues$check_id == input$detail_check_select,]
    if(nrow(dsub)==0){
      datatable(data.frame(Message="No issues for this check."),options=list(dom='t'))
    } else {
      datatable(dsub,options=list(pageLength=5,scrollX=TRUE))
    }
  })
  
  output$download_full_report <- downloadHandler(
    filename=function(){
      paste0("data_quality_report_",Sys.Date(),".csv")
    },
    content=function(file){
      req(rv$issues)
      write.csv(rv$issues, file, row.names=FALSE)
    }
  )
  
  output$download_adhoc_csv <- downloadHandler(
    filename = function() {
      paste0("adhoc_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(rv$adhoc_compare_df)) {
        # Create a tiny placeholder if user clicks too early
        write.csv(data.frame(Message="No comparison has been run yet."),
                  file, row.names = FALSE)
      } else {
        write.csv(rv$adhoc_compare_df, file, row.names = FALSE)
      }
    }
  )
  
  observeEvent(input$btn_update_analysis,{
    safe_notify("Re-checking with current data/mapping/selection...", "message")
    wrap_run_checks({ run_all_checks() })
  })
  
  observeEvent(input$btn_reset_analysis,{
    showNotification("Resetting entire analysis and returning to Step 1...", type="warning")
    
    rv$raw_data        <- NULL
    rv$colnames_all    <- NULL
    rv$mapping         <- list()
    rv$mapped_data     <- NULL
    rv$issues          <- NULL
    rv$custom_checks   <- rv$custom_checks[0,]
    rv$conditions_list <- rv$conditions_list[0,]
    
    # Recreate the catalog so Step 4 is immediately usable after reset
    rv$checks_available <- make_checks_def()
    
    updateTabItems(session, "tabs","tab_load_data")
  })
  
  
  # ----------------------------------------------------------------------
  # Select ALL / Deselect ALL check-boxes
  # ----------------------------------------------------------------------
  observeEvent(input$btn_select_all_checks, ignoreInit = TRUE, {
    if (!is.null(rv$checks_available) && NROW(rv$checks_available) > 0) {
      for (cid in rv$checks_available$check_id) {
        sid <- paste0("check_switch_", cid)
        if (!is.null(input[[sid]])) updateCheckboxInput(session, sid, value = TRUE)
      }
    }
    if (nrow(rv$custom_checks) > 0) {
      for (cid in rv$custom_checks$check_id) {
        sid <- paste0("check_switch_custom_", cid)
        if (!is.null(input[[sid]])) updateCheckboxInput(session, sid, value = TRUE)
      }
    }
  })
  
  observeEvent(input$btn_deselect_all_checks, ignoreInit = TRUE, {
    if (!is.null(rv$checks_available) && NROW(rv$checks_available) > 0) {
      for (cid in rv$checks_available$check_id) {
        sid <- paste0("check_switch_", cid)
        if (!is.null(input[[sid]])) updateCheckboxInput(session, sid, value = FALSE)
      }
    }
    if (nrow(rv$custom_checks) > 0) {
      for (cid in rv$custom_checks$check_id) {
        sid <- paste0("check_switch_custom_", cid)
        if (!is.null(input[[sid]])) updateCheckboxInput(session, sid, value = FALSE)
      }
    }
  })
  
  
  ##### Checks deletion
  observeEvent(input$last_deleted, {
    req(input$last_deleted)
    # Extract check_id from input id format: "del_custom_<check_id>"
    del_id <- sub("^del_custom_", "", input$last_deleted)
    
    # Remove the corresponding custom check row
    rv$custom_checks <- rv$custom_checks[rv$custom_checks$check_id != del_id, ]
    
    showNotification(paste("Custom check deleted:", del_id), type = "message")
  })
  
  ############################################################################
  # Go to step 6
  observeEvent(input$btn_go_step6, { go_to_step6(session) })
  
  ############################################################################
  # STEP 6: DATA ANONYMIZATION METRICS & EXTENDED ANALYSIS
  ############################################################################
  # STEP 6: DATA ANONYMIZATION METRICS & EXTENDED ANALYSIS
  # Helper to read any uploaded anonymized file (pure; returns data.frame or NULL)
  read_uploaded_any <- function(type,
                                csv_file = NULL, csv_header = TRUE, csv_sep = ",",
                                xls_file = NULL, xls_sheet = 1,
                                json_file = NULL,
                                fhir_file = NULL) {
    out <- NULL
    if (type == "CSV/TXT" && !is.null(csv_file)) {
      out <- tryCatch({
        data.table::fread(csv_file$datapath, header = csv_header, sep = csv_sep,
                          data.table = FALSE, showProgress = TRUE)
      }, error = function(e) NULL)
    } else if (type == "Excel" && !is.null(xls_file)) {
      out <- tryCatch({
        as.data.frame(readxl::read_excel(xls_file$datapath, sheet = xls_sheet))
      }, error = function(e) NULL)
    } else if (type == "JSON" && !is.null(json_file)) {
      out <- tryCatch({
        read_json_tabular(json_file$datapath)
      }, error = function(e) NULL)
    } else if (type == "FHIR" && !is.null(fhir_file)) {
      out <- tryCatch({
        read_fhir_tabular(fhir_file$datapath)
      }, error = function(e) NULL)
      # Optional demo fallback if parsing failed:
      if (is.null(out)) {
        out <- data.frame(patient_id = c("FHIR-ANON-1","FHIR-ANON-2"),
                          gender = c("male","female"),
                          stringsAsFactors = FALSE)
      }
    }
    out
  }
  
  # Load anonymized file (this MUST be outside the helper and inside server)
  observeEvent(input$btn_load_anonym, {
    wrap_load_anon({
      req(input$anon_file_type)
      df_anon <- switch(
        input$anon_file_type,
        "CSV/TXT" = read_uploaded_any("CSV/TXT",
                                      csv_file = input$file_anonym_csv,
                                      csv_header = isTRUE(input$anon_header_csv),
                                      csv_sep = input$anon_sep_csv %||% ","),
        "Excel"   = read_uploaded_any("Excel",
                                      xls_file = input$file_anonym_excel,
                                      xls_sheet = input$anon_excel_sheet %||% 1),
        "JSON"    = read_uploaded_any("JSON",
                                      json_file = input$file_anonym_json),
        "FHIR"    = read_uploaded_any("FHIR",
                                      fhir_file = input$file_anonym_fhir),
        NULL
      )
      
      if (is.null(df_anon)) {
        showNotification("Error reading anonymized file. Please verify format/options.", type="error")
        return()
      }
      
      # unify columns using the original mapping
      data_anon_unified <- df_anon
      if (length(rv$mapping)) {
        for (std_col in names(rv$mapping)) {
          user_col <- rv$mapping[[std_col]]
          if (!is.null(user_col) && user_col %in% colnames(data_anon_unified)) {
            colnames(data_anon_unified)[colnames(data_anon_unified) == user_col] <- std_col
          }
        }
      }
      rv$anonym_data <- data_anon_unified
      showNotification("Anonymized data loaded. You can now use Ad-hoc Column Comparison.", type = "message")
    })
  })
  
  
  # New Output: View Anonymized Data
  output$anon_data_view <- renderDT({
    req(rv$anonym_data)
    datatable(rv$anonym_data, options=list(pageLength=10, scrollX=TRUE))
  })
  
  # New Output: Column Comparison between original and anonymized
  output$column_comparison <- renderDT({
    req(rv$mapped_data, rv$anonym_data)
    orig_cols <- colnames(rv$mapped_data)
    anon_cols <- colnames(rv$anonym_data)
    all_cols <- union(orig_cols, anon_cols)
    df_cols <- data.frame(
      Column = all_cols,
      In_Original = ifelse(all_cols %in% orig_cols, "Yes", "No"),
      In_Anonymized = ifelse(all_cols %in% anon_cols, "Yes", "No"),
      stringsAsFactors = FALSE
    )
    datatable(df_cols, options=list(pageLength=10, scrollX=TRUE))
  })
  
  # ============ Extended Numeric Analysis (adds D & sqrt(n_eff)*D, fmt_p) ============
  output$numeric_analysis <- renderDT({
    req(rv$mapped_data, rv$anonym_data)
    
    df_orig <- as.data.frame(rv$mapped_data)
    df_anon <- as.data.frame(rv$anonym_data)
    
    common_cols  <- intersect(colnames(df_orig), colnames(df_anon))
    numeric_cols <- common_cols[sapply(df_orig[, common_cols, drop = FALSE], function(x) is.numeric(x) || is.integer(x))]
    
    if (length(numeric_cols) == 0) {
      return(DT::datatable(data.frame(Message = "No common numeric columns."), options = list(dom = "t")))
    }
    
    force_num <- function(v) { if (is.numeric(v)) v else suppressWarnings(as.numeric(v)) }
    safe_stat <- function(fun, v) { v <- v[is.finite(v)]; if (length(v) == 0) NA_real_ else fun(v, na.rm = TRUE) }
    
    res_list <- lapply(numeric_cols, function(col) {
      orig <- force_num(df_orig[[col]])
      anon <- force_num(df_anon[[col]])
      
      ks_out <- ks_test_asymp(orig, anon)
      
      data.frame(
        Column          = col,
        Orig_N_total    = length(orig),
        Anon_N_total    = length(anon),
        N_total_Delta   = length(anon) - length(orig),
        
        Orig_Distinct   = length(unique(orig[is.finite(orig)])),
        Anon_Distinct   = length(unique(anon[is.finite(anon)])),
        Distinct_Delta  = length(unique(anon[is.finite(anon)])) - length(unique(orig[is.finite(orig)])),
        
        Orig_Min        = safe_stat(min,  orig),
        Anon_Min        = safe_stat(min,  anon),
        Min_Shift       = safe_stat(min,  anon) - safe_stat(min,  orig),
        
        Orig_Max        = safe_stat(max,  orig),
        Anon_Max        = safe_stat(max,  anon),
        Max_Shift       = safe_stat(max,  anon) - safe_stat(max,  orig),
        
        Orig_Mean       = safe_stat(mean, orig),
        Anon_Mean       = safe_stat(mean, anon),
        Mean_Shift      = safe_stat(mean, anon) - safe_stat(mean, orig),
        
        Orig_SD         = safe_stat(sd,   orig),
        Anon_SD         = safe_stat(sd,   anon),
        SD_Shift        = safe_stat(sd,   anon) - safe_stat(sd,   orig),
        
        KS_D            = ks_out$statistic,
        KS_Z_sqrtNeffD  = ks_out$z,
        KS_p_value      = fmt_p(ks_out$p.value),
        stringsAsFactors = FALSE
      )
    })
    
    res_df <- do.call(rbind, res_list)
    DT::datatable(res_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  # =============================================================================== 
  
  
  
  # New Output: Extended Categorical Analysis
  output$categorical_analysis <- renderDT({
    req(rv$mapped_data, rv$anonym_data)
    df_orig <- as.data.frame(rv$mapped_data)
    df_anon <- as.data.frame(rv$anonym_data)
    common_cols <- intersect(colnames(df_orig), colnames(df_anon))
    cat_cols <- common_cols[sapply(df_orig[, common_cols, drop=FALSE], function(x) is.character(x) || is.factor(x))]
    if(length(cat_cols)==0) return(datatable(data.frame(Message="No common categorical columns."), options=list(dom='t')))
    
    res_list <- lapply(cat_cols, function(col){
      orig <- as.character(df_orig[[col]])
      anon <- as.character(df_anon[[col]])
      
      orig_missing <- sum(is.na(orig) | orig=="")
      anon_missing <- sum(is.na(anon) | anon=="")
      
      orig_levels <- unique(orig[!is.na(orig) & orig!=""])
      anon_levels <- unique(anon[!is.na(anon) & anon!=""])
      overlap_ratio <- length(intersect(orig_levels, anon_levels)) / length(union(orig_levels, anon_levels))
      
      tab_orig <- table(orig)
      tab_anon <- table(anon)
      common_levels <- intersect(names(tab_orig), names(tab_anon))
      if(length(common_levels) > 1){
        chisq_p <- tryCatch(chisq.test(rbind(tab_orig[common_levels], tab_anon[common_levels]))$p.value, error=function(e) NA)
      } else {
        chisq_p <- NA
      }
      
      # NEW: N (total) and Distinct counts
      orig_n_total <- length(orig)
      anon_n_total <- length(anon)
      orig_distinct <- length(orig_levels)
      anon_distinct <- length(anon_levels)
      
      data.frame(
        Column            = col,
        Orig_N_total      = orig_n_total,
        Anon_N_total      = anon_n_total,
        N_total_Delta     = anon_n_total - orig_n_total,
        Orig_Distinct     = orig_distinct,
        Anon_Distinct     = anon_distinct,
        Distinct_Delta    = anon_distinct - orig_distinct,
        Orig_Missing      = orig_missing,
        Anon_Missing      = anon_missing,
        Missing_Delta     = anon_missing - orig_missing,
        Overlap_Ratio     = overlap_ratio,
        ChiSq_p_value     = chisq_p, 
        stringsAsFactors  = FALSE
      )
    })
    
    res_df <- do.call(rbind, res_list)
    datatable(res_df, options=list(pageLength=10, scrollX=TRUE))
  })
  
  ## ----------  SERVER additions for ad-hoc column comparison ----------
  
  # Populate the two drop-downs once the respective data frames exist
  observe({
    req(rv$mapped_data)
    updateSelectInput(session,
                      "compare_orig_col",
                      choices = colnames(as.data.frame(rv$mapped_data)))
  })
  observe({
    req(rv$anonym_data)
    updateSelectInput(session,
                      "compare_anon_col",
                      choices = colnames(as.data.frame(rv$anonym_data)))
  })
  
  # Main comparison: runs when the button is clicked
  # =================== Ad-hoc Column Comparison (REPLACEMENT) ===================
  observeEvent(input$btn_compare_cols, {
    req(rv$mapped_data, rv$anonym_data, input$compare_orig_col, input$compare_anon_col)
    
    col_o <- input$compare_orig_col
    col_a <- input$compare_anon_col
    orig  <- rv$mapped_data[[col_o]]
    anon  <- rv$anonym_data[[col_a]]
    
    is_num_o <- is.numeric(orig)
    is_num_a <- is.numeric(anon)
    
    if (is_num_o && is_num_a) {
      # ---------- NUMERIC ----------
      # KS (asymptotisch) + Effektmaße
      ks_out <- ks_test_asymp(na.omit(orig), na.omit(anon))
      
      n_total_o <- length(orig)
      n_total_a <- length(anon)
      n_dist_o  <- length(unique(na.omit(orig)))
      n_dist_a  <- length(unique(na.omit(anon)))
      
      # Ergebnis-Tabelle inkl. Effektmaße und "asymptotic; ties present"
      res <- data.frame(
        Metric     = c("N total", "Distinct count",
                       "Min", "Max", "Mean", "SD",
                       "KS D (effect size)", "sqrt(n_eff)*D", 
                       "KS p-value (asymptotic; ties present)"),
        Original   = c(n_total_o, n_dist_o,
                       min(orig, na.rm = TRUE),
                       max(orig, na.rm = TRUE),
                       mean(orig, na.rm = TRUE),
                       sd  (orig, na.rm = TRUE),
                       ks_out$statistic, ks_out$z,
                       fmt_p(ks_out$p.value)),
        Anonymized = c(n_total_a, n_dist_a,
                       min(anon, na.rm = TRUE),
                       max(anon, na.rm = TRUE),
                       mean(anon, na.rm = TRUE),
                       sd  (anon, na.rm = TRUE),
                       "", "", ""),
        Shift      = c(n_total_a - n_total_o,
                       n_dist_a  - n_dist_o,
                       min(anon, na.rm = TRUE) - min(orig, na.rm = TRUE),
                       max(anon, na.rm = TRUE) - max(orig, na.rm = TRUE),
                       mean(anon, na.rm = TRUE) - mean(orig, na.rm = TRUE),
                       sd  (anon, na.rm = TRUE) - sd  (orig, na.rm = TRUE),
                       "", "", ""),
        check.names = FALSE
      )
      
      rv$adhoc_compare_df <- res
      rv$adhoc_compare_meta <- list(
        type     = "numeric",
        mean_orig= mean(orig, na.rm = TRUE),
        mean_anon= mean(anon, na.rm = TRUE),
        sd_orig  = sd  (orig, na.rm = TRUE),
        sd_anon  = sd  (anon, na.rm = TRUE),
        ks_p_raw = ks_out$p.value,
        ks_D     = ks_out$statistic,
        ks_z     = ks_out$z,
        ks_n_eff = ks_out$n_eff
      )
      
      output$ks_exact_value <- renderUI({
        m <- rv$adhoc_compare_meta
        if (is.null(m) || m$type != "numeric") return(NULL)
        if (is.na(m$ks_p_raw)) {
          HTML("<em>KS test not computed (constant/invalid series).</em>")
        } else {
          htmltools::HTML(paste0(
            "<b>KS p-value (asymptotic; ties present):</b> <code>",
            fmt_p(m$ks_p_raw), "</code>",
            " &nbsp; | &nbsp; <b>D:</b> ",
            format(m$ks_D, digits = 6),
            " &nbsp; | &nbsp; <b>√(n<sub>eff</sub>)·D:</b> ",
            format(m$ks_z, digits = 6)
          ))
        }
      })
      
    } else {
      # ---------- CATEGORICAL / mixed ----------
      orig_chr <- as.character(orig)
      anon_chr <- as.character(anon)
      
      orig_missing <- sum(is.na(orig_chr) | orig_chr == "")
      anon_missing <- sum(is.na(anon_chr) | anon_chr == "")
      
      lv_orig <- unique(orig_chr[!is.na(orig_chr) & nzchar(orig_chr)])
      lv_anon <- unique(anon_chr[!is.na(anon_chr) & nzchar(anon_chr)])
      overlap <- length(intersect(lv_orig, lv_anon)) / max(1L, length(union(lv_orig, lv_anon)))
      
      # Build aligned 2 x K table for Chi-square and Cramér's V
      tab_o <- table(orig_chr, useNA = "no")
      tab_a <- table(anon_chr, useNA = "no")
      levels_u <- union(names(tab_o), names(tab_a))
      tab_o_u <- tab_o[levels_u]; tab_o_u[is.na(tab_o_u)] <- 0L
      tab_a_u <- tab_a[levels_u]; tab_a_u[is.na(tab_a_u)] <- 0L
      tbl <- rbind(Original = as.numeric(tab_o_u), Anonymized = as.numeric(tab_a_u))
      colnames(tbl) <- levels_u
      
      vstats <- cramers_v_tbl(tbl)
      chi_p  <- vstats$p
      v_val  <- vstats$v
      
      # Present a compact result table (and format p safely)
      extra_rows <- data.frame(
        Statistic = c("Total entries (orig)", "Total entries (anon)",
                      "Distinct levels (orig)", "Distinct levels (anon)"),
        Value     = c(length(orig_chr), length(anon_chr),
                      length(lv_orig),   length(lv_anon)),
        check.names = FALSE
      )
      base_rows <- data.frame(
        Statistic = c("Orig missing", "Anon missing", "Missing Δ",
                      "Level overlap", "Cramér's V (effect size)",
                      "Chi-Sq p-value (marginal distribution)"),
        Value     = c(orig_missing, anon_missing, anon_missing - orig_missing,
                      overlap, v_val, fmt_p(chi_p)),
        check.names = FALSE
      )
      
      rv$adhoc_compare_df   <- rbind(extra_rows, base_rows)
      rv$adhoc_compare_meta <- list(
        type       = "categorical",
        missing_delta = anon_missing - orig_missing,
        overlap       = as.numeric(overlap),
        chi_p         = as.numeric(chi_p),
        cramer_v      = as.numeric(v_val)
      )
      
      output$ks_exact_value <- renderUI({ NULL })  # only shown for numeric
    }
    
    output$adhoc_compare_table <- renderDT({
      DT::datatable(rv$adhoc_compare_df, options = list(dom = "t", scrollX = TRUE))
    })
    # =================== SIMPLER, PLAIN-LANGUAGE INTERPRETATION BOX ===================
    output$adhoc_interpret_box <- renderUI({
      m <- rv$adhoc_compare_meta
      if (is.null(m)) return(HTML("<em>No interpretation yet. Run a comparison above.</em>"))
      
      pct <- function(x) if (is.na(x)) "n/a" else paste0(sprintf("%d", round(100 * x)), "%")
      numf <- function(x, d = 2) if (is.na(x)) "n/a" else formatC(x, digits = d, format = "f", drop0trailing = TRUE)
      
      if (identical(m$type, "numeric")) {
        # --- inputs from meta
        D          <- suppressWarnings(as.numeric(m$ks_D))
        z          <- suppressWarnings(as.numeric(m$ks_z))
        mean_shift <- suppressWarnings(as.numeric(m$mean_anon - m$mean_orig))
        sd_shift   <- suppressWarnings(as.numeric(m$sd_anon - m$sd_orig))
        sd_orig    <- suppressWarnings(as.numeric(m$sd_orig))
        
        # --- simple bands
        rel_mean <- if (isTRUE(sd_orig > 0)) abs(mean_shift) / sd_orig else NA_real_
        mean_band <-
          if (is.na(rel_mean)) "unknown"
        else if (rel_mean <= 0.10) "tiny"
        else if (rel_mean <= 0.30) "moderate"
        else "big"
        
        spread_pct <- if (isTRUE(sd_orig != 0)) 100 * (sd_shift / sd_orig) else NA_real_
        spread_band <-
          if (is.na(spread_pct)) "unknown"
        else if (abs(spread_pct) <= 10) "about the same"
        else if (spread_pct > 10) "more spread out"
        else "more squeezed"
        
        shape_band <-
          if (is.na(D)) "unknown"
        else if (D < 0.05) "small"
        else if (D < 0.10) "clear"
        else "large"
        
        # --- one clear verdict (traffic light)
        verdict <-
          if (!is.na(D) && D < 0.05 &&
              (is.na(rel_mean) || rel_mean <= 0.10) &&
              (is.na(spread_pct) || abs(spread_pct) <= 10)) {
            "✅ No meaningful change"
          } else if (!is.na(D) && D < 0.10 &&
                     (is.na(rel_mean) || rel_mean <= 0.30)) {
            "⚠️ Noticeable change"
          } else {
            "❌ Big change"
          }
        
        # --- “is this likely real or just noise?” (no stats words)
        real_change_msg <-
          if (is.na(z)) "n/a"
        else if (z < 1.36) "Probably just random noise"
        else if (z < 1.63) "Likely a real difference"
        else "Almost certainly a real difference"
        
        HTML(paste0(
          "<ul>",
          "<li><b>Verdict:</b> ", verdict, "</li>",
          "<li><b>Typical value (average):</b> changed by ", numf(mean_shift),
          " (", mean_band, ").</li>",
          "<li><b>Spread (how variable):</b> ", spread_band,
          if (!is.na(spread_pct)) paste0(" (", sprintf("%+d%%", round(spread_pct)), ")") else "", ".</li>",
          "<li><b>Overall shape:</b> at most about ", if (!is.na(D)) sprintf("%d%%", round(100 * D)) else "n/a",
          " of the data look shifted.</li>",
          "<li><b>Real change or just noise?</b> ", real_change_msg, ".</li>",
          "</ul>",
          "<small><b>How to read:</b> Green = safe to use as-is. Yellow = keep an eye on it. Red = expect analysis results to change.</small>"
        ))
        
      } else {
        # ------------------------------- categorical -------------------------------
        v        <- suppressWarnings(as.numeric(m$cramer_v))
        p        <- suppressWarnings(as.numeric(m$chi_p))
        overlap  <- suppressWarnings(as.numeric(m$overlap))
        miss_d   <- suppressWarnings(as.numeric(m$missing_delta))
        
        # We’ll estimate “percent extra blanks” against total rows as a simple proxy.
        denom <- tryCatch(nrow(rv$mapped_data), error = function(e) NA_integer_)
        miss_pct <- if (!is.na(denom) && denom > 0 && !is.na(miss_d)) 100 * miss_d / denom else NA_real_
        
        mix_band <-
          if (is.na(v)) "unknown"
        else if (v < 0.10) "small"
        else if (v < 0.30) "medium"
        else "large"
        
        overlap_txt <- if (is.na(overlap)) "n/a" else sprintf("%d%%", round(100 * overlap))
        
        # verdict (traffic light)
        verdict <-
          if ((is.na(v) || v < 0.10) &&
              (is.na(overlap) || overlap >= 0.80) &&
              (is.na(miss_pct) || miss_pct <= 5)) {
            "✅ No meaningful change"
          } else if ((is.na(v) || v < 0.30) &&
                     (is.na(overlap) || overlap >= 0.60) &&
                     (is.na(miss_pct) || miss_pct <= 10)) {
            "⚠️ Noticeable change"
          } else {
            "❌ Big change"
          }
        
        real_change_msg <-
          if (is.na(p)) "n/a"
        else if (p < 0.01) "Almost certainly a real difference"
        else if (p < 0.05) "Likely a real difference"
        else "Probably just random noise"
        
        HTML(paste0(
          "<ul>",
          "<li><b>Verdict:</b> ", verdict, "</li>",
          "<li><b>Category mix:</b> ", mix_band, " shift (common labels got reweighted).</li>",
          "<li><b>Shared labels kept:</b> ", overlap_txt, ".</li>",
          "<li><b>Extra blanks after anonymization:</b> ",
          if (!is.na(miss_d)) paste0("+", miss_d, if (!is.na(miss_pct)) paste0(" (~", sprintf("%d%%", round(miss_pct)), ")") else "") else "n/a",
          ".</li>",
          "<li><b>Real change or just noise?</b> ", real_change_msg, ".</li>",
          "</ul>",
          "<small><b>How to read:</b> Green = categories basically the same. Yellow = some shifts. Red = many labels changed or got removed.</small>"
        ))
      }
    })
    # =============================================================================== 
    
    
    # Empfehlungen (unchanged; nutzt m$type etc.)
    output$adhoc_reco <- renderUI({
      m <- rv$adhoc_compare_meta
      if (is.null(m)) return(HTML("<em>No recommendations yet.</em>"))
      
      if (m$type == "numeric") {
        mean_shift <- m$mean_anon - m$mean_orig
        rel_shift  <- ifelse(isTRUE(m$sd_orig > 0), abs(mean_shift) / m$sd_orig, NA_real_)
        sd_shift   <- m$sd_anon - m$sd_orig
        ks_flag <- !is.na(m$ks_p_raw) && m$ks_p_raw < 0.05
        
        bullets <- c()
        if (!is.na(rel_shift) && rel_shift <= 0.1 && !ks_flag) {
          bullets <- c(bullets, "<b>Fitness:</b> High — anonymization preserves distribution reasonably well.")
        } else if ((!is.na(rel_shift) && rel_shift <= 0.3) || ks_flag) {
          bullets <- c(bullets, "<b>Fitness:</b> Moderate — noticeable distribution changes detected.")
        } else {
          bullets <- c(bullets, "<b>Fitness:</b> Low — strong evidence of distortion; use with caution.")
        }
        if (abs(sd_shift) <= 0.1 * max(1e-9, m$sd_orig)) {
          bullets <- c(bullets, "<b>Utility:</b> Variability largely preserved; most statistical models should remain stable.")
        } else {
          bullets <- c(bullets, "<b>Utility:</b> Variability altered; recalibrate downstream models or consider robust methods.")
        }
        actions <- c(
          "Review anonymization settings for this variable (rounding/jittering/banding).",
          "If KS p-value < 0.05: relax generalization here or adjust k on less critical QIs.",
          "Check important clinical thresholds still classify consistently.",
          "Document observed shifts (mean/variance/KS) in release notes."
        )
        
        HTML(paste0("<ul><li>", paste(bullets, collapse="</li><li>"),
                    "</li></ul><b>Next actions:</b><ul><li>",
                    paste(actions, collapse="</li><li>"), "</li></ul>"))
        
      } else {
        miss_flag <- !is.na(m$missing_delta) && m$missing_delta > 0.05 * nrow(rv$mapped_data)
        overlap_flag <- !is.na(m$overlap) && m$overlap < 0.6
        chi_flag <- !is.na(m$chi_p) && m$chi_p < 0.05
        
        bullets <- c()
        if (!overlap_flag && !chi_flag && !miss_flag) {
          bullets <- c(bullets, "<b>Fitness:</b> High — category structure is well preserved.")
        } else if (overlap_flag || chi_flag) {
          bullets <- c(bullets, "<b>Fitness:</b> Moderate — category frequencies changed; monitor prevalence-sensitive analyses.")
        } else {
          bullets <- c(bullets, "<b>Fitness:</b> Low — loss of categories or heavy suppression detected.")
        }
        if (!miss_flag) {
          bullets <- c(bullets, "<b>Utility:</b> Minimal added missingness; imputations likely unnecessary.")
        } else {
          bullets <- c(bullets, "<b>Utility:</b> Increased missingness; plan simple imputations or sensitivity analyses.")
        }
        actions <- c(
          "Reduce suppression for rare but important categories (grouping instead of removal).",
          "Align recoding lists between original and anonymized to maximize level overlap.",
          "If Chi-sq p < 0.05: review smoothing/PRAM to better preserve marginals.",
          "Publish a value-mapping before/after table."
        )
        
        HTML(paste0("<ul><li>", paste(bullets, collapse="</li><li>"),
                    "</li></ul><b>Next actions:</b><ul><li>",
                    paste(actions, collapse="</li><li>"), "</li></ul>"))
      }
    })
  })
  # ============================================================================== 
  
  
}

shinyApp(ui, server)
