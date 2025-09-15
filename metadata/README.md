# Metadata — Check Catalog & Concept

This folder ships the metadata concept for the DQ DualSight checks:

- **`check_catalog.xlsx`** — a catalog of data-quality rules grouped by categories such as:
  - Completeness of Coding
  - Age-based Plausibility
  - Gender-based Plausibility
  - Temporal Consistency
  - Diagnosis–Procedure Consistency
  - Code Integrity (ICD/OPS)

Each rule documents (at minimum):
- `check_id` — unique identifier (e.g., `cat4_2`)
- `category` — grouping label
- `description` — short human-readable summary
- `required_data_elements` — fields the rule needs (e.g., `admission_date`, `discharge_date`)
- `implementation_logic` — code or formula used by the app to flag issues
- `severity` — suggested priority for remediation

## How the app uses metadata

- In **Step 4**, the app renders long labels that expose *what the rule flags* and *which fields are required*.
- The server-side logic reads this catalog (and built-ins) to determine which checks can run given the mapping from **Step 2**.
- Custom, study-specific checks can be defined in **Step 3** and are handled similarly.

## Extending the catalog

1. Duplicate a row and assign a new `check_id`.
2. Keep names and categories consistent.
3. Specify `required_data_elements` that must be present after mapping.
4. Provide a test case and expected behavior in an “Examples” or “Notes” column.
5. Version your catalog (e.g., `v1.1`) and track changes in a separate sheet or in `CHANGELOG.md` (optional).

> **Documentation:** A consolidated overview of categories, required elements, and implementation details can be compiled into your study protocol or appendix for auditability.
