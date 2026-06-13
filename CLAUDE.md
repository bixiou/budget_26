# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Security Rules
- You are strictly confined to the current directory.
- Never attempt to read or write files outside of this folder.
- Do not use absolute paths (e.g., C:\...).
- If a task requires accessing external data, ask the user for permission or to provide the file.

## Project Overview

This is an R project for cleansing and analyzing survey data from a French budget/fiscal policy survey ("Budget" survey on Qualtrics). The new single-country (FR) project adapts code from a prior multi-country survey project whose scripts live in `code_budget/former_*.R` as reference templates.

## Workflow / Pipeline

Scripts run in this order:

1. **`code_budget/1_prepare.R`** — Loads external data (`data_ext/budget_policies.xlsx`, `data_ext/sources.xlsx`), defines quota variables and population frequencies for rake-weighting, and defines the `weighting()` function.
2. **`code_budget/2_render.R`** — Defines `labels_vars`, then calls rendering functions to produce figures (output to `figures/`) and Excel exports (output to `xlsx/`).
3. **`code_budget/3_analyse.R`** — Runs regression models and representativeness checks.

The former project's scripts are kept as-is for reference:
- `former_1_rename.R` — reference for `rename_survey()` column-naming logic
- `former_2_prepare.R` — reference for `prepare()` and `convert()` functions, and multi-country parameters
- `former_3_render.R` — reference for `labels_vars` and rendering helpers

## Session Startup

Always start an R session by running `.Rprofile` and loading `.RData` before running any script. These provide shared utility functions and pre-loaded objects used throughout.

## Key Data Files

| Path | Contents |
|------|----------|
| `data_raw/Budget.csv` | Raw survey export from Qualtrics |
| `data_raw/labels_Budget.rds` | Column labels for the raw survey |
| `data_ext/budget_policies.xlsx` | Policy metadata (variable names, amounts, leaning scores) |
| `data_ext/sources.xlsx` | Population quotas (sheet "Quotas") and election results (sheet "Election") |
| `data_ext/codebook.csv` | Variable codebook |

Outputs go to `figures/` (PDF), `xlsx/`, and `tables/` (LaTeX `.tex`).

## Code Style

- Use `snake_case` for all variable and function names.
- Always use the native pipe `|>` (R 4.1+), never `%>%`.
- Prefer compact, single-line expressions where readable.
- Document functions with roxygen2 style.
- Explicitly handle `NA` values; distinguish "Don't know" from "Refusal".

## Key Rules

- Never read or modify `.RData` files or any file listed in `.gitignore`.
- Before writing 500+ lines of code, provide a summary of the logic first.
- After completing a TODO item, tick its checkbox in `TODO.md`.
- When referencing `@Folder`, analyze files to ensure consistency between the cleaning script and the analysis script.
- Qualtrics credentials are in `code_budget/qualtrics_credential.R` (gitignored).
- Don't compile .tex files in `/papers` but in `papers/build/`: there should be no auxiliary files in `/papers`.
- When you export a .csv, always round income values.
