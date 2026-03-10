# Inspect every column of Budget.csv: unique values and type, for cleansing logic.
# Run from project root: Rscript code_budget/inspect_budget_columns.R
# Or from code_budget: Rscript inspect_budget_columns.R
if (basename(getwd()) != "code_budget") setwd("code_budget")
if (!file.exists("../data_raw/Budget.csv")) stop("../data_raw/Budget.csv not found. Run fetch_budget_columns.R first.")
library(readr)
e <- read_csv("../data_raw/Budget.csv", guess_max = Inf, show_col_types = FALSE)
out <- character(0)
for (nm in names(e)) {
  x <- e[[nm]]
  u <- unique(na.omit(as.character(x)))
  u <- u[u != ""]
  n_u <- length(u)
  if (n_u <= 30) {
    out <- c(out, paste0("\n=== ", nm, " (", class(x)[1], ", n_unique=", n_u, ") ==="))
    out <- c(out, paste(sort(u), collapse = " | "))
  } else {
    out <- c(out, paste0("\n=== ", nm, " (", class(x)[1], ", n_unique=", n_u, ") ==="))
    out <- c(out, paste(head(sort(u), 20), collapse = " | "))
    out <- c(out, "... (truncated)")
  }
}
writeLines(out, "../data_raw/budget_column_inspection.txt")
message("Written ../data_raw/budget_column_inspection.txt")
