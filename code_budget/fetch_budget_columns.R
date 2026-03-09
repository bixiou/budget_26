# Fetch Budget survey from Qualtrics and write column names to ../data_raw/budget_columns.txt.
# Run from project root: Rscript code_budget/fetch_budget_columns.R
# Or in R: setwd("code_budget"); source("fetch_budget_columns.R")

# Ensure we're in code_budget so source("qualtrics_credential.R") and .Rprofile work
if (basename(getwd()) != "code_budget") { if (file.exists("code_budget/fetch_budget_columns.R")) setwd("code_budget") else if (!file.exists("fetch_budget_columns.R")) stop("Run from project root (parent of code_budget) or from code_budget.") }
if (file.exists(".Rprofile")) source(".Rprofile")
library(qualtRics); library(readr); library(memisc); source("qualtrics_credential.R")

survey_list <- all_surveys()
if (!"Budget" %in% survey_list$name) stop("Survey named 'Budget' not found. Available: ", paste(survey_list$name, collapse = ", "))

e <- fetch_survey(survey_list$id[survey_list$name == "Budget"], include_display_order = TRUE, verbose = TRUE, convert = FALSE, col_types = cols("m" = col_character()))

dir.create("../data_raw", showWarnings = FALSE); dir.create("../data_raw/labels", showWarnings = FALSE)
writeLines(names(e), "../data_raw/budget_columns.txt")
e <- e[, which(!(names(e) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
for (v in names(e)) label(e[[v]]) <- c(v = paste0(v, ": ", gsub("\n", "§", Label(e[[v]]))))
write_csv(e, "../data_raw/Budget.csv", na = ""); saveRDS(label(e), "../data_raw/labels/Budget.rds")
message("Done. Column names written to ../data_raw/budget_columns.txt\nData saved to ../data_raw/Budget.csv and ../data_raw/labels/Budget.rds")
