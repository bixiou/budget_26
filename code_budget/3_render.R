##### Budget survey: render descriptive graphs #####
source("2_prepare.R")

# Load Budget data (fetch = FALSE if already in ../data_raw/Budget.csv)
e <- prepare(scope = "final", fetch = FALSE, convert = TRUE, rename = TRUE, duration_min = 360, pilot = FALSE, weighting = FALSE)

labels_vars <- c("country" = "Country", "gender" = "Gender", "age_exact" = "Age", "age" = "Age", "age_factor" = "Age", "education" = "Highest diploma", "income" = "Income", "income_quartile" = "Income quartile", "employment_status" = "Employment status", "duration" = "Duration", "finished" = "Finished", "excluded" = "Excluded", "final" = "Final sample", "weight" = "Weight")

# Add decrit(), bar charts, or heatmaps for variables in e as needed (see former_3_render.R for patterns).
