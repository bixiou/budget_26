##### Budget survey: render descriptive graphs #####
# Same structure as former_3_render; barres only (no heatmaps, single country). PDFs to ../figures.
if (file.exists(".Rprofile")) source(".Rprofile")
source("2_prepare.R")

# Load Budget data
# e <- prepare(scope = "final", fetch = FALSE, convert = TRUE, rename = TRUE, duration_min = 360, pilot = FALSE, weighting = FALSE)
if (!"weight" %in% names(e)) e$weight <- 1

# Variable sets (variables_*) are set by define_var_lists() inside convert(). Optionally restrict effect_program to main items (exclude DO_).
# variables_effect_program <- setdiff(variables_effect_program, grep("DO_", variables_effect_program, value = TRUE))

##### labels_vars #####
{
labels_vars <- c(
  "country" = "Country", "gender" = "Gender", "age_exact" = "Age", "age" = "Age", "age_factor" = "Age",
  "education" = "Highest diploma", "income" = "Income", "income_quartile" = "Income quartile",
  "employment_status" = "Employment status", "duration" = "Duration", "finished" = "Finished",
  "excluded" = "Excluded", "final" = "Final sample", "weight" = "Weight",
  "vote_original" = "Vote", "vote_factor" = "Vote", "voted" = "Voted",
  "group_defended" = "Group defended", "group_defended_world" = "Group defended (world)",
  "ncs_support" = "National climate scheme", "gcs_support" = "Global climate scheme",
  "convergence_support" = "Convergence support", "sustainable_future" = "Sustainable future",
  "wealth_tax_support" = "Wealth tax support", "top_tax_support" = "Top tax support",
  "sum_souhaitable" = "Sum (G€) Souhaitable", "sum_convenable" = "Sum (G€) Souhaitable or Convenable", "sum_supportable" = "Sum (G€) Souhaitable, Convenable or Supportable",
  setNames(names(e), names(e))
)
for (v in names(e)) { 
  if (grepl("-", Label(e[[v]])) & labels_vars[v] == v) labels_vars[v] <- sub("(.*)- ", "", Label(e[[v]]))
  if (grepl("_control", v) & labels_vars[v] == v) labels_vars[v] <- labels_vars[sub("_control", "", v)]
  if (grepl("TRUE / FALSE", Levels(e[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(e[[v]]), NA)) if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
}
}

##### barres_defs #####
# Sets: name -> list(vars = variables_X, width, height). fill_barres will resolve variables_X from env and add labels, legend, etc.
# Individual vars: name -> list(vars = "single_var", width, height).
barres_defs <- list(
  "effect_program"       = list(vars = variables_effect_program, width = 900, height = 550),
  "intl_policy"          = list(vars = variables_intl_policy, width = 850, height = 450),
  "group_identified"     = list(vars = variables_group_identified, width = 850, height = 400),
  "intl_governance"      = list(vars = variables_intl_governance, width = 900, height = 500),
  "assembly_outcome"     = list(vars = variables_assembly_outcome, width = 850, height = 400),
  "inheritance_type"     = list(vars = variables_inheritance_type, width = 900, height = 500),
  "budget_policies"      = list(vars = variables_budget_policies, width = 950, height = 700),
  "sustainable_future"  = list(vars = variables_sustainable_future, width = 850, height = 450),
  "group_defended"       = list(vars = variables_group_defended, width = 870, height = 500),
  "wealth_tax_support"   = list(vars = variables_wealth_tax_support, width = 850, height = 450),
  "top_tax_support"      = list(vars = variables_top_tax_support, width = 850, height = 450),
  "gcs_support"          = list(vars = variables_gcs_support, width = 850, height = 450),
  "vote_factor"         = list(vars = "vote_factor", width = 850, height = 500),
  "convergence_support"  = list(vars = "convergence_support", width = 700, height = 450),
  "ncs_support"         = list(vars = "ncs_support", width = 600, height = 400)
)

vars_barres <- c()
barres_defs <- fill_barres(vars_barres, barres_defs, df = e)

##### barres_defs_nolabel #####
barres_defs_nolabel <- list(
  "effect_program"       = list(vars = variables_effect_program, width = 980),
  "intl_policy"          = list(vars = variables_intl_policy, width = 900),
  "group_identified"     = list(vars = variables_group_identified, width = 900),
  "intl_governance"      = list(vars = variables_intl_governance, width = 980),
  "assembly_outcome"     = list(vars = variables_assembly_outcome, width = 900),
  "inheritance_type"     = list(vars = variables_inheritance_type, width = 980),
  "budget_policies"      = list(vars = variables_budget_policies, width = 1100),
  "sustainable_future"   = list(vars = variables_sustainable_future, width = 980),
  "group_defended"       = list(vars = variables_group_defended, width = 980),
  "wealth_tax_support"   = list(vars = variables_wealth_tax_support, width = 980),
  "top_tax_support"      = list(vars = variables_top_tax_support, width = 980),
  "gcs_support"         = list(vars = variables_gcs_support, width = 980),
  "vote_factor"          = list(vars = "vote_factor", width = 900),
  "convergence_support"  = list(vars = "convergence_support", width = 500),
  "ncs_support"          = list(vars = "ncs_support", width = 550)
)
barres_defs_nolabel <- fill_barres(c(), barres_defs_nolabel, df = e)

##### Export PDFs to ../figures (not country_comparison) #####
barres_multiple(barres_defs, df = e, folder = "../figures/", method = "webshot", format = "pdf")
barres_multiple(barres_defs_nolabel, df = e, folder = "../figures/", nolabel = TRUE, method = "webshot", format = "pdf")
