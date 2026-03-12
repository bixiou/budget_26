##### Budget survey: prepare, convert, stats_exclude #####
if (file.exists(".Rprofile")) source(".Rprofile")
source("1_rename.R")
library(readr)
if (!exists("decrit")) decrit <- function(x, ...) print(summary(x))
if (!exists("create_item")) {
  if (!requireNamespace("memisc", quietly = TRUE)) stop("Package memisc required for create_item. Install with install.packages('memisc').")
  library(memisc)
  create_item <- function(var, new_var = var, labels, df, grep = FALSE, keep_original = FALSE, missing.values = NA, values = names(labels), annotation = NULL) {
    if (length(var) > 1) {
      for (v in seq_along(var)) df <- create_item(var[v], new_var = new_var[v], df = df, labels = labels, grep = grep, missing.values = missing.values, values = values, annotation = NULL)
    } else if (var %in% names(df)) {
      if (keep_original) df[[paste0(var, "_original")]] <- df[[var]]
      temp <- NA
      for (i in seq_along(labels)) temp[if (grep) grepl(values[[i]], df[[var]]) else df[[var]] %in% values[[i]]] <- labels[i]
      temp[is.na(df[[var]])] <- NA
      df[[new_var]] <- as.item(temp, labels = labels, missing.values = missing.values, annotation = if (is.null(annotation)) paste0(var, ": ", paste(sapply(names(labels), function(i) paste0(labels[i], ": ", i)), collapse = "; ")) else annotation)
    }
    return(df)
  }
}
if (!exists("no.na")) no.na <- function(vec, rep = "") replace(vec, is.na(vec) | vec == "", rep)
if (!exists("n")) n <- function(var) as.numeric(as.vector(var))

# Load votes table from data_ext/sources.xlsx sheet "Election" (party/candidate -> leaning, major, group). Used for vote_agg, vote_major_candidate, etc.
votes <- list()
votes_path <- if (file.exists("../data_ext/sources.xlsx")) "../data_ext/sources.xlsx" else if (file.exists("data_ext/sources.xlsx")) "data_ext/sources.xlsx" else NULL
if (!is.null(votes_path) && requireNamespace("openxlsx", quietly = TRUE)) {
  sh <- tryCatch(openxlsx::getSheetNames(votes_path), error = function(e) character(0))
  if ("Election" %in% sh) {
    votes_xlsx <- openxlsx::read.xlsx(votes_path, sheet = "Election", cols = 1:6)
    nms <- tolower(names(votes_xlsx))
    col_country <- which(nms %in% "country")[1]
    col_party <- which(nms %in% c("party", "candidate"))[1]
    if (is.na(col_party) && ncol(votes_xlsx) >= 2) col_party <- 2
    if (!is.na(col_country) && !is.na(col_party) && "leaning" %in% nms) {
      for (c in unique(votes_xlsx[[col_country]])) {
        if (is.na(c)) next
        votes[[as.character(c)]] <- votes_xlsx[votes_xlsx[[col_country]] == c, ]
        row.names(votes[[as.character(c)]]) <- as.character(votes[[as.character(c)]][[col_party]])
      }
    }
  }
}

# Manual 1-3 word suffixes for renaming effect_program_*, intl_policy_*, etc. (from question labels).
var_suffix_effect_program <- c(
  "1" = "aide_developpement", "2" = "taxe_milliardaires_onu", "3" = "milliardaires_exoneration",
  "4" = "budget_education_sante", "5" = "allocations_familiales", "6" = "deficit_public_pib",
  "7" = "depenses_fonctionnement", "8" = "aides_sociales_restreindre", "9" = "oqtf_sans_papiers",
  "10" = "regulariser_sans_papiers", "11" = "peines_planchers_recidive", "12" = "retraite_65_ans",
  "13" = "retraite_62_ans", "14" = "smic_10", "15" = "ric_referendum", "16" = "deputes_proportionnelle",
  "17" = "climat_green_deal")
var_suffix_intl_policy <- c(
  "1" = "mondialisation", "2" = "frontieres_ouvertes", "3" = "redistribution_richesses",
  "4" = "citoyens_decisions_mondiales", "5" = "intervention_pays_attaque", "6" = "demilitarisation",
  "7" = "impot_minimum_societes")
var_suffix_group_identified <- c("1" = "ville", "2" = "region", "3" = "france", "4" = "union_europeenne", "5" = "monde")
var_suffix_intl_governance <- c(
  "1" = "elus_chefs_etat", "2" = "referendum_citoyens", "3" = "tirage_sort",
  "4" = "experts_scientifiques", "5" = "parlement_mondial", "6" = "sondages_representatifs")
var_suffix_assembly_outcome <- c(
  "1" = "consultatives_recommandations", "2" = "referendum_mondial",
  "3" = "referendum_pays_accord", "4" = "institutions_internationales")
var_suffix_inheritance_type <- c(
  "1" = "designees_defunt", "2" = "epoux_descendants", "3" = "employes_societe",
  "4" = "etat_actionnaire", "5" = "fonds_citoyens", "6" = "onu_education_sante")

# Rename variables prefix_1, prefix_2, ... to prefix_<suffix> using a mapping (named vector: "1" -> suffix).
rename_vars_by_suffix <- function(e, vars, prefix, suffix_map) {
  if (length(vars) == 0 || is.null(suffix_map)) return(e)
  new_names <- character(length(vars))
  for (i in seq_along(vars)) {
    num <- sub(paste0("^", prefix), "", vars[i])
    suf <- suffix_map[num]
    new_names[i] <- if (!is.na(suf)) paste0(prefix, suf) else vars[i]
  }
  used <- character(0)
  for (i in seq_along(vars)) {
    if (new_names[i] %in% used || new_names[i] %in% names(e)) new_names[i] <- vars[i]
    else used <- c(used, new_names[i])
  }
  for (i in seq_along(vars)) {
    if (new_names[i] != vars[i] && vars[i] %in% names(e)) names(e)[names(e) == vars[i]] <- new_names[i]
  }
  e
}

# Budget prepare: load Budget.csv, filter by scope, rename, convert.
prepare <- function(scope = "final", fetch = FALSE, convert = TRUE, rename = TRUE, duration_min = 360, pilot = FALSE, weighting = FALSE) {
  data_name <- "Budget"
  if (fetch) stop("Fetch not implemented for Budget in this script. Run fetch_budget_columns.R to refresh ../data_raw/Budget.csv.")
  e <- read_csv(paste0("../data_raw/", data_name, ".csv"), guess_max = Inf)
  if ("finished" %in% names(e)) e$Finished <- e$finished
  if ("progress" %in% names(e)) e$Progress <- e$progress
  if ("Finished...7" %in% names(e)) e$Finished <- e$Finished...7
  if ("Q_TerminateFlag" %in% names(e)) e$excluded <- e$Q_TerminateFlag else e$excluded <- NA
  e$finished <- e$Finished %in% c(TRUE, "TRUE", 1, "1")
  e$fast <- if ("Q_TotalDuration" %in% names(e)) n(e$Q_TotalDuration) < duration_min else FALSE
  e$valid <- !e$excluded %in% "QuotaMet"
  e$legit <- is.na(e$excluded)
  e$dropout <- is.na(e$excluded) & !e$finished
  e$stayed <- e$finished & !e$excluded %in% "QuotaMet"
  e$final <- is.na(e$excluded) & e$finished
  if (scope %in% names(e)) e <- e[e[[scope]] == TRUE, ]
  if (rename) e <- rename_survey(e, pilot = pilot)
  if (convert) e <- convert(e)
  e <- e[, !duplicated(names(e))]
  e
}

# Budget convert: cleanse only variables present in Budget (adapted from former_2_prepare.R, absent columns removed).
convert <- function(e) {
  # Load budget policies (id, amount, label, variable_name) from data_ext when needed
  budget_policies <- NULL
  xlsx_path <- if (file.exists("../data_ext/budget_policies.xlsx")) "../data_ext/budget_policies.xlsx" else if (file.exists("data_ext/budget_policies.xlsx")) "data_ext/budget_policies.xlsx" else NULL
  if (!is.null(xlsx_path) && requireNamespace("openxlsx", quietly = TRUE)) {
    budget_policies <- openxlsx::read.xlsx(xlsx_path, sheet = 1)
  }
  # Duration: main duration in minutes
  if ("Q_TotalDuration" %in% names(e)) {
    e$duration <- n(e$Q_TotalDuration) / 60
    label(e$duration) <- "duration: Duration (in min)"
  }
  variables_duration_budget <- c("duration_consent", "duration_sociodemos", "duration_program", "duration_budget", "duration_wtp", "duration_gcs", "duration_wealth_tax", "duration_global_inheritance", "duration_sustainable_future", "duration_top_tax", "duration_comprehension", "duration_custom_redistr", "duration_synthetic", "duration_main_questions")
  for (v in intersect(variables_duration_budget, names(e))) {
    lab <- if (!is.null(attr(e[[v]], "label"))) attr(e[[v]], "label") else NULL
    e[[v]] <- as.numeric(as.vector(gsub("[^0-9.]", "", as.character(e[[v]]))))
    if (!is.null(lab)) label(e[[v]]) <- lab
    e[[v]] <- e[[v]] / 60
  }

  # Numeric (trust/wtp_certainty may have text like "Pas du tout confiance0" or "Complètement certain.e10" -> extract digits)
  for (i in intersect(c("hh_size", "Nb_children__14", "wtp", "income_exact", "wealth"), names(e))) {
    lab <- if (!is.null(attr(e[[i]], "label"))) attr(e[[i]], "label") else NULL
    e[[i]] <- as.numeric(as.vector(gsub("[^0-9.]", "", as.character(e[[i]]))))
    if (!is.null(lab)) label(e[[i]]) <- lab
  }
  if ("trust" %in% names(e)) {
    lab <- attr(e$trust, "label")
    e$trust <- suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(e$trust))))
    e$trust[!e$trust %in% 0:10] <- NA
    if (!is.null(lab)) label(e$trust) <- lab
  }

  # WTP contribution: numeric percentage (0.5, 1, 2, 3, 5, 7, 10...)
  if ("wtp_contribution" %in% names(e)) {
    lab <- attr(e$wtp_contribution, "label")
    x <- gsub(",", ".", as.character(e$wtp_contribution))
    e$wtp_contribution <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
    if (!is.null(lab)) label(e$wtp_contribution) <- lab
  }

  # Binary (couple; home_1..home_4)
  for (j in intersect(c("couple", "home_1", "home_2", "home_3", "home_4"), names(e))) {
    temp <- if (!is.null(attr(e[[j]], "label"))) attr(e[[j]], "label") else NULL
    e[[j]] <- !is.na(e[[j]]) & as.character(e[[j]]) != ""
    if (!is.null(temp)) label(e[[j]]) <- temp
  }

  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% "A little"

  # Socio-demos
  if ("gender" %in% names(e)) {
    e$man <- e$gender %in% c("Man", "Homme")
    label(e$man) <- "man: T/F. gender %in% Man/Homme."
  }
  if ("age_exact" %in% names(e)) {
    e <- create_item("age_exact", new_var = "age", labels = c("18-24" = 21.5, "25-34" = 30, "35-49" = 42.5, "50-64" = 57.5, "65+" = 71),
                     values = list("18 et 20|21 et 24|18 to 20|21 to 24", "25 et 29|30 et 34|25 to 29|30 to 34", "35 et 39|40 et 44|45 et 49|35 to 39|40 to 44|45 to 49", "50 et 54|55 et 59|60 et 64|50 to 54|55 to 59|60 to 64", "65 et 69|70 et 74|75 et 79|80 et 84|85 et 89|90 et 99|100|65 to 69|70 to 74|75 to 79|80 to 84|85 to 89|90 to 99|100 or above"), grep = TRUE, df = e)
    e$age_factor <- relevel(as.factor(e$age), "35-49")
  }
  if ("education" %in% names(e)) e <- create_item("education", labels = c("Below upper secondary" = 1, "Upper secondary" = 2, "Above upper secondary" = 3), grep = TRUE, keep_original = TRUE, values = c("1|2", "3|4", "5|6|7"), df = e)
  if ("income" %in% names(e)) {
    e <- create_item("income", new_var = "income_quartile", labels = c("PNR" = 0, "Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4), values = c("not", "100|200|250", "300|400|500", "600|700|750", "800|900"), grep = TRUE, missing.values = c("PNR"), df = e)
  }
  if ("employment_status" %in% names(e)) e <- create_item("employment_status", new_var = "employment_agg", labels = c("Not working", "Student", "Working", "Retired"), grep = TRUE, values = c("Inactive|Unemployed", "Student", "employed$", "Retired"), df = e)
  if ("urbanity" %in% names(e)) {
    e$urban <- e$urbanity == 1
    e <- create_item("urbanity", labels = c("Cities" = 1, "Towns and suburbs" = 2, "Rural" = 3), grep = TRUE, values = c("1", "2", "3|4"), keep_original = TRUE, missing.values = 0, df = e)
  }
  if ("foreign" %in% names(e)) e <- create_item("foreign", new_var = "foreign_born_family", labels = c("No" = 0, "One parent" = 1, "Two parents" = 2, "Self" = 3), grep = TRUE, values = c("too", "one of", "both", "Yes"), df = e)
  if ("millionaire" %in% names(e)) e <- create_item("millionaire", new_var = "millionaire_agg", labels = c("Unlikely" = -1, "Likely" = 0, "Already" = 1), grep = TRUE, values = c("nlikely", "Very l|Likely", "already"), df = e)

  # Vote (FR): create vote-derived variables using data_ext/sources.xlsx$Election when available (party -> leaning, major)
  if ("voted" %in% names(e)) e$voted <- e$voted %in% "Yes"
  if ("vote_FR" %in% names(e)) e$vote_original <- e$vote_FR
  label(e$vote_original) <- "vote_original: Vote (if voted) or closest candidate (if !voted) in last election."
  if ("voted" %in% names(e)) label(e$voted) <- "voted: T/F Voted in last election."
  if ("vote_original" %in% names(e)) {
    e$vote_voters <- ifelse(e$voted, as.character(e$vote_original), "Non-voter or PNR")
    label(e$vote_voters) <- "vote_voters: Vote if voted else 'Non-voter or PNR'."
    v_orig <- as.character(e$vote_original)
    v_orig_sub <- sub("Prefer not to say", "Other", v_orig)
    use_votes <- "FR" %in% names(votes) && nrow(votes[["FR"]]) > 0 && "leaning" %in% tolower(names(votes[["FR"]]))
    if (use_votes) {
      vtab <- votes[["FR"]]
      nms_v <- tolower(names(vtab))
      col_leaning <- which(nms_v %in% "leaning")[1]
      col_major <- which(nms_v %in% "major")[1]
      idx <- match(v_orig_sub, row.names(vtab))
      e$vote_agg <- ifelse(is.na(v_orig) | v_orig %in% c("Prefer not to say", "Other"), -1, vtab[idx, col_leaning])
      e$vote_agg <- suppressWarnings(as.numeric(e$vote_agg))
      e$vote_agg[is.na(e$vote_agg)] <- -1
      e$vote_major_candidate <- if (is.na(col_major)) NA else (vtab[idx, col_major] %in% c(1, TRUE, "1"))
      e$vote_major <- ifelse(e$vote_major_candidate %in% TRUE, v_orig, "PNR or Other")
      e$vote_major_voters <- ifelse(e$voted, ifelse(e$vote_major %in% "PNR or Other", "Non-voter, PNR or Other", e$vote_major), "Non-voter, PNR or Other")
      e$vote <- ifelse(e$voted, e$vote_agg, -1)
      e$vote_factor <- factor(e$vote, levels = c(-1, 0, 1, 2), labels = c("Non-voter, PNR or Other", "Left", "Center-right or Right", "Far right"))
      e$vote_factor <- relevel(e$vote_factor, "Non-voter, PNR or Other")
    } else {
      e$vote_agg <- v_orig
      e$vote_agg[is.na(e$vote_original) | e$vote_agg %in% c("Prefer not to say", "Other")] <- "PNR or Other"
      e$vote <- ifelse(e$voted, e$vote_agg, "Non-voter, PNR or Other")
      e$vote_factor <- relevel(as.factor(e$vote), "Non-voter, PNR or Other")
      e$vote_major_candidate <- NA
      e$vote_major <- ifelse(e$vote_major_candidate %in% TRUE, v_orig, "PNR or Other")
      e$vote_major_voters <- ifelse(e$voted & !e$vote_major %in% "PNR or Other", as.character(e$vote_major), "Non-voter, PNR or Other")
    }
    label(e$vote_factor) <- "vote_factor: Vote [-1: Non-voter, PNR or Other; else vote_agg/leaning]."
    if ("vote_major_candidate" %in% names(e)) label(e$vote_major_candidate) <- "vote_major_candidate: Vote (or hypothetical vote) for a major candidate."
    if ("vote_major" %in% names(e)) label(e$vote_major) <- "vote_major: Vote if vote_major_candidate else 'PNR or Other'."
    if ("vote_major_voters" %in% names(e)) label(e$vote_major_voters) <- "vote_major_voters: Vote if voted for major candidate else 'Non-voter, PNR or Other'."
  }

  # Likert / create_item for variables present in Budget (already in former convert)
  if ("ncs_support" %in% names(e)) e <- create_item("ncs_support", labels = c("PNR" = -0.1, "No" = 0, "Yes" = 100), values = c("Prefer not to say", "No", "Yes"), missing.values = c("", NA, "PNR"), df = e)
  if ("convergence_support" %in% names(e)) {
    e$convergence_support[is.na(e$convergence_support)] <- "prefer not"
    e <- create_item("convergence_support", labels = c("No" = -1, "PNR" = 0, "Yes" = 1), grep = TRUE, values = c("No", "prefer not", "Yes"), missing.values = c(0, NA), df = e)
  }
  if ("gcs_comprehension" %in% names(e)) e <- create_item("gcs_comprehension", labels = c("decrease" = -1, "not be affected" = 0, "increase" = 1), df = e)
  if ("gcs_comprehension" %in% names(e)) e$gcs_understood <- e$gcs_comprehension == 1
  if ("group_defended" %in% names(e)) e <- create_item("group_defended", labels = c("Family and self" = -2, "Community (region, gender...)" = -1, "Fellow citizens" = 0, "Humans" = 1, "Sentient beings" = 2), grep = TRUE, values = c("family|self", "religion|Community", "Americans|Fellow citizens|citoyens|Citizens", "Humans", "Sentient"), df = e)
  # Tax support: Budget uses Oui/Non (not 5-level Likert)
  if ("solidary_tax_support" %in% names(e)) e <- create_item("solidary_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
  if ("national_tax_support" %in% names(e)) e <- create_item("national_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
  if ("intl_tax_support" %in% names(e)) e <- create_item("intl_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
  if ("sustainable_future_a" %in% names(e)) e$sustainable_future_A <- e$sustainable_future_a == "Scenario A"
  if ("sustainable_future_n" %in% names(e)) e$sustainable_future_B <- e$sustainable_future_n == "Scenario B"
  else if ("sustainable_future_e" %in% names(e)) e$sustainable_future_B <- e$sustainable_future_e == "Scenario B"
  # Sustainable future: variant and aggregated preference for sustainable option
  if ("sustainable_future_A" %in% names(e) || "sustainable_future_B" %in% names(e)) {
    e$variant_sustainable_future <- NA
    if ("sustainable_future_A" %in% names(e)) e$variant_sustainable_future[!is.na(e$sustainable_future_A)] <- "A_sustainable"
    if ("sustainable_future_B" %in% names(e)) e$variant_sustainable_future[!is.na(e$sustainable_future_B)] <- "B_sustainable"
    e$sustainable_future <- NA
    if ("sustainable_future_A" %in% names(e)) e$sustainable_future[!is.na(e$sustainable_future_A)] <- e$sustainable_future_A[!is.na(e$sustainable_future_A)]
    if ("sustainable_future_B" %in% names(e)) e$sustainable_future[!is.na(e$sustainable_future_B)] <- e$sustainable_future_B[!is.na(e$sustainable_future_B)]
  }
  if ("top8_tax_support" %in% names(e)) e <- create_item("top8_tax_support", labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), df = e)
  if ("top5_tax_support" %in% names(e)) e <- create_item("top5_tax_support", labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), df = e)

  # Wealth tax (solidary / national / international): variant and aggregated support
  if (any(c("solidary_tax_support", "national_tax_support", "intl_tax_support") %in% names(e))) {
    e$variant_wealth_tax <- NA
    if ("solidary_tax_support" %in% names(e)) e$variant_wealth_tax[!is.na(e$solidary_tax_support)] <- "solidary"
    if ("national_tax_support" %in% names(e)) e$variant_wealth_tax[!is.na(e$national_tax_support)] <- "national"
    if ("intl_tax_support" %in% names(e)) e$variant_wealth_tax[!is.na(e$intl_tax_support)] <- "intl"
    e$wealth_tax_support <- NA
    if ("solidary_tax_support" %in% names(e)) e$wealth_tax_support[!is.na(e$solidary_tax_support)] <- e$solidary_tax_support[!is.na(e$solidary_tax_support)]
    if ("national_tax_support" %in% names(e)) e$wealth_tax_support[!is.na(e$national_tax_support)] <- e$national_tax_support[!is.na(e$national_tax_support)]
    if ("intl_tax_support" %in% names(e)) e$wealth_tax_support[!is.na(e$intl_tax_support)] <- e$intl_tax_support[!is.na(e$intl_tax_support)]
  }

  # Top tax (top8 vs top5): variant and aggregated support
  if (any(c("top8_tax_support", "top5_tax_support") %in% names(e))) {
    e$variant_top_tax <- NA
    if ("top8_tax_support" %in% names(e)) e$variant_top_tax[!is.na(e$top8_tax_support)] <- "top8"
    if ("top5_tax_support" %in% names(e)) e$variant_top_tax[!is.na(e$top5_tax_support)] <- "top5"
    e$top_tax_support <- NA
    if ("top8_tax_support" %in% names(e)) e$top_tax_support[!is.na(e$top8_tax_support)] <- e$top8_tax_support[!is.na(e$top8_tax_support)]
    if ("top5_tax_support" %in% names(e)) e$top_tax_support[!is.na(e$top5_tax_support)] <- e$top5_tax_support[!is.na(e$top5_tax_support)]
  }

  # Owner from home (Budget: home_1=tenant, home_2=owner, home_3=landlord, home_4=hosted typically)
  if ("home_1" %in% names(e) && "home_2" %in% names(e)) e$owner <- e$home_1 | e$home_2
  if ("home_2" %in% names(e) && !"home_1" %in% names(e)) e$owner <- e$home_2

  # --- Batch 1: effect_program, budget_progressistes (create_item), wtp_certainty (numeric 1-10 only) ---
  # effect_program_1..17: 5-level scale (Beaucoup moins / Moins / Ne changerait rien / Plus / Beaucoup plus)
  variables_effect_program <- grep("^effect_program_[0-9]+$", names(e), value = TRUE)
  if (length(variables_effect_program) > 0) {
    e <- create_item(variables_effect_program, labels = c("Beaucoup moins favorable" = -2, "Moins favorable" = -1, "Ne changerait rien" = 0, "Plus favorable" = 1, "Beaucoup plus favorable" = 2),
                     values = c("Beaucoup moins", "Moins favorable", "Ne changerait rien", "Plus favorable", "Beaucoup plus"), grep = TRUE, df = e)
  }
  e <- rename_vars_by_suffix(e, variables_effect_program, "effect_program_", var_suffix_effect_program)
  # budget_progressistes_1..30: 5-level (Souhaitable / Convenable / Supportable / Inacceptable / Ne sais pas)
  variables_budget_progressistes <- grep("^budget_progressistes_[0-9]+$", names(e), value = TRUE)
  if (length(variables_budget_progressistes) > 0) {
    e <- create_item(variables_budget_progressistes, labels = c("Inacceptable" = -2, "Ne sais pas" = -0.1, "Supportable" = 0, "Convenable" = 1, "Souhaitable" = 2),
                     values = c("Inacceptable", "Ne sais pas", "Supportable", "Convenable", "Souhaitable"), grep = TRUE, missing.values = -0.1, df = e)
  }
  # Rename budget_progressistes_N to variable_name from data_ext/budget_policies.xlsx
  if (!is.null(budget_policies) && "variable_name" %in% names(budget_policies) && "id" %in% names(budget_policies) && "amount" %in% names(budget_policies)) {
    for (i in seq_len(nrow(budget_policies))) {
      id <- budget_policies$id[i]
      new_name <- budget_policies$variable_name[i]
      old_name <- paste0("budget_progressistes_", id)
      if (old_name %in% names(e) && !new_name %in% names(e)) {
        names(e)[names(e) == old_name] <- new_name
      }
    }
    # Sums of amounts (G€) by acceptability: Souhaitable=2, Convenable=1, Supportable=0
    budget_var_names <- intersect(budget_policies$variable_name, names(e))
    if (length(budget_var_names) > 0) {
      amt <- setNames(budget_policies$amount, budget_policies$variable_name)
      e$sum_souhaitable <- rowSums(sapply(budget_var_names, function(v) {
        val <- as.numeric(as.vector(e[[v]]))
        ifelse(!is.na(val) & val == 2, amt[v], 0)
      }), na.rm = TRUE)
      e$sum_convenable <- rowSums(sapply(budget_var_names, function(v) {
        val <- as.numeric(as.vector(e[[v]]))
        ifelse(!is.na(val) & val >= 1, amt[v], 0)
      }), na.rm = TRUE)
      e$sum_supportable <- rowSums(sapply(budget_var_names, function(v) {
        val <- as.numeric(as.vector(e[[v]]))
        ifelse(!is.na(val) & val >= 0, amt[v], 0)
      }), na.rm = TRUE)
      label(e$sum_souhaitable) <- "sum_souhaitable: Sum (G€) of budget policy amounts rated Souhaitable only."
      label(e$sum_convenable) <- "sum_convenable: Sum (G€) of budget policy amounts rated Souhaitable or Convenable."
      label(e$sum_supportable) <- "sum_supportable: Sum (G€) of budget policy amounts rated Souhaitable, Convenable or Supportable."
    }
  }
  # wtp_certainty: keep only numerical 1-10 (values can be "3", "Complètement certain.e10", "Pas du tout certain.e1" -> extract digits)
  if ("wtp_certainty" %in% names(e)) {
    lab <- attr(e$wtp_certainty, "label")
    x <- as.character(e$wtp_certainty)
    e$wtp_certainty <- suppressWarnings(as.numeric(gsub("[^0-9]", "", x)))
    e$wtp_certainty[!e$wtp_certainty %in% 1:10] <- NA
    if (!is.null(lab)) label(e$wtp_certainty) <- lab
  }

  # WTP: variant (contribution level) and aggregated support
  if ("wtp" %in% names(e)) {
    if ("wtp_contribution" %in% names(e)) {
      e$variant_wtp <- e$wtp_contribution
    } else {
      e$variant_wtp <- NA
    }
    e$wtp_support <- e$wtp
  }

  # --- Batch 2: climate_belief, gcs_support_no_info, gcs_support_info ---
  if ("climate_belief" %in% names(e)) e <- create_item("climate_belief",
    labels = c("n'est pas une réalité" = -3, "principalement variabilité" = -2, "autant variabilité qu'activité" = -1, "principalement activité" = 0, "entièrement activité" = 1),
    values = c("n'est pas une r", "principalement dû.*variabilité", "autant dû", "principalement dû.*activité", "entièrement dû"), grep = TRUE, df = e)
  if ("gcs_support_no_info" %in% names(e)) e <- create_item("gcs_support_no_info", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
  if ("gcs_support_info" %in% names(e)) e <- create_item("gcs_support_info", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)

  # GCS branches: variant and aggregated support
  if (any(c("gcs_support_no_info", "gcs_support_info") %in% names(e))) {
    e$variant_gcs <- NA
    if ("gcs_support_no_info" %in% names(e)) e$variant_gcs[!is.na(e$gcs_support_no_info)] <- "no_info"
    if ("gcs_support_info" %in% names(e)) e$variant_gcs[!is.na(e$gcs_support_info)] <- "info"
    e$gcs_support <- NA
    if ("gcs_support_no_info" %in% names(e)) e$gcs_support[!is.na(e$gcs_support_no_info)] <- e$gcs_support_no_info[!is.na(e$gcs_support_no_info)]
    if ("gcs_support_info" %in% names(e)) e$gcs_support[!is.na(e$gcs_support_info)] <- e$gcs_support_info[!is.na(e$gcs_support_info)]
  }

  # --- Batch 3: global_movement (binary), intl_policy (6-level), globalisation ---
  for (v in grep("^global_movement_[0-9]+$", names(e), value = TRUE)) {
    lab <- attr(e[[v]], "label")
    e[[v]] <- !is.na(e[[v]]) & as.character(e[[v]]) != ""
    if (!is.null(lab)) label(e[[v]]) <- lab
  }
  intl_policy_vars <- grep("^intl_policy_[0-9]+$", names(e), value = TRUE)
  if (length(intl_policy_vars) > 0) {
    e <- create_item(intl_policy_vars, labels = c("Très mauvais" = -5, "Mauvais" = -3, "Plutôt mauvais" = -1, "Plutôt bien" = 1, "Bien" = 3, "Très bien" = 5),
                     values = c("Très mauvais", "Mauvais", "Plutôt mauvais", "Plutôt bien", "Bien", "Très bien"), grep = TRUE, df = e)
  }
  e <- rename_vars_by_suffix(e, intl_policy_vars, "intl_policy_", var_suffix_intl_policy)
  if ("globalisation" %in% names(e)) {
    lab <- attr(e$globalisation, "label")
    e$globalisation <- as.numeric(gsub("[^0-9.-]", "", as.character(e$globalisation)))
    if (!is.null(lab)) label(e$globalisation) <- lab
  }

  # --- Batch 4: group_considered, group_helping, group_identified_1..5, group_defended_world ---
  if ("group_considered" %in% names(e)) e <- create_item("group_considered",
    labels = c("Leurs propres intérêts" = -1, "communauté ou pays" = 0, "tous les humains" = 1),
    values = c("Leurs propres", "communauté|pays", "tous les humains"), grep = TRUE, df = e)
  if ("group_helping" %in% names(e)) {
    lab <- attr(e$group_helping, "label")
    e$group_helping <- as.character(e$group_helping)
    if (!is.null(lab)) label(e$group_helping) <- lab
  }
  # group_identified_1..5: 6-level Likert → -5, -3, -1, 1, 3, 5
  variables_group_identified <- grep("^group_identified_[0-9]+$", names(e), value = TRUE)
  if (length(variables_group_identified) > 0) {
    e <- create_item(variables_group_identified, labels = c("Pas du tout" = -5, "Très peu" = -3, "Peu" = -1, "Plutôt" = 1, "Beaucoup" = 3, "Tout à fait" = 5),
                     values = c("Pas du tout", "Très peu", "Peu", "Plutôt", "Beaucoup", "Tout à fait"), grep = TRUE, df = e)
  }
  e <- rename_vars_by_suffix(e, variables_group_identified, "group_identified_", var_suffix_group_identified)
  if ("group_defended_world" %in% names(e)) e <- create_item("group_defended_world",
    labels = c("Ma famille et moi" = -2, "ma communauté" = -1, "Les Français" = 0, "Les humains" = 1, "êtres sentients" = 2),
    values = c("Ma famille et moi", "communauté", "Français", "Les humains", "sentients"), grep = TRUE, df = e)

  # Group defended: variant (baseline vs world) and aggregated scale
  if (any(c("group_defended", "group_defended_world") %in% names(e))) {
    e$variant_group_defended <- NA
    if ("group_defended" %in% names(e)) e$variant_group_defended[!is.na(e$group_defended)] <- "baseline"
    if ("group_defended_world" %in% names(e)) e$variant_group_defended[!is.na(e$group_defended_world)] <- "world"
    e$group_defended_agg <- NA
    if ("group_defended" %in% names(e)) e$group_defended_agg[!is.na(e$group_defended)] <- e$group_defended[!is.na(e$group_defended)]
    if ("group_defended_world" %in% names(e)) e$group_defended_agg[!is.na(e$group_defended_world)] <- e$group_defended_world[!is.na(e$group_defended_world)]
  }

  # --- Batch 5: intl_governance_1..6 (6-level), assembly_outcome_1..4 (5-level), intl_cooperation_1..4 (6-level) ---
  # 6-level Likert: -5, -3, -1, 1, 3, 5
  variables_intl_governance <- grep("^intl_governance_[0-9]+$", names(e), value = TRUE)
  if (length(variables_intl_governance) > 0) {
    e <- create_item(variables_intl_governance, labels = c("Complètement défavorable" = -5, "Défavorable" = -3, "Plutôt défavorable" = -1, "Plutôt favorable" = 1, "Favorable" = 3, "Complètement favorable" = 5),
                     values = c("Complètement défavorable", "Défavorable", "Plutôt défavorable", "Plutôt favorable", "Favorable", "Complètement favorable"), grep = TRUE, df = e)
  }
  e <- rename_vars_by_suffix(e, variables_intl_governance, "intl_governance_", var_suffix_intl_governance)
  variables_assembly_outcome <- grep("^assembly_outcome_[0-9]+$", names(e), value = TRUE)
  if (length(variables_assembly_outcome) > 0) {
    e <- create_item(variables_assembly_outcome, labels = c("Complètement défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Complètement favorable" = 2),
                     values = c("Complètement défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Complètement favorable"), grep = TRUE, df = e)
  }
  e <- rename_vars_by_suffix(e, variables_assembly_outcome, "assembly_outcome_", var_suffix_assembly_outcome)
  intl_cooperation_vars <- grep("^intl_cooperation_[0-9]+$", names(e), value = TRUE)
  if (length(intl_cooperation_vars) > 0) {
    e <- create_item(intl_cooperation_vars, labels = c("Complètement défavorable" = -5, "Défavorable" = -3, "Plutôt défavorable" = -1, "Plutôt favorable" = 1, "Favorable" = 3, "Complètement favorable" = 5),
                     values = c("Complètement défavorable", "Défavorable", "Plutôt défavorable", "Plutôt favorable", "Favorable", "Complètement favorable"), grep = TRUE, df = e)
  }

  # --- Batch 6: inheritance_type_1..6, tax_*, inheritance_400k/1m/10m/1g/100g, tax_millionaires ---
  variables_inheritance_type <- grep("^inheritance_type_[0-9]+$", names(e), value = TRUE)
  for (v in variables_inheritance_type) {
    lab <- attr(e[[v]], "label")
    e[[v]] <- suppressWarnings(as.numeric(as.character(e[[v]])))
    if (!is.null(lab)) label(e[[v]]) <- lab
  }
  e <- rename_vars_by_suffix(e, variables_inheritance_type, "inheritance_type_", var_suffix_inheritance_type)
  variables_tax_policy <- c("tax_business_bequest", "inter_vivo_gifts", "net_wealth_tax", "tax_millionaires")
  variables_tax_policy <- intersect(variables_tax_policy, names(e))
  if (length(variables_tax_policy) > 0) {
    e <- create_item(variables_tax_policy, labels = c("Très défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Très favorable" = 2),
                     values = c("Très défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Très favorable"), grep = TRUE, df = e)
  }
  # Inheritance amounts and implicit tax rates
  inheritance_parents <- c(inheritance_400k = 400000, inheritance_1m = 1e6, inheritance_10m = 1e7,
                           inheritance_1g = 1e9, inheritance_100g = 1e11)
  for (v in intersect(names(inheritance_parents), names(e))) {
    lab <- attr(e[[v]], "label")
    raw <- as.character(e[[v]])
    # Parse amounts in euros, handling millions and milliards explicitly
    amt <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", raw)))
    is_million <- grepl("million", raw, ignore.case = TRUE) & !grepl("milliard", raw, ignore.case = TRUE)
    is_milliard <- grepl("milliard", raw, ignore.case = TRUE)
    amt[is_million & !is.na(amt)] <- amt[is_million & !is.na(amt)] * 1e6
    amt[is_milliard & !is.na(amt)] <- amt[is_milliard & !is.na(amt)] * 1e9
    e[[v]] <- amt
    if (!is.null(lab)) label(e[[v]]) <- lab
    parent_val <- inheritance_parents[[v]]
    new_v <- sub("^inheritance_", "inheritance_tax_", v)
    e[[new_v]] <- ifelse(is.na(e[[v]]), NA, 1 - e[[v]] / parent_val)
  }

  # --- Batch 7: custom_losers, custom_winners, custom_min_income, custom_redistr, custom_slider_*, difficulty_1..4 ---
  for (v in c("custom_losers", "custom_winners", "custom_min_income")) {
    if (v %in% names(e)) {
      lab <- attr(e[[v]], "label")
      txt <- paste0(v, "_1_TEXT")
      raw <- if (txt %in% names(e)) as.character(e[[txt]]) else as.character(e[[v]])
      e[[v]] <- as.numeric(gsub("[^0-9.]", "", raw))
      e[[v]][grepl("NSP|Ne sait pas", raw, ignore.case = TRUE)] <- NA
      if (!is.null(lab)) label(e[[v]]) <- lab
    }
  }
  if ("custom_redistr" %in% names(e)) e <- create_item("custom_redistr",
    labels = c("Très défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Très favorable" = 2),
    values = c("Très défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Très favorable"), grep = TRUE, df = e)
  for (v in c("custom_slider_win", "custom_slider_lose")) {
    if (v %in% names(e)) {
      lab <- attr(e[[v]], "label")
      e[[v]] <- as.numeric(as.character(e[[v]]))
      if (!is.null(lab)) label(e[[v]]) <- lab
    }
  }
  for (v in grep("^difficulty_[0-9]+$", names(e), value = TRUE)) {
    lab <- attr(e[[v]], "label")
    e[[v]] <- !is.na(e[[v]]) & as.character(e[[v]]) != ""
    if (!is.null(lab)) label(e[[v]]) <- lab
  }

  # --- Batch 8: donation_charities, interested_politics, involvement_govt_1 ---
  for (v in c("donation_charities", "interested_politics", "involvement_govt_1")) {
    if (v %in% names(e)) {
      lab <- attr(e[[v]], "label")
      e[[v]] <- as.numeric(gsub("[^0-9.]", "", as.character(e[[v]])))
      if (!is.null(lab)) label(e[[v]]) <- lab
    }
  }

  # Define variable lists (effect_program, intl_policy, group_identified, intl_governance, assembly_outcome, inheritance_type, budget policies)
  define_var_lists(e)

  # Drop variables that are entirely NA
  if (ncol(e) > 0) {
    keep_cols <- vapply(e, function(col) !all(is.na(col)), logical(1L))
    e <- e[, keep_cols, drop = FALSE]
  }

  e
}

# Define global variable lists for Budget (effect_program_*, intl_policy_*, etc.). Call with converted e.
define_var_lists <- function(e) {
  nms <- names(e)
  variables_effect_program <<- grep("^effect_program_", nms, value = TRUE)
  variables_intl_policy <<- grep("^intl_policy_", nms, value = TRUE)
  variables_group_identified <<- grep("^group_identified_", nms, value = TRUE)
  variables_intl_governance <<- grep("^intl_governance_", nms, value = TRUE)
  variables_assembly_outcome <<- grep("^assembly_outcome_", nms, value = TRUE)
  variables_inheritance_type <<- grep("^inheritance_type_", nms, value = TRUE)
  variables_budget_policies <<- grep("^budget_", nms, value = TRUE)
  variables_budget_policies <<- setdiff(variables_budget_policies, c("sum_souhaitable", "sum_convenable", "sum_supportable"))
  variables_sustainable_future <<- setdiff(grep("^sustainable_future", nms, value = TRUE), "variant_sustainable_future")
  variables_group_defended <<- setdiff(grep("^group_defended", nms, value = TRUE), c("variant_group_defended", "group_defended_agg"))
  variables_wealth_tax_support <<- intersect(c("solidary_tax_support", "national_tax_support", "intl_tax_support", "wealth_tax_support"), nms)
  variables_top_tax_support <<- intersect(c("top8_tax_support", "top5_tax_support", "top_tax_support"), nms)
  variables_gcs_support <<- setdiff(grep("^gcs_support", nms, value = TRUE), "variant_gcs")
}

# Adapted from former_2_prepare.R: guards for columns that may be absent in Budget.
stats_exclude <- function(data_name, all = FALSE, old_names = FALSE) {
  cat("\n\nSURVEY:", data_name, "\n\n")
  e <- read_csv(paste0("../data_raw/", data_name, ".csv"), guess_max = Inf)
  if (!old_names) {
    if ("finished" %in% names(e)) e$Finished <- e$finished
    if ("progress" %in% names(e)) e$Progress <- e$progress
  }
  if ("Finished...7" %in% names(e)) e$Finished <- e$Finished...7
  socio_demo_cond <- if ("income" %in% names(e) && "urbanity" %in% names(e) && "region" %in% names(e) && "age_exact" %in% names(e)) (e$income == "Prefer not to say" | e$urbanity %in% 0 | e$region %in% 0 | e$age_exact %in% "Below 18") else rep(FALSE, nrow(e))
  desc_num <- function(x) print(summary(x))
  cat(paste0(nrow(e), " total obs.\n"))
  if ("Q_TerminateFlag" %in% names(e)) {
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "QuotaMet") / nrow(e), 1), "% quota met (incl. socio-demo screened)\n"))
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "QuotaMet" & socio_demo_cond) / nrow(e), 1), "% socio-demo screened\n"))
    if (all) cat(paste0(sum((!e$Q_TerminateFlag %in% "QuotaMet") & socio_demo_cond), " socio-demo screened not flagged as Quota Met\n"))
  }
  if ("income" %in% names(e)) cat(paste0(round(100 * sum(e$income %in% "Prefer not to say") / nrow(e), 1), "% socio-demo screened due to PNR income\n"))
  if ("urbanity" %in% names(e) || "region" %in% names(e)) { urb_or_reg <- (if ("urbanity" %in% names(e)) e$urbanity %in% 0 else FALSE) | (if ("region" %in% names(e)) e$region %in% 0 else FALSE); cat(paste0(round(100 * sum(urb_or_reg) / nrow(e), 1), "% socio-demo screened due to unrecognized zipcode\n")) }
  if (all && "age_exact" %in% names(e)) cat(paste0(round(100 * sum(e$age_exact %in% "Below 18") / nrow(e), 1), "% socio-demo screened due to age < 18\n"))
  if ("Q_TerminateFlag" %in% names(e)) {
    n_valid <- sum(!e$Q_TerminateFlag %in% "QuotaMet"); n_legit <- sum(is.na(e$Q_TerminateFlag))
    cat(paste0(n_valid, " valid: not quota met or socio-demo screened\n"), paste0(sum(e$Q_TerminateFlag %in% "Screened"), " screened after socio-demo\n"))
    if (all) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened") / nrow(e), 1), "% screened in total\n"))
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened") / n_valid, 1), "% screened among valid\n"))
    if ("attention_test" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little")) / n_valid, 1), "% screened among valid due to failed attention_test\n"))
    if ("Q_TotalDuration" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & e$Q_TotalDuration < 360) / n_valid, 1), "% screened among valid due to duration < 360\n"))
    if ("attention_test" %in% names(e) && "Q_TotalDuration" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little") & e$Q_TotalDuration < 360) / n_valid, 1), "% screened among valid due to both reasons\n"))
    if (all) cat(paste0(n_legit, " legit: not quota met nor screened out\n"))
    if (n_legit > 0) { cat(paste0(round(100 * sum(!e$Finished %in% c(TRUE, "TRUE", 1)) / n_legit, 1), "% dropout among legit\n")); if (all) cat(paste0(round(100 * sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag)) / n_legit, 1), "% finished among legit\n")) }
    cat(paste0(sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag)), " final: finished, not quota met nor screened out\n"))
  }
  if (all && "Progress" %in% names(e) && "Q_TerminateFlag" %in% names(e)) { cat("Progress among legit\n"); desc_num(e$Progress[is.na(e$Q_TerminateFlag)]) }
  if ("Q_TerminateFlag" %in% names(e) && "Q_TotalDuration" %in% names(e) && "Finished" %in% names(e)) {
    final_idx <- is.na(e$Q_TerminateFlag) & e$Finished %in% c(TRUE, "TRUE", 1)
    cat("Duration in final sample\n"); if (sum(final_idx) > 0) desc_num(e$Q_TotalDuration[final_idx] / 60)
    if (all && "long" %in% names(e) && sum(final_idx) > 0) print(summary(lm(Q_TotalDuration/60 ~ (long > .42), data = e, subset = final_idx)))
    if (all && "cut" %in% names(e) && "long" %in% names(e)) {
      for (lab in c("cut == 1", "cut == 0", "long > .42", "long < .42")) {
        idx <- if (lab == "cut == 1") e$cut == 1 & final_idx else if (lab == "cut == 0") e$cut == 0 & final_idx else if (lab == "long > .42") e$long > .42 & final_idx else e$long < .42 & final_idx
        cat("Duration in final sample for", lab, "\n"); if (sum(idx, na.rm = TRUE) > 0) desc_num(e$Q_TotalDuration[idx] / 60)
      }
      if (sum(final_idx) > 0) print(summary(lm(Q_TotalDuration/60 ~ (long > .42) * cut, data = e, subset = final_idx)))
    }
  }
  cat("___________________________________________\n")
}

# Export codebook for Budget (Variable, Label, Levels). Does not rely on global 'e'.
export_codebook <- function(data, file = "codebook_Budget.csv", stata = FALSE, keep = NULL, omit = NULL) {
  if (missing(keep)) keep <- seq_along(data)
  if (!missing(omit)) keep <- setdiff(keep, omit)
  data <- data[, keep, drop = FALSE]
  lab_fun <- function(x) {
    a <- attr(x, "label")
    if (is.null(a) || length(a) == 0) "" else as.character(a)[1]
  }
  lvl_fun <- function(x) {
    u <- unique(na.omit(as.character(x)))
    u <- u[u != ""]
    if (length(u) > 50) return(paste0(length(u), " unique values"))
    paste(sort(u), collapse = " | ")
  }
  codebook <- data.frame(
    Variable = names(data),
    Label = vapply(names(data), function(n) lab_fun(data[[n]]), character(1)),
    Levels = vapply(names(data), function(n) lvl_fun(data[[n]]), character(1)),
    stringsAsFactors = FALSE
  )
  readr::write_csv(codebook, file)
  message("Codebook written to ", normalizePath(file, mustWork = FALSE))
  invisible(codebook)
}

# Run codebook export when script is executed (e.g. Rscript 2_prepare.R or source after setwd("code_budget"))
if (file.exists("../data_raw/Budget.csv")) {
  e_cb <- read_csv("../data_raw/Budget.csv", guess_max = Inf, show_col_types = FALSE)
  if (file.exists("../data_raw/labels/Budget.rds")) {
    labs_cb <- readRDS("../data_raw/labels/Budget.rds")
    for (v in names(e_cb)) if (v %in% names(labs_cb)) label(e_cb[[v]]) <- labs_cb[[v]]
  }
  export_codebook(e_cb, "codebook_Budget.csv", stata = FALSE)
}
