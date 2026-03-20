##### Budget survey: render descriptive graphs #####
# Same structure as former_3_render; barres only (no heatmaps, single country). PDFs to ../figures.
# if (file.exists(".Rprofile")) source(".Rprofile")
# source("2_prepare.R")

# Load Budget data
# e <- prepare(scope = "final", fetch = FALSE, convert = TRUE, rename = TRUE, duration_min = 360, pilot = FALSE, weighting = FALSE)
# if (!"weight" %in% names(e)) e$weight <- 1

# Variable sets (variables_*) are set by define_var_lists() inside convert(). Optionally restrict effect_program to main items (exclude DO_).
# variables_effect_program <- setdiff(variables_effect_program, grep("DO_", variables_effect_program, value = TRUE))

##### labels_vars #####
{
labels_vars <- c(
  "country" = "Country",
  "gender" = "Gender",
  "age_exact" = "Age",
  "age" = "Age",
  "age_factor" = "Age",
  "education" = "Highest diploma",
  "income" = "Income",
  "income_quartile" = "Income quartile",
  "employment_status" = "Employment status",
  "duration" = "Duration",
  "finished" = "Finished",
  "excluded" = "Excluded",
  "final" = "Final sample",
  "weight" = "Weight",
  "vote_original" = "Vote",
  "vote_factor" = "Vote",
  "vote_agg" = "Vote",
  "vote" = "Vote",
  "voted" = "Voted",
  "group_defended" = "Group defended",
  "group_defended_world" = "Group defended (world)",
  "ncs_support" = "National climate scheme",
  "gcs_support" = "Global climate scheme",
  "gcs_comprehension" = "Expected effect of global climate scheme",
  "convergence_support" = "Convergence support",
  "sustainable_future" = "Sustainable future",
  "sustainable_future_a" = "Full",
  "sustainable_future_n" = "Symbolic policy",
  "sustainable_future_e" = "None",
  "wealth_tax_support" = "Wealth tax support",
  "solidary_tax_support" = "Support for solidarity wealth tax",
  "national_tax_support" = "Support for national wealth tax",
  "intl_tax_support" = "Support for international wealth tax",
  "top_tax_support" = "Top tax support",
  "top5_tax_support" = "Support for tax on top 5%",
  "top8_tax_support" = "Support for tax on top 8%",
  "tax_business_bequest" = "Tax on business bequests",
  "inter_vivo_gifts" = "Tax on inter-vivos gifts",
  "net_wealth_tax" = "Net wealth tax",
  "tax_millionaires" = "Tax on millionaires",
  "climate_belief" = "Climate change belief",
  "group_considered" = "Group considered in decisions",
  "wtp" = "Willingness to pay",
  "wtp_0.5" = "WTP at 0.5% of income",
  "wtp_1" = "WTP at 1% of income",
  "wtp_2" = "WTP at 2% of income",
  "wtp_3" = "WTP at 3% of income",
  "wtp_5" = "WTP at 5% of income",
  "wtp_7" = "WTP at 7% of income",
  "wtp_10" = "WTP at 10% of income",
  "wtp_certainty" = "WTP certainty",
  "gcs_support_info" = "Global climate scheme (with info)",
  "gcs_support_no_info" = "Global climate scheme (no info)",
  "custom_redistr" = "Global income redistribution",
  "custom_redistr_among_affected" = "Global income redistribution (among affected)",
  "custom_redistr_among_non_affected" = "Global income redistribution (among non-affected)",
  "custom_losers" = "Preferred share of losers",
  "custom_winners" = "Preferred share of winners",
  "custom_min_income" = "Preferred minimum income",
  "custom_losers_agg" = "Preferred share of losers",
  "custom_winners_agg" = "Preferred share of winners",
  "custom_min_income_agg" = "Preferred minimum income",
  "custom_slider_losers" = "Preferred share of losers",
  "custom_slider_winners" = "Preferred share of winners",
  # "effect_program_reduire_aide_developpement" = "Effect: reduce development aid",
  # "effect_program_taxe_millionaires_onu" = "Effect: UN tax on millionaires",
  # "effect_program_fin_dutreil" = "Effect: end Dutreil pact",
  # "effect_program_education_sante" = "Effect: invest in education & health",
  # "effect_program_augmenter_allocs_familiales" = "Effect: increase family benefits",
  # "effect_program_reduire_deficit" = "Effect: reduce public deficit",
  # "effect_program_reduire_depenses_fonctionnement" = "Effect: reduce operating expenses",
  # "effect_program_restreindre_aides_etrangers" = "Effect: restrict aid to foreigners",
  # "effect_program_appliquer_oqtf" = "Effect: enforce return orders (OQTF)",
  # "effect_program_regulariser_sans_papiers" = "Effect: regularize undocumented migrants",
  # "effect_program_peines_planchers_recidive" = "Effect: mandatory minimum sentences",
  # "effect_program_retraite_65_ans" = "Effect: retirement at 65",
  # "effect_program_retraite_62_ans" = "Effect: retirement at 62",
  # "effect_program_augmenter_smic" = "Effect: increase minimum wage (SMIC)",
  # "effect_program_ric" = "Effect: citizens' initiative referendum (RIC)",
  # "effect_program_proportionnelle" = "Effect: proportional representation",
  # "effect_program_maintenir_green_deal" = "Effect: maintain Green Deal",
  # "intl_policy_mondialisation" = "View on globalization",
  # "intl_policy_frontieres_ouvertes" = "View on open borders",
  # "intl_policy_redistribution_richesses" = "View on global wealth redistribution",
  # "intl_policy_citoyens_decisions" = "View on citizens in global decisions",
  # "intl_policy_intervention_pays_attaque" = "View on intervention when a country is attacked",
  # "intl_policy_demilitarisation" = "View on global demilitarisation",
  # "intl_policy_impot_minimum_societes" = "View on minimum tax on corporations",
  # "group_identified_ville" = "Identification with city",
  # "group_identified_region" = "Identification with region",
  # "group_identified_france" = "Identification with France",
  # "group_identified_ue" = "Identification with European Union",
  # "group_identified_monde" = "Identification with world",
  # "intl_governance_elus_chefs_etat" = "Global governance: elected heads of state",
  # "intl_governance_referendum_citoyens" = "Global governance: citizens' referendums",
  # "intl_governance_tirage_sort" = "Global governance: sortition",
  # "intl_governance_experts_scientifiques" = "Global governance: scientific experts",
  # "intl_governance_parlement_mondial" = "Global governance: world parliament",
  # "intl_governance_sondages_consultatifs" = "Global governance: consultative polls",
  # "assembly_outcome_consultatives_recommandations" = "Assembly outcome: consultative recommendations",
  # "assembly_outcome_referendum_mondial" = "Assembly outcome: world referendum",
  # "assembly_outcome_referendum_pays_par_pays" = "Assembly outcome: referendum country by country",
  # "assembly_outcome_appliquees_institutions_inter" = "Assembly outcome: implemented by international institutions",
  # "inheritance_type_designees_defunt" = "Heirs: designated by deceased",
  # "inheritance_type_epoux_descendants" = "Heirs: spouse and descendants",
  # "inheritance_type_employes_societe" = "Heirs: company employees",
  # "inheritance_type_etat_actionnaire" = "Heirs: state shareholder",
  # "inheritance_type_fonds_citoyens" = "Heirs: citizens' fund",
  # "inheritance_type_onu_education_sante" = "Heirs: UN education & health",
  "group_defended_base" = "Group defended (baseline)",
  # "inheritance_agg_designees_defunt" = "Effective tax: designated by deceased",
  # "inheritance_agg_epoux_descendants" = "Effective tax: spouse and descendants",
  # "inheritance_agg_employes_societe" = "Effective tax: company employees",
  # "inheritance_agg_etat_actionnaire" = "Effective tax: state shareholder",
  # "inheritance_agg_fonds_citoyens" = "Effective tax: citizens' fund",
  # "inheritance_agg_onu_education_sante" = "Effective tax: UN education & health",
  "inheritance_tax_400k" = "Inheritance tax rate at 400k€",
  "inheritance_tax_1m" = "Inheritance tax rate at 1M€",
  "inheritance_tax_10m" = "Inheritance tax rate at 10M€",
  "inheritance_tax_1g" = "Inheritance tax rate at 1G€",
  "inheritance_tax_100g" = "Inheritance tax rate at 100G€",
  "inheritance_tax_agg_400k" = "Inheritance tax rate at 400k€",
  "inheritance_tax_agg_1m" = "Inheritance tax rate at 1M€",
  "inheritance_tax_agg_10m" = "Inheritance tax rate at 10M€",
  "inheritance_tax_agg_1g" = "Inheritance tax rate at 1G€",
  "inheritance_tax_agg_100g" = "Inheritance tax rate at 100G€",
  "sum_souhaitable" = "Sum (G€) Souhaitable",
  "sum_convenable" = "Sum (G€) Souhaitable or Convenable",
  "sum_supportable" = "Sum (G€) Souhaitable, Convenable or Supportable",
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
barres_defs_label <- list(
  "custom_losers_agg"    = list(vars = "custom_losers_agg", width = 850, height = 450),
  "custom_winners_agg"   = list(vars = "custom_winners_agg", width = 850, height = 450),
  "custom_min_income_agg"= list(vars = "custom_min_income_agg", width = 850, height = 450),
  # "climate_belief"       = list(vars = "climate_belief", width = 850, height = 450),
  "group_considered"     = list(vars = "group_considered", width = 850, height = 450),
  "gcs_comprehension"    = list(vars = "gcs_comprehension", width = 850, height = 450),
  # "vote_agg"             = list(vars = "vote_agg", width = 850, height = 500),
  # "vote"                 = list(vars = "vote", width = 850, height = 500),
  "wtp_certainty"        = list(vars = "wtp_certainty", width = 850, height = 450),
  "custom_redistr"       = list(vars = "custom_redistr", width = 900),
  "custom_redistr_all"   = list(vars = variables_custom_redistr_all, width = 850, height = 450)
  # "difficulty"           = list(vars = variables_difficulty, width = 850, height = 450),
)
barres_defs_label <- fill_barres(c(), barres_defs_label, df = e)
barres_defs_label <- fill_barres(c("custom_losers_agg", "custom_winners_agg", "custom_min_income_agg","group_considered",  "wtp_certainty", "custom_redistr", "custom_redistr_all"), barres_defs_label, df = e)

##### barres_defs_nolabel #####
barres_defs <- list(
  "vote_agg"             = list(vars = "vote_agg", width = 850, height = 500, miss = T), 
  "vote"                 = list(vars = "vote", width = 850, height = 500, miss = T),
  "climate_belief"       = list(vars = "climate_belief", width = 1300),
  "custom_losers_agg"    = list(vars = "custom_losers_agg", width = 900),
  "custom_winners_agg"   = list(vars = "custom_winners_agg", width = 900),
  "custom_min_income_agg"= list(vars = "custom_min_income_agg", width = 900),
  "effect_program"       = list(vars = variables_effect_program, width = 980),
  "budget"               = list(vars = variables_budget, width = 1100, height = 1500, miss = T),
  "top_tax_support"      = list(vars = variables_top_tax_support, width = 980),
  "wtp"                  = list(vars = variables_wtp, width = 900),
  "inheritance_tax_agg"  = list(vars = variables_inheritance_tax_agg, width = 980),
  "wealth_tax_support"   = list(vars = variables_wealth_tax_support, width = 980),
  # "inheritance_type"     = list(vars = variables_inheritance_type, width = 980),
  "intl_policy"          = list(vars = variables_intl_policy, width = 900),
  "group_identified"     = list(vars = variables_group_identified, width = 900),
  "intl_governance"      = list(vars = variables_intl_governance, width = 900, height = 500),
  "assembly_outcome"     = list(vars = variables_assembly_outcome, width = 900),
  "sustainable_future"   = list(vars = variables_sustainable_future, width = 980),
  "group_defended"       = list(vars = variables_group_defended, width = 980),
  "gcs_support"          = list(vars = variables_gcs_support, width = 980),
  "tax_policy"           = list(vars = variables_tax_policy, width = 980),
  "inheritance_agg"      = list(vars = variables_inheritance_agg, width = 980),
  "group_considered"     = list(vars = "group_considered", width = 900),
  "gcs_comprehension"    = list(vars = "gcs_comprehension", width = 900),
  "custom_redistr"       = list(vars = "custom_redistr", width = 900),
  "custom_redistr_all"   = list(vars = variables_custom_redistr_all, width = 900),
  "difficulty"           = list(vars = variables_difficulty, width = 900),
  "wtp_certainty"        = list(vars = "wtp_certainty", width = 900)
)
barres_defs <- fill_barres(c(), barres_defs, df = e)


##### Export PDFs to ../figures (not country_comparison) #####
barres_multiple(barres_defs, df = e) 
barres_multiple(barres_defs_label, df = e, nolabel = FALSE) 

barres_defs[["budget"]]$legend[5] <- "Ne sais pas"
# barres_defs[["budget"]]$width <- 1100
barres_defs[["budget"]]$labels <- break_strings(paste0(labels_vars[variables_budget], ": ", sub(".", ",", as.character(budget_policies_amounts), fixed = T), " Mds"), 64)
sum(grepl("<br>.*<br", barres_defs[["budget"]]$labels))
# barres_defs[["budget"]]$labels[1] <- paste(barres_defs[["budget"]]$labels[1], "Mds €")
# barres_defs[["budget"]]$labels[17] <- break_strings(labels_vars[variables_budget[17]], 90)
barres_multiple(barres_defs["budget"], df = e, weights = F, nolabel = TRUE, format = "pdf")

barres_multiple(barres_defs["vote"], df = e) 

##### Budget policy acceptability table #####
{
  bp <- budget_policies[, c("variable_name", "amount", "label")]
  bp$souhaitable <- bp$conv_souh <- bp$supp_conv_souh <- bp$souhaitable_xpnr <- bp$conv_souh_xpnr <- bp$supp_conv_souh_xpnr <- NA
  for (i in seq_len(nrow(bp))) {
    v <- bp$variable_name[i]
    # bp$souhaitable[i]      <- wtd.mean(e[[v]] == 2, e$weight)
    # bp$conv_souh[i]        <- wtd.mean(e[[v]] >= 1, e$weight)
    # bp$supp_conv_souh[i]   <- wtd.mean(e[[v]] >= 0, e$weight)
    # bp$souhaitable_xpnr[i]    <- wtd.mean(e[[v]] == 2, e$weight * !is.missing(e[[v]]))
    # bp$conv_souh_xpnr[i]      <- wtd.mean(e[[v]] >= 1, e$weight * !is.missing(e[[v]]))
    # bp$supp_conv_souh_xpnr[i] <- wtd.mean(e[[v]] >= 0, e$weight * !is.missing(e[[v]]))
    bp$souhaitable[i]      <- wtd.mean(e[[v]] == 2, 1)
    bp$conv_souh[i]        <- wtd.mean(e[[v]] >= 1, 1)
    bp$supp_conv_souh[i]   <- wtd.mean(e[[v]] >= 0, 1)
    bp$souhaitable_xpnr[i]    <- wtd.mean(e[[v]] == 2, !is.missing(e[[v]]))
    bp$conv_souh_xpnr[i]      <- wtd.mean(e[[v]] >= 1, !is.missing(e[[v]]))
    bp$supp_conv_souh_xpnr[i] <- wtd.mean(e[[v]] >= 0, !is.missing(e[[v]]))
  }
  bp <- bp[order(-bp$conv_souh_xpnr), ]
  bp$cum_conv_souh <- cumsum(ifelse(is.na(bp$conv_souh_xpnr), 0, bp$amount))
  names(bp) <- c("variable", "amount", "label", "supp+conv+souh xPNR", "conv+souh xPNR", "souh xPNR", "supp+conv+souh", "conv+souh", "souhaitable", "cum conv+souh")
  num_cols <- 4:10
  bp[, num_cols] <- round(bp[, num_cols], 3)
  write.csv(bp, "../tables/budget_policy_table.csv", row.names = FALSE)
  print(bp[, c("variable", "amount", "souhaitable", "conv+souh", "supp+conv+souh", "souh xPNR", "conv+souh xPNR", "supp+conv+souh xPNR", "cum conv+souh")])
}

budget_majorite_convenable <- bp$variable[bp$`conv+souh xPNR` > 0.5]
budget_majorite_souhaitable <- bp$variable[bp$`souh xPNR` > 0.5]
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_convenable]) # 5 (37.5G€) hurt the rich, 3 shrink welfare state (26.7G€), 3 sectoral (24.7G€), 2 hurt foreigners (8G€)
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_souhaitable]) # 2 hurt the rich (14.5G€), 2 hurt foreigners (8G€), 1 sectoral (9G€)
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_convenable], weights = budget_policies$amount[budget_policies$variable_name %in% budget_majorite_convenable])
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_souhaitable], weights = budget_policies$amount[budget_policies$variable_name %in% budget_majorite_souhaitable])
