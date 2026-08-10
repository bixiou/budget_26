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
  "tax_millionaires" = "Êtes-vous favorable à taxer les millionnaires si cela conduit une partie d'entre eux à s'expatrier et ainsi réduit le PIB et les recettes fiscales à long terme ?",
  "climate_belief" = "Climate change belief",
  "group_considered" = "Lorsque les citoyens s'engagent en politique, quels principaux éléments devraient-ils prendre en compte ?", # "Group considered in decisions", 
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
  "group_defended_base" = "Group defended (baseline)",
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
  # "inheritance_agg_designees_defunt" = "Effective tax: designated by deceased",
  # "inheritance_agg_epoux_descendants" = "Effective tax: spouse and descendants",
  # "inheritance_agg_employes_societe" = "Effective tax: company employees",
  # "inheritance_agg_etat_actionnaire" = "Effective tax: state shareholder",
  # "inheritance_agg_fonds_citoyens" = "Effective tax: citizens' fund",
  # "inheritance_agg_onu_education_sante" = "Effective tax: UN education & health",
  setNames(names(e), names(e))
)

labels_vars_en <- c("intl_policy_mondialisation" = "Globalization, that is to say the increase in interactions and interdependence between the peoples and countries of the world, is...", 
                    "intl_policy_frontieres_ouvertes" = "A world with increasingly open borders would be...", 
                    "intl_policy_redistribution_richesses" = "A greater redistribution of wealth from rich countries to the poorest countries in the world would be...", 
                    "intl_policy_citoyens_decisions" = "All the citizens of the world participating directly in decisions with global stakes would be...", 
                    "intl_policy_intervention_pays_attaque" = "The intervention of the rest of the world when one country attacks another would be...", 
                    "intl_policy_demilitarisation" = "The countries of the world demilitarizing by reducing their armies and arsenals would be...", 
                    "intl_policy_impot_minimum_societes" = "The establishment of a minimum corporate tax of 35% on multinational companies to finance education and health worldwide would be...", 
                    "group_identified_ville" = "My city", 
                    "group_identified_region" = "My region", 
                    "group_identified_france" = "France", 
                    "group_identified_ue" = "The European Union", 
                    "group_identified_monde" = "The world", 
                    "intl_governance_elus_chefs_etat" = "Elected officials and heads of state should make the decisions", 
                    "intl_governance_referendum_citoyens" = "Citizens should decide directly by referendum", 
                    "intl_governance_tirage_sort" = "Citizens should be selected by lot for citizens' assemblies whose decisions would then be implemented by national parliaments", 
                    "intl_governance_experts_scientifiques" = "Experts chosen by the scientific community should make the decisions", 
                    "intl_governance_parlement_mondial" = "A world parliament, directly elected by the global population, should make most decisions", 
                    "intl_governance_sondages_consultatifs" = "Regular representative polls aimed at establishing global public opinion on international issues should serve as information for policymakers", 
                    "assembly_outcome_consultatives_recommandations" = "Consultative and serve as recommendations to countries, which would retain the final decision", 
                    "assembly_outcome_referendum_mondial" = "Submitted to a worldwide referendum and implemented in countries where the 'Yes' vote wins", 
                    "assembly_outcome_referendum_pays_par_pays" = "Submitted to a referendum and implemented in all countries if the 'Yes' vote wins at the global level", 
                    "assembly_outcome_appliquees_institutions_inter" = "Implemented directly by international institutions",
                    "group_considered" = "When citizens get involved in politics, what key factors should they consider?",
                    # effect_program
                    "effect_program_reduire_aide_developpement"      = "Reduce development aid for low-income countries",
                    "effect_program_taxe_millionaires_onu"           = "Propose a UN international tax on millionaires, 30% of which would fund health and education in low-income countries",
                    "effect_program_fin_dutreil"                     = "End the inheritance tax exemption for billionaires (Dutreil pact)",
                    "effect_program_education_sante"                 = "Increase the national education and health budget",
                    "effect_program_augmenter_allocs_familiales"     = "Increase family allowances",
                    "effect_program_reduire_deficit"                 = "Reduce the public deficit below 3% of GDP and stabilize public debt by 2032",
                    "effect_program_reduire_depenses_fonctionnement" = "Significantly reduce the state's operating expenditure",
                    "effect_program_restreindre_aides_etrangers"     = "Restrict access to welfare benefits, medical aid, and social housing for foreigners",
                    "effect_program_appliquer_oqtf"                  = "Systematically enforce deportation orders (OQTF)",
                    "effect_program_regulariser_sans_papiers"        = "Regularize undocumented workers in shortage occupations",
                    "effect_program_peines_planchers_recidive"       = "Introduce mandatory minimum sentences for repeat offenders and criminal liability from age 16",
                    "effect_program_retraite_65_ans"                 = "Raise the legal retirement age to 65",
                    "effect_program_retraite_62_ans"                 = "Restore the legal retirement age to 62",
                    "effect_program_augmenter_smic"                  = "Raise the minimum wage (SMIC) by 10%, to €1,600 net per month",
                    "effect_program_ric"                             = "Introduce the Citizens' Initiative Referendum (RIC) for any proposal gathering 1 million signatures",
                    "effect_program_proportionnelle"                 = "Elect MPs by proportional representation",
                    "effect_program_maintenir_green_deal"            = "Maintain Green Deal climate policies: carbon price on oil and gas, phase-out of combustion engine vehicles",
                    # budget
                    "budget_aligner_tva_restauration"                        = "Align the restaurant VAT rate (10%) with the standard rate (20%)",
                    "budget_augmenter_age_retraite_65"                       = "Raise the legal retirement age from 64 to 65, except for long careers or arduous work",
                    "budget_augmenter_cotisations_salaires_moyens"           = "Increase contributions on average wages (reduce CICE payroll tax cuts)",
                    "budget_augmenter_csg_1pt"                               = "Raise the CSG rate by one point (a tax applying to almost all income)",
                    "budget_augmenter_duree_travail_droit_chomage"           = "Extend the qualifying work period for unemployment benefits by 4 months",
                    "budget_augmenter_impot_heritages_eleves"                = "Increase inheritance tax for the top 10% of estates",
                    "budget_augmenter_impot_revenu_aises"                    = "Raise income tax on the wealthy by adding higher brackets, with a top rate of 65%",
                    "budget_augmenter_impot_revenu_tous"                     = "Raise income tax rates by one point and lower thresholds to broaden the taxpayer base",
                    "budget_augmenter_impot_societes"                        = "Raise the corporate tax rate from 25% to 33.5%",
                    "budget_augmenter_taxe_revenus_capital"                  = "Raise the capital income tax from 30% to 33%",
                    "budget_augmenter_tva_1pt"                               = "Raise the standard VAT rate by one point (from 20% to 21%)",
                    "budget_diminuer_credit_impot_recherche"                 = "Reduce the research tax credit for large companies",
                    "budget_diminuer_subventions_ecole_privee"               = "Cut private school subsidies by one third",
                    "budget_eliminer_doublons_territoriaux"                  = "Eliminate overlaps between territorial levels",
                    "budget_geler_aides_sociales"                            = "Freeze welfare benefits (excl. housing aid), cut housing aid by 5% and restrict it to low-income households",
                    "budget_geler_depenses_etat_collectivites"               = "Freeze state and local authority spending for two years",
                    "budget_reduire_aides_apprentissage"                     = "Reduce subsidies to firms for apprenticeship contracts",
                    "budget_reduire_depenses_educatives_demographie"         = "Reduce education spending in line with demographic decline",
                    "budget_reduire_depenses_militaires"                     = "Reduce military spending (cancel the planned increase)",
                    "budget_reduire_pensions_retraite"                       = "Reduce the annual pension indexation by 0.5 points for 4 years",
                    "budget_reduire_remboursement_soins"                     = "Reduce reimbursement of certain healthcare costs, sick pay, and patient transport",
                    "budget_restaurer_taxe_habitation_aises"                 = "Restore the housing tax for the wealthiest 20%",
                    "budget_retablir_isf"                                    = "Reinstate a strengthened wealth tax for millionaires (with a 2% rate for billionaires)",
                    "budget_retirer_aides_sociales_etrangers"                = "Remove welfare benefits from non-European foreigners (RSA, family allowances, housing aid, minimum pension)",
                    "budget_soumettre_livret_a_impot"                        = "Subject Livret A and LDDS savings account interest to income tax and social levies",
                    "budget_supprimer_abattement_ir_retraites"               = "Remove the 10% income tax allowance on pensions",
                    "budget_supprimer_ame"                                   = "Abolish the State Medical Aid covering emergency care for undocumented migrants",
                    "budget_supprimer_avantages_fiscaux_complements_salaire" = "Remove tax advantages on pay supplements (profit-sharing, meal vouchers, etc.)",
                    "budget_supprimer_exonerations_taxes_carburants"         = "Remove tax exemptions on fuel in maritime, aviation, road, and farming sectors, and raise the air ticket tax",
                    "budget_tva_luxe"                                        = "Create a 25% VAT rate for luxury goods (watches, yachts, sports cars...)"
)
for (v in names(e)) { 
  if (grepl("-", Label(e[[v]])) & labels_vars[v] == v) labels_vars[v] <- sub("(.*)- ", "", Label(e[[v]]))
  if (grepl("_control", v) & labels_vars[v] == v) labels_vars[v] <- labels_vars[sub("_control", "", v)]
  if (grepl("TRUE / FALSE", Levels(e[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(e[[v]]), NA)) if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
}
}



##### barres_defs #####
barres_defs_label <- list(
  "custom_min_income_agg"= list(vars = "custom_min_income_agg", width = 1100)
  # "custom_losers_agg"    = list(vars = "custom_losers_agg", width = 850, height = 450),
  # "custom_winners_agg"   = list(vars = "custom_winners_agg", width = 850, height = 450),
  # "group_considered"     = list(vars = "group_considered", width = 850, height = 450),
  # "gcs_comprehension"    = list(vars = "gcs_comprehension", width = 850, height = 450),
  # "wtp_certainty"        = list(vars = "wtp_certainty", width = 850, height = 450),
  # "custom_redistr"       = list(vars = "custom_redistr", width = 900),
  # # "climate_belief"       = list(vars = "climate_belief", width = 850, height = 450),
  # # "vote_agg"             = list(vars = "vote_agg", width = 850, height = 500),
  # # "vote"                 = list(vars = "vote", width = 850, height = 500),
  # # "custom_redistr_all"   = list(vars = variables_custom_redistr_all, width = 850, height = 450)
  # # "difficulty"           = list(vars = variables_difficulty, width = 850, height = 450),
)
# barres_defs_label <- fill_barres(c(), barres_defs_label, df = e)
barres_defs_label <- fill_barres(c("custom_losers_agg", "custom_winners_agg", "custom_min_income_agg","group_considered", "tax_millionaires", "wtp_certainty", "custom_redistr"), barres_defs_label, df = e)

barres_defs_en_label <- fill_barres(c("group_considered"), list(), df = e, labels = labels_vars_en)

##### barres_defs_nolabel #####
barres_defs <- list(
  "vote_agg"             = list(vars = "vote_agg", width = 850, height = 500, miss = T), 
  "vote"                 = list(vars = "vote", width = 850, height = 500, miss = T),
  "climate_belief"       = list(vars = "climate_belief", width = 1300),
  "intl_governance"      = list(vars = variables_intl_governance, width = 900, height = 500),
  "group_defended"       = list(vars = variables_group_defended, width = 980),
  "top_tax_support"      = list(vars = variables_top_tax_support, width = 980),
  "wtp"                  = list(vars = variables_wtp, width = 900, sort = FALSE),
  "effect_program"       = list(vars = variables_effect_program, width = 980),
  "budget"               = list(vars = variables_budget, width = 1100, height = 1500, miss = T)
  # "custom_losers_agg"    = list(vars = "custom_losers_agg", width = 900),
  # "custom_winners_agg"   = list(vars = "custom_winners_agg", width = 900),
  # "custom_min_income_agg"= list(vars = "custom_min_income_agg", width = 900),
  # "inheritance_tax_agg"  = list(vars = variables_inheritance_tax_agg, width = 980),
  # "wealth_tax_support"   = list(vars = variables_wealth_tax_support, width = 980),
  # "intl_policy"          = list(vars = variables_intl_policy, width = 900),
  # "group_identified"     = list(vars = variables_group_identified, width = 900),
  # "assembly_outcome"     = list(vars = variables_assembly_outcome, width = 900),
  # "sustainable_future"   = list(vars = variables_sustainable_future, width = 980),
  # "gcs_support"          = list(vars = variables_gcs_support, width = 980),
  # "tax_policy"           = list(vars = variables_tax_policy, width = 980),
  # "inheritance_agg"      = list(vars = variables_inheritance_agg, width = 980),
  # "group_considered"     = list(vars = "group_considered", width = 900),
  # "gcs_comprehension"    = list(vars = "gcs_comprehension", width = 900),
  # "custom_redistr"       = list(vars = "custom_redistr", width = 900),
  # "custom_redistr_all"   = list(vars = variables_custom_redistr_all, width = 900),
  # "wtp_certainty"        = list(vars = "wtp_certainty", width = 900)
  # # "inheritance_type"     = list(vars = variables_inheritance_type, width = 980),
  # # "difficulty"           = list(vars = variables_difficulty, width = 900),
)
# barres_defs <- fill_barres(c(), barres_defs, df = e)
barres_defs <- fill_barres(c("custom_losers_agg", "custom_winners_agg", "custom_min_income_agg", 
                             "effect_program", "top_tax_support", "variables_wtp", "inheritance_tax_agg", "wealth_tax_support",
                             "intl_policy", "group_identified", "intl_governance", "assembly_outcome", "variables_sustainable_future", "variables_group_defended",
                             "variables_gcs_support", "tax_policy", "inheritance_agg", "group_considered", "gcs_comprehension", "custom_redistr", "custom_redistr_all", "wtp_certainty"), 
                           barres_defs, df = e)
barres_defs_en <- fill_barres(c("effect_program", "budget", "intl_policy", "group_identified", "intl_governance", "assembly_outcome"), 
                              list("intl_governance" = list(vars = variables_intl_governance, width = 900, height = 500),
                                   "effect_program" = list(vars = variables_effect_program, width = 860),
                                   "budget" = list(vars = variables_budget, width = 1100, height = 1500, miss = T)), df = e, labels = labels_vars_en)

barres_defs_en_label[["group_considered"]]$legend <- c("Own interests", "Community/country", "All humans")
barres_defs_en[["intl_governance"]]$legend <- c("Completely unfavorable", "Unfavorable", "Rather unfavorable", "Rather favorable", "Favorable", "Completely favorable")
barres_defs_en[["intl_policy"]]$legend <- c("Very bad", "Bad", "Rather bad", "Rather good", "Good", "Very good")
barres_defs_en[["group_identified"]]$legend <- c("Not at all", "Very little", "Little", "Rather", "A lot", "Completely")
barres_defs_en[["assembly_outcome"]]$legend <- c("Completely unfavorable", "Rather unfavorable", "Undecided", "Rather favorable", "Completely favorable")


##### Export PDFs to ../figures (not country_comparison) #####
barres_multiple(barres_defs) 
barres_multiple(barres_defs_label, nolabel = FALSE) 
barres_multiple(barres_defs_en, append_name = "_en") 
barres_multiple(barres_defs_en_label, nolabel = FALSE, append_name = "_en") 

barres_defs[["budget"]]$legend[5] <- "Ne sais pas"
# barres_defs[["budget"]]$width <- 1100
barres_defs[["budget"]]$labels <- break_strings(paste0(labels_vars[variables_budget], ": ", sub(".", ",", as.character(budget_policies_amounts), fixed = T), " Mds"), 64)
sum(grepl("<br>.*<br", barres_defs[["budget"]]$labels))
# barres_defs[["budget"]]$labels[1] <- paste(barres_defs[["budget"]]$labels[1], "Mds €")
# barres_defs[["budget"]]$labels[17] <- break_strings(labels_vars[variables_budget[17]], 90)
barres_multiple(barres_defs["budget"], weights = F)
barres_multiple(barres_defs["effect_program"])

# barres_defs_en[["effect_program"]] <- list(vars = variables_effect_program, width = 980, labels = labels_vars_en[variables_effect_program])
barres_defs_en[["effect_program"]]$legend <- c("Much less favorable", "Less favorable", "No change", "More favorable", "Much more favorable")
barres_defs_en[["effect_program"]]$labels[2] <- "Propose a UN international tax on millionaires, with 30%<br>funding health and education in low-income countries"
barres_multiple(barres_defs_en["effect_program"], append_name = "_en", weights = F)

# barres_defs_en[["budget"]] <- list(vars = variables_budget, width = 1100, height = 1500, miss = TRUE)
barres_defs_en[["budget"]]$legend <- c(rev(c("Desirable", "Acceptable", "Tolerable", "Unacceptable")), "Don't know")
barres_defs_en[["budget"]]$labels <- break_strings(paste0(labels_vars_en[variables_budget], ": ", as.character(budget_policies_amounts), " bn"), 64)
barres_multiple(barres_defs_en["budget"], append_name = "_en", weights = F)

# barres_multiple(barres_defs["wtp"], df = e[e$wtp_certainty >= 7,], append_name = "_certainty_7_10")
barres_multiple(barres_defs["wtp"], df = e[e$income_quartile > 2 & e$wtp_certainty >= 7,], append_name = "_income_top50")

# barres_multiple(barres_defs["wtp"]) 
# barres_multiple(barres_defs_label["custom_min_income_agg"], nolabel = FALSE) 

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

sum(bp$amount[bp$`conv+souh xPNR` > 0.5]) # 96.9
sum(bp$amount[bp$`souh xPNR` > 0.5]) # 31.5
sum(bp$amount[bp$`supp+conv+souh xPNR` > 0.5]) # 174.8
budget_majorite_convenable <- bp$variable[bp$`conv+souh xPNR` > 0.5]
budget_majorite_souhaitable <- bp$variable[bp$`souh xPNR` > 0.5]
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_convenable]) # 5 (37.5G€) hurt the rich, 3 shrink welfare state (26.7G€), 3 sectoral (24.7G€), 2 hurt foreigners (8G€)
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_souhaitable]) # 2 hurt the rich (14.5G€), 2 hurt foreigners (8G€), 1 sectoral (9G€)
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_convenable], weights = budget_policies$amount[budget_policies$variable_name %in% budget_majorite_convenable])
decrit(budget_policies$leaning[budget_policies$variable_name %in% budget_majorite_souhaitable], weights = budget_policies$amount[budget_policies$variable_name %in% budget_majorite_souhaitable])
