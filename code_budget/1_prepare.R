##### Load objects #####
budget_policies <- read.xlsx("../data_ext/budget_policies.xlsx", sheet = 1) # leaning: -1 hurt rich; 1 shrink welfare state; 2 hurt foreigners; 0 cost many/everyone; 0.5: sectoral policy
budget_policies_amounts <- setNames(budget_policies$amount, budget_policies$variable_name)
votes_xlsx <- read.xlsx("../data_ext/sources.xlsx", sheet = "Election", cols = 1:6)
votes <- list()
for (c in unique(votes_xlsx$country)) votes[[c]] <- votes_xlsx[votes_xlsx$country == c, ]  
for (c in unique(votes_xlsx$country)) row.names(votes[[c]]) <- votes[[c]]$party


##### Quotas #####
{
    levels_quotas <- list(
        "gender" = c("Femme", "Autre", "Homme"), # c("Woman", "Other", "Man"), # we could add: urbanity, education, wealth, occupation, employment_agg, marital_status, Nb_children, HH_size, home (ownership)
        "income_quartile" = c("Q1", "Q2", "Q3", "Q4"), # 1:4, 
        "age" = c("18-24", "25-34", "35-49", "50-64", "65+"),
        "urbanity" = c("Cities", "Towns and suburbs", "Rural"),
        "education_quota" = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64"), # "Not 25-64"
        "employment_18_64" = c("Inactive", "Unemployed", "Employed", "65+"),
        "vote" = c("Left", "Center-right or Right", 'Far right', "Non-voter, PNR or Other"),
        "region" = 1:5, # It's OK if some values are missing in one population. (2 regions: IT, PL; 3 regions: DE, CH; 4 regions: RU, SA, US)
        "urban" = c(TRUE, FALSE),
        "gcs_support" = c("Yes", "No"),
        "gcs_understood" = c(TRUE, FALSE)
    )
    
    # TODO? automatic _vote in quotas, nb_regions automatic
    quotas <- list("default" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "region")
                   # "FR" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "region"), #, "urban_category") From oecd_climate: Pb sur cette variable car il y a des codes postaux à cheval sur plusieurs types d'aires urbaines. Ça doit fausser le type d'aire urbaine sur un peu moins de 10% des répondants. Plus souvent que l'inverse, ça les alloue au rural alors qu'ils sont urbains.
                   # # Au final ça rajoute plus du bruit qu'autre chose, et ça gène pas tant que ça la représentativité de l'échantillon (surtout par rapport à d'autres variables type age ou diplôme). Mais ça justifie de pas repondérer par rapport à cette variable je pense. cf. FR_communes.R pour les détails.
    )
    for (q in names(quotas)) quotas[[paste0(q, "_vote")]] <- c(quotas[[q]], "vote")
    for (q in names(quotas)) quotas[[paste0(q, "_gcs")]] <- c(quotas[[q]], "gcs_support")
    # for (q in names(quotas)) quotas[[paste0(q, "_gcs_simple")]] <- c("gcs_support")
    for (q in names(quotas)) quotas[[paste0(q, "_vote_gcs")]] <- c(quotas[[q]], "vote", "gcs_support")
    for (q in names(quotas)) quotas[[paste0(q, "_all_gcs")]] <- c(quotas[[q]], "vote", "gcs_support", "gcs_understood")
    # for (q in names(quotas)) quotas[[paste0(q, "_vote_gcs_simple")]] <- c("gender", "income_quartile", "age", "education_quota", "vote", "gcs_support")
    # for (c in countries_EU) quotas[[paste0(c, "_all")]] <- c(quotas[[c]], "employment_18_64", "vote")
    
    qs <- round(read.xlsx("../data_ext/sources.xlsx", sheet = "Quotas", rowNames = T, rows = 1:2, cols = 1:57))
    
    pop_freq <- list()
    for (c in "FR") {
        pop_freq[[c]]$gender <- c("Femme" = qs[c,"women"], 0.001, "Homme" = qs[c,"men"])/1000
        pop_freq[[c]]$income_quartile <- rep(.25, 4)
        pop_freq[[c]]$age <- unlist(qs[c, c("18-24", "25-34", "35-49", "50-64", "65+")]/1000)
        pop_freq[[c]]$education_quota <- unlist(c(qs[c, c("Below.upper.secondary.25-64.0-2", "Upper.secondary.25-64.3", "Above.Upper.secondary.25-64.4-8")]/1000, "Not 25-64" = sum(unlist(qs[c, c("18-24", "65+")]/1000)))) # It's called 3 and 4-8 though in reality it's 3-4 and 5-8.
        pop_freq[[c]]$urbanity <- unlist(qs[c, c("Cities", "Towns.and.suburbs", "Rural")]/1000)
        pop_freq[[c]]$region <- unlist(qs[c, paste0("Region.", 1:5)]/1000)
        pop_freq[[c]]$employment_18_64 <- unlist(c(c("Inactive" = qs[c, "Inactivity"], "Unemployed" = qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000, "Employed" =  1000-qs[c, "Inactivity"]-qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000)*(1000-qs[c, c("65+")])/1000, "65+" = qs[c, c("65+")])/1000)
        # pop_freq[[c]]$vote <- unlist(c(c("Left" = qs[c, "Left"], "Center-right or Right" = qs[c, "Center-right.or.Right"], "Far right" = qs[c, "Far.right"])*(1000-qs[c, "Abstention"])/sum(qs[c, c("Left", "Center-right.or.Right", "Far.right")], na.rm = T), "Abstention" = qs[c, "Abstention"])/1000) # We exclude Other in this variant
        pop_freq[[c]]$vote <- unlist(c("Left" = qs[c, "Left"], "Center-right or Right" = qs[c, "Center-right.or.Right"], "Far right" = qs[c, "Far.right"], "Non-voter, PNR or Other" = sum(qs[c, "Abstention"], qs[c, "Vote_other"]))/1000) # We exclude Other in this variant
    }
}


##### Functions #####
weighting <- function(e, country = e$country[1], printWeights = T, variant = NULL, min_weight_for_missing_level = F, trim = T) {
    if (nrow(e) == 0) return(1)
    else {
        if (!missing(variant) & printWeights) print(variant)
        country_variant <- paste0(c(country, variant), collapse = "_") 
        if (!country_variant %in% names(quotas)) {
            warning("No country-variant quotas found, using default variables.")
            country_variant <- ifelse(is.null(variant), "default", paste0("default_", variant)) }
        vars <- quotas[[country_variant]]
        freqs <- list()
        for (v in vars) {
            if (!(v %in% names(e))) warning(paste(v, "not in data"))
            e[[v]] <- as.character(e[[v]], include.missings = T)
            e[[v]][is.na(e[[v]])] <- "NA"
            var <- ifelse(v %in% names(levels_quotas), v, paste(country, v, sep="_"))
            if (!(var %in% names(levels_quotas))) warning(paste(var, "not in levels_quotas"))
            levels_v <- as.character(levels_quotas[[var]])
            levels_v <- levels_v[levels_v != 0]
            missing_levels <- setdiff(levels(as.factor(e[[v]])), levels_v) 
            present_levels <- which(levels_v %in% levels(as.factor(e[[v]]))) 
            if (length(present_levels) != length(levels_v)) warning(paste0("Following levels are missing from data: ", var, ": ", 
                                                                           paste(levels_v[!1:length(levels_v) %in% present_levels], collapse = ', '), " (for ", country, "). Weights are still computed, neglecting this category."))
            prop_v <- pop_freq[[country]][[var]][present_levels]
            if (min_weight_for_missing_level) freq_missing <- rep(0.000001, length(missing_levels)) # imputes 0 weight for levels present in data but not in the weight's definition
            else freq_missing <- vapply(missing_levels, function(x) sum(e[[v]]==x), FUN.VALUE = c(0))
            freq_v <- c(prop_v*(nrow(e)-sum(freq_missing)), freq_missing)
            df <- data.frame(c(levels_v[present_levels], missing_levels), freq_v)
            # df <- data.frame(c(levels_v, missing_levels), nrow(e)*c(pop_freq[[country]][[var]], rep(0.0001, length(missing_levels))))
            names(df) <- c(v, "Freq")
            freqs <- c(freqs, list(df))
        }
        # print(freqs)
        unweigthed <- svydesign(ids=~1, data=e)
        raked <- rake(design= unweigthed, sample.margins = lapply(vars, function(x) return(as.formula(paste("~", x)))), population.margins = freqs)
        
        if (printWeights) {    print(summary(weights(raked))  )
            print(paste("(mean w)^2 / (n * mean w^2): ", representativity_index(weights(raked)), " (pb if < 0.5)")) # <0.5 : problématique
            print(paste("proportion not in [0.25; 4]: ", round(length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)), 3), "Nb obs. in sample: ", nrow(e)))
        }
        if (trim) return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE)))
        else return(weights(raked, lower=0.25, upper=4, strict=TRUE))
        
    }
}

stats_exclude <- function(data_name = "Budget", all = FALSE, old_names = FALSE) {
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

define_var_lists <- function() {
    var_suffix_effect_program <<- c("1" = "reduire_aide_developpement", "2" = "taxe_millionaires_onu", "3" = "fin_dutreil", "4" = "education_sante", "5" = "augmenter_allocs_familiales", "6" = "reduire_deficit", "7" = "reduire_depenses_fonctionnement", "8" = "restreindre_aides_etrangers", "9" = "appliquer_oqtf",
                                    "10" = "regulariser_sans_papiers", "11" = "peines_planchers_recidive", "12" = "retraite_65_ans", "13" = "retraite_62_ans", "14" = "augmenter_smic", "15" = "ric", "16" = "proportionnelle", "17" = "maintenir_green_deal")
    var_suffix_intl_policy <<- c("1" = "mondialisation", "2" = "frontieres_ouvertes", "3" = "redistribution_richesses", "4" = "citoyens_decisions", "5" = "intervention_pays_attaque", "6" = "demilitarisation", "7" = "impot_minimum_societes")
    var_suffix_group_identified <<- c("1" = "ville", "2" = "region", "3" = "france", "4" = "ue", "5" = "monde")
    var_suffix_intl_governance <<- c("1" = "elus_chefs_etat", "2" = "referendum_citoyens", "3" = "tirage_sort", "4" = "experts_scientifiques", "5" = "parlement_mondial", "6" = "sondages_consultatifs")
    var_suffix_assembly_outcome <<- c("1" = "consultatives_recommandations", "2" = "referendum_mondial", "3" = "referendum_pays_par_pays", "4" = "appliquees_institutions_inter")
    var_suffix_inheritance_type <<- c("1" = "designees_defunt", "2" = "epoux_descendants", "3" = "employes_societe", "4" = "etat_actionnaire", "5" = "fonds_citoyens", "6" = "onu_education_sante")
    var_suffix_home <<- c("1" = "tenant", "2" = "owner", "3" = "landlord", "4" = "hosted")
    var_suffix_difficulty <<- c("1" = "trop_difficile", "2" = "difficile", "3" = "comprehensible", "4" = "facile")
    var_suffix_budget <<- sub("budget_", "", setNames(budget_policies$variable_name, budget_policies$id))
    prefixes <<- c("effect_program", "intl_policy", "group_identified", "intl_governance", "assembly_outcome", "inheritance_type", "home", "difficulty", "budget")

    variables_duration <<- paste0("duration_", c("consent", "sociodemos", "program", "budget", "wtp", "gcs", "wealth_tax", "global_inheritance", "sustainable_future", "top_tax", "comprehension", "custom_redistr", "synthetic", "main_questions"))
    variables_home <<- paste0("home_", var_suffix_home) 
    variables_effect_program <<- paste0("effect_program_", var_suffix_effect_program) # grep("^effect_program_", nms, value = TRUE)
    variables_intl_policy <<- paste0("intl_policy_", var_suffix_intl_policy) # grep("^intl_policy_", nms, value = TRUE)
    variables_group_identified <<- paste0("group_identified_", var_suffix_group_identified) # grep("^group_identified_", nms, value = TRUE)
    variables_intl_governance <<- paste0("intl_governance_", var_suffix_intl_governance) # grep("^intl_governance_", nms, value = TRUE)
    variables_assembly_outcome <<- paste0("assembly_outcome_", var_suffix_assembly_outcome) # grep("^assembly_outcome_", nms, value = TRUE)
    variables_inheritance_type <<- paste0("inheritance_type_", var_suffix_inheritance_type) # grep("^inheritance_type_", nms, value = TRUE)
    variables_difficulty <<- paste0("difficulty_", var_suffix_difficulty) # grep("^difficulty_", nms, value = TRUE")
    variables_budget <<- budget_policies$variable_name # grep("^budget_", nms, value = TRUE)
    variables_budget_all <<- c(variables_budget, c("sum_souhaitable", "sum_convenable", "sum_supportable"))
    variables_sustainable_future <<- paste0("sustainable_future_", c("a", "n", "e")) # setdiff(grep("^sustainable_future", nms, value = TRUE), "variant_sustainable_future")
    variables_sustainable_future_all <<- paste0("sustainable_future", c("_a", "_n", "_e", ""))
    variables_group_defended <<- c("group_defended_base", "group_defended_world") # setdiff(grep("^group_defended", nms, value = TRUE), c("variant_group_defended", "group_defended_agg"))
    variables_group_defended_all <<- c("group_defended_base", "group_defended_world", "group_defended")
    variables_wealth_tax_support <<- paste0(c("solidary", "national", "intl"), "_tax_support") # intersect(c("solidary_tax_support", "national_tax_support", "intl_tax_support", "wealth_tax_support"), nms)
    variables_wealth_tax_support_all <<- paste0(c("solidary", "national", "intl", "wealth"), "_tax_support")
    variables_top_tax_support <<- paste0(c("top5", "top8"), "_tax_support") # intersect(c("top8_tax_support", "top5_tax_support", "top_tax_support"), nms)
    variables_top_tax_support_all <<- paste0(c("top5", "top8", "top"), "_tax_support")
    variables_gcs_support <<- paste0("gcs_support_", c("info", "no_info")) # setdiff(grep("^gcs_support", nms, value = TRUE), "variant_gcs")
    variables_gcs_support_all <<- paste0("gcs_support", c("_info", "_no_info", ""))
    variables_inheritance_tax <<- paste0("inheritance_tax_", c("400k", "1m", "10m", "1g", "100g"))
    variables_inheritance_agg <<- paste0("inheritance_agg_", var_suffix_inheritance_type)
    variables_inheritance_tax_agg <<- paste0("inheritance_tax_agg_", c("400k", "1m", "10m", "1g", "100g"))
    variables_tax_policy <<- c("tax_business_bequest", "inter_vivo_gifts", "net_wealth_tax", "tax_millionaires")
    variables_wtp <<- paste0("wtp_", c("0.5", 1, 2, 3, 5, 7, 10))
    variables_custom_redistr_all <<- c("custom_redistr", "custom_redistr_among_affected", "custom_redistr_among_non_affected")
    variables_numeric <<- c(variables_duration, variables_inheritance_type, paste0("custom_slider_", c("win", "lose")), "hh_size", "Nb_children__14", "income_exact", "trust", "wtp_certainty", "wtp_contribution") # , paste0("custom_", c("min_income", "winners", "losers"))
    variables_sociodemos_all <<- c("gender", "age_exact", "foreign", "foreign_born_family", "foreign_born", "foreign_origin", "couple", "hh_size", "Nb_children__14", "race", "income", "income_quartile", "income_exact", "education_original", "education", "education_quota", 
                                   "employment_status", "employment_agg", "working", "retired_or_not_working", "employment_18_64", "urbanity", "region", "owner", "millionaire", "nationality_SA", "voted", "vote")
    variables_quotas_base <<- c("man", "age_factor", "income_quartile", "education", "urbanity", "region") 
    variables_socio_demos <<- c(variables_quotas_base, "millionaire_agg", "couple", "employment_agg", "vote_factor") # add "hh_size", "owner", "wealth_factor", "donation_charities"?
    variables_sociodemos <<- c("man", "age_factor", "income_factor", "education_factor", "urbanity_factor", "region_factor", "millionaire_factor", "couple", "employment_agg", "vote_factor") # add "hh_size", "owner", "wealth_factor", "donation_charities"?
    control_variables <<- c("vote_factor", "man", "age_factor", "income_factor", "education_factor", "urbanity_factor", "millionaire_factor", "couple", "employment_agg", "foreign_born", "country_name", "country_region") # add "hh_size", "owner", "wealth_factor", "donation_charities"? "region_factor", "region_factor:country"
}

define_var_lists()

# Budget prepare: load Budget.csv, filter by scope, rename, convert.
prepare <- function(scope = "final", fetch = FALSE, convert = TRUE, rename = FALSE, duration_min = 360, weighting = T, remove_id = TRUE) {
    sample_name <- "Budget"
    if (fetch) {
        survey_list <- all_surveys()
        e <- fetch_survey(survey_list$id[survey_list$name == sample_name], include_display_order = T, verbose = T, convert = F, col_types = cols("m" = col_character()))
        if (!remove_id) e$ExternalReference <- e$m
        if (!remove_id) e$DistributionChannel <- e$IPAddress
        if (remove_id) e$interview <- grepl("@", e$interview)
        if (remove_id) e$budget_mail <- grepl("@", e$budget_mail)
        if (remove_id) e <- e[,which(!(names(e) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
        for (v in names(e)) label(e[[v]]) <- c(v = paste0(v, ": ", gsub("\n", "§", label(e[[v]]))))
        write_csv(e, paste0("../data_raw/", sample_name, ".csv"), na = "")
        saveRDS(label(e), paste0("../data_raw/labels_", sample_name, ".rds"))
        e <- read_csv(paste0("../data_raw/", sample_name, ".csv"), guess_max = Inf)
    }
    e <- read_csv(paste0("../data_raw/", sample_name, ".csv"), guess_max = Inf)
    labels <- readRDS(paste0("../data_raw/labels_", sample_name, ".rds"))
    for (v in names(e)) label(e[[v]]) <- labels[[v]]
    if ("Q_TerminateFlag" %in% names(e)) e$excluded <- e$Q_TerminateFlag else e$excluded <- NA
    e$finished <- e$Finished %in% c(TRUE, "TRUE", 1, "1")
    e$duration <- n(e$Q_TotalDuration) / 60
    label(e$duration) <- "duration: Duration (in min)"
    e$fast <- if ("Q_TotalDuration" %in% names(e)) n(e$Q_TotalDuration) < duration_min else FALSE
    label(e$fast) <- paste0("fast: T/F duration < ", duration_min/60, " min")
    e$valid <- !e$excluded %in% "QuotaMet"
    label(e$valid) <- "valid: T/F Allowed to continue after socio-demos: Not quota met (excluded != QuotaMet)"
    e$legit <- is.na(e$excluded)
    label(e$legit) <- "legit: T/F Not excluded (not quota met nor screened) (is.na(excluded))"
    e$dropout <- is.na(e$excluded) & !e$finished
    label(e$dropout) <- "dropout: T/F Chose to leave the survey (is.na(excluded) & !finished)"
    e$stayed <- e$finished & !e$excluded %in% "QuotaMet"# includes failed attention_test
    label(e$stayed) <- "stayed: T/F Did not drop out (excluded != QuotaMet & finished)" # Includes Screened (fast or quality) but excludes dropout
    e$final <- is.na(e$excluded) & e$finished
    label(e$final) <- "final: T/F In Final sample (!excluded & finished)"
    e$dropout_late <- e$dropout & n(e$Progress) >= 30 # TODO
    label(e$dropout_late) <- "dropout: Respondent who did not complete the survey though was not excluded, and who dropped out after the socio-demographic questions." 
    if (scope %in% names(e)) e <- e[e[[scope]] == TRUE, ]
    if (rename) e <- rename_survey(e)
    if (convert) e <- convert(e)
    e <- e[, !duplicated(names(e))]
    
    if (weighting) {
        e$weight <- weighting(e, "FR")
        label(e$weight) <- "weight: Weight for the international sample: weight_country is rescaled so that each country is weighted according to its adult population. Sums to nrow(all). (Created outside 'prepare')"
        if ("vote" %in% names(e)) e$weight_vote <- weighting(e, "FR", variant = "vote") # ("vote_us" %in% names(e) & (sum(e$vote_us=="PNR/no right")!=0)) | ("vote" %in% names(e))
        else e$weight_vote <- e$weight
        label(e$weight_vote) <- "weight_vote: [0.25; 4] Weight to adjust to country demographics. Sums to nrow([country]). Quota variables used: vote, quotas$[country], with frequencies pop_freq$[country]."
        # if (any(e$custom_redistr_asked) & !pilot) e <- compute_custom_redistr(e, name = "FR") # TODO 
    } else e$weight <- 1
    e$n <- if (scope == "all") paste0("FRa", 1:nrow(e)) else paste0("FRn", 1:nrow(e))
    
    e
}

convert <- function(e) {
    for (i in prefixes) e <- rename_vars_by_suffix(e, i)
    for (i in budget_policies$id) label(e[[variables_budget[i]]]) <- sub("policy", budget_policies$label[i], label(e[[variables_budget[i]]]))
    
    e$wtp_contribution <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", gsub(",", ".", as.character(e$wtp_contribution)))))
    
    inheritance_parents <- c(inheritance_400k = 400000, inheritance_1m = 1e6, inheritance_10m = 1e7, inheritance_1g = 1e9, inheritance_100g = 1e11)
    for (v in intersect(names(inheritance_parents), names(e))) {
        lab <- attr(e[[v]], "label")
        e[[paste0(v, "_num")]] <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", gsub(",", ".", e[[v]]))))
        is_million <- grepl("million", e[[v]], ignore.case = TRUE) & !grepl("milliard", e[[v]], ignore.case = TRUE)
        is_milliard <- grepl("milliard", e[[v]], ignore.case = TRUE)
        e[[paste0(v, "_num")]][is_million & !is.na(e[[paste0(v, "_num")]])] <- e[[paste0(v, "_num")]][is_million & !is.na(e[[paste0(v, "_num")]])] * 1e6
        e[[paste0(v, "_num")]][is_milliard & !is.na(e[[paste0(v, "_num")]])] <- e[[paste0(v, "_num")]][is_milliard & !is.na(e[[paste0(v, "_num")]])] * 1e9
        e[[sub("^inheritance_", "inheritance_tax_", v)]] <- ifelse(is.na(e[[v]]), NA, 1 - e[[paste0(v, "_num")]] / inheritance_parents[[v]])
        label(e[[v]]) <- label(e[[sub("^inheritance_", "inheritance_tax_", v)]]) <- lab
        labs <- setNames(e[[paste0(v, "_num")]], e[[v]])
        labs <- sort(labs[!duplicated(labs)])
        e <- create_item(v, labels = labs, values = names(labs), df = e)
        e <- create_item(sub("^inheritance_", "inheritance_tax_", v), new_var = sub("^inheritance_", "inheritance_tax_agg_", v), labels = c("0%" = 0, "5%" = .05, "10% - 12.5%" = .1, "25%" = .25, "50%" = .5, "75%" = .75, "90% - 95%" = .9, "99% - 100%" = 1), 
                         values = list(0, .05, c(.1, .125), .25, .5, .75, c(.9, .95), c(.995, .9995, .99995, .999995, 1)), df = e)
        # e <- create_item(sub("^inheritance_", "inheritance_tax_", v), new_var = sub("^inheritance_", "inheritance_tax_agg_", v), labels = c("0%" = 0, "5% - 12.5%" = .1, "25%" = .25, "50%" = .5, "75%" = .75, "90%" = .9, "95%" = .95, "99.x%" = .995, "100%" = 1), 
        #                  values = list(0, c(.05, .1, .125), .25, .5, .75, .9, .95, c(.995, .9995, .99995, .999995), 1), df = e)
    }
    
    for (i in intersect(variables_numeric, names(e))) {
        lab <- if (!is.null(attr(e[[i]], "label"))) attr(e[[i]], "label") else NULL
        e[[i]] <- as.numeric(as.vector(gsub("[^0-9.]", "", as.character(e[[i]])))) # trust/wtp_certainty may have text like "Pas du tout confiance0" -> extract digits)
        if (!is.null(lab)) label(e[[i]]) <- lab
    }
    for (v in intersect(variables_duration, names(e))) e[[v]] <- e[[v]] / 60

    for (j in intersect(variables_home, names(e))) {
        temp <- if (!is.null(attr(e[[j]], "label"))) attr(e[[j]], "label") else NULL
        e[[j]] <- !is.na(e[[j]]) & as.character(e[[j]]) != ""
        if (!is.null(temp)) label(e[[j]]) <- temp
    }
    
    if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% c("A little", "Un peu")
    
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
    e$education_quota <- ifelse(e$age > 25 & e$age < 65, e$education, 0)
    e <- create_item("education_quota", labels = c("Not 25-64" = 0, "Below upper secondary" = 1, "Upper secondary" = 2, "Post secondary" = 3), values = 0:3, missing.values = c(NA, 0), df = e, annotation = "education_quota: ifelse(age > 25 & age < 65, education, 0).")
    if ("income" %in% names(e)) e <- create_item("income", new_var = "income_quartile", labels = c("PNR" = 0, "Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4), values = c("not", "100|200|250", "300|400|500", "600|700|750", "800|900"), grep = TRUE, missing.values = c("PNR"), df = e)
    e <- create_item("income", new_var = "income_decile", labels = c("d1" = 1, "d2" = 2, "d3" = 3, "d4" = 4, "d5" = 5, "d6" = 6, "d7" = 7, "d8" = 8, "d9" = 9, "d10" = 10, "PNR" = 0), values = c("less", "100 and", "201|300", "301", "401", "501", "601", "701|800", "801", "more", "not"), grep = T, missing.values = c("PNR"), df = e)  
    e$uc <- .5 + .5*pmax(0, e$hh_size - e$Nb_children__14) + .3*e$Nb_children__14
    
    e <- create_item("employment_status", new_var = "employment_agg", labels = c("Not working", "Student", "Working", "Retired"), grep = T, values = c("Inacti|Unemployed|chômage", "Student|Étudiant", "employed$|Employé|Indépend", "Retired|Retrait"), df = e, 
                     annotation = "employment_agg: Not working (Inactive or Unemployed) / Student / Retired / Employed (full-time, part-time, or self-employed). Built from employment_status.")
    if ("urbanity" %in% names(e)) {
        e$urban <- e$urbanity == 1
        e <- create_item("urbanity", labels = c("Cities" = 1, "Towns and suburbs" = 2, "Rural" = 3), grep = TRUE, values = c("1", "2", "3|4"), keep_original = TRUE, missing.values = 0, df = e)
    }
    e <- create_item("foreign", new_var = "foreign_born_family", labels = c("No" = 0, "One parent" = 1, "Two parents" = 2, "Self" = 3), grep = TRUE, values = c("too|Non", "one of|un de", "both|deux", "Yes|Oui"), df = e) 
    e$foreign_born <- e$foreign_born_family %in% 3
    label(e$foreign_born) <- "foreign_born: T/F. Born abroad (foreign == 3)."
    e$foreign_origin <- e$foreign_born_family > 0 
    label(e$foreign_origin) <- "foreign_origin: T/F. At least one parent (or self) born abroad (foreign > 0)."
    
    e <- create_item("millionaire", new_var = "millionaire_agg", labels = c("Unlikely" = -1, "Likely" = 0, "Already" = 1), grep = TRUE, values = c("nlikely|eu", "Very l|Likely|Prob|Très prob", "already|déjà"), df = e)
    e <- create_item("millionaire", labels = c("Très peu probable" = -3, "Peu probable" = -1, "Probable" = 1, "Très probable" = 3, "Je suis déjà millionnaire" = 5), df = e)
    e$millionaire_factor <- factor(e$millionaire_agg)
    e <- create_item("wealth", new_var = "wealth_quantile", labels = c("< d3" = 15, "d3-d5" = 40, "d5-d6" = 55, "d6-d7" = 65, "d7-d8" = 75, "d8-d9" = 85, "p91-p95" = 93, "> p95" = 97, "NSP" = -.1), grep = T, df = e, missing.values = -.1,
                     values = c("Moins", "Entre 25", "Entre 100", "Entre 200", "Entre 300", "Entre 400", "Entre 700", "Plus", "Ne")) # https://www.insee.fr/fr/statistiques/7941421?sommaire=7941491
    e <- create_item("wealth", new_var = "wealth_quartile_5", labels = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "D10" = 5, "NSP" = -.1), grep = T, df = e, missing.values = -.1,
                     values = c("Moins", "Entre 25", "Entre 100|Entre 200|Entre 300", "Entre 400|Entre 700", "Plus", "Ne"))
    e <- create_item("wealth", new_var = "wealth_quartile", labels = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "NSP" = -.1), grep = T, df = e, missing.values = -.1,
                     values = c("Moins", "Entre 25", "Entre 100|Entre 200|Entre 300", "Entre 400|Entre 700|Plus", "Ne"))
    e <- create_item("wealth", labels = c("< 25k" = 10, "25-100k" = 50, "100-200k" = 150, "200-300k" = 250, "300-400k" = 350, "400-700k" = 500, "700k - 1M" = 800, "> 1M" = 1100, "NSP" = -.1), grep = T, df = e, missing.values = -.1,
                         values = c("Moins", "Entre 25", "Entre 100", "Entre 200", "Entre 300", "Entre 400", "Entre 700", "Plus", "Ne"))
    
    if ("vote_FR" %in% names(e)) {
        names(e)[names(e) == "vote_FR"] <- "vote_original"
        label(e$vote_original) <- "vote_original: Vote (if voted) or closest candidate (if !voted) in last election."
        e <- create_item("voted", new_var = "voted_original", c("No right" = -1, "PNR" = -0.1, "No" = 0, "Yes" = 1), grep = T, values = c("right|droit", "Prefer not|réfère", "No", "Yes|Oui"), df = e)
        e$voted <- e$voted %in% c("Yes", "Oui")
        label(e$voted) <- "voted: T/F Voted in last election."
        label(e$vote_original) <- "vote_original: Vote (if voted) or closest candidate (if !voted) in last election."
        e$vote_agg <- ifelse(e$vote_original %in% c("Prefer not to say", "Other", NA), -1, votes[["FR"]][e$vote_original, "leaning"]) # PNR, Other as -1
        e$vote_leaning <- ifelse(e$vote_original == "Other", NA, e$vote_agg) # PNR as -1, Other as NA
        label(e$vote_leaning) <- "vote_leaning: ifelse(vote_original == Other, NA, vote_agg)"
        e$vote_major_candidate <- votes[["FR"]][sub("Prefer not to say", "Other", e$vote_original), "major"] %in% 1
        label(e$vote_major_candidate) <- "vote_major_candidate: Vote (or hypothetical vote) for a major candidate (> 1% (?) of actual votes)."
        e$vote_major <- ifelse(e$vote_major_candidate, e$vote_original, "PNR or Other")
        label(e$vote_major) <- "vote_major: Vote (or hypothetical vote) if vote_major_candidate, else 'PNR or Other'."
        e$vote_voters <- ifelse(e$voted, e$vote_original, "Non-voter or PNR")
        label(e$vote_voters) <- "vote_voters: Vote if voted else 'Non-voter or PNR'."
        e$vote_major_voters <- ifelse(e$voted, ifelse(e$vote_major %in% "PNR or Other", "Non-voter, PNR or Other", e$vote_major), "Non-voter, PNR or Other")
        label(e$vote_major_voters) <- "vote_major_voters: Vote if voted for a major candidate, else 'Non-voter, PNR or Other'."
        e$vote <- ifelse(e$voted, e$vote_agg, -1) # Only on voters
        
        e <- create_item("vote_agg", labels = c("PNR or Other"  = -1, "Left" = 0, "Center-right or Right" = 1, "Far right" = 2), values = -1:2, missing.values = -1, df = e, annotation = "vote_agg: -1-2. Vote (or hypothetical vote) [-1: PNR or Other; 0: Left; 1: Center-right or Right; 2: Far right].")
        e <- create_item("vote", labels = c("Non-voter, PNR or Other"  = -1, "Left" = 0, "Center-right or Right" = 1, "Far right" = 2), values = -1:2, missing.values = -1, df = e, annotation = "vote: -1-2. Vote [-1: Non-voter, PNR or Other; 0: Left; 1: Center-right or Right; 2: Far right].")
        e$vote_agg_factor <- relevel(as.factor(as.character(e$vote_agg, include.missings = T)), "PNR or Other")
        label(e$vote_agg_factor) <- "vote_agg_factor: -1-2. Vote (or hypothetical vote) [-1: PNR or Other; 0: Left; 1: Center-right or Right; 2: Far right]."
        e$vote_factor <- relevel(as.factor(as.character(e$vote, include.missings = T)), "Non-voter, PNR or Other") # Left
        label(e$vote_factor) <- "vote_factor: -1-2. Vote [-1: Non-voter, PNR or Other; 0: Left; 1: Center-right or Right; 2: Far right]."
    }
    
    # Likert / create_item for variables present in Budget (already in former convert)
    e <- create_item("gcs_comprehension", labels = c("decrease" = -1, "not be affected" = 0, "increase" = 1), values = c("dimin", "pas aff", "augment"), grep = T, df = e)
    e$gcs_understood <- e$gcs_comprehension == 1 
    e <- create_item("group_defended", new_var = "group_defended_base", labels = c("Family and self" = -2, "Community (region, gender...)" = -1, "Fellow citizens" = 0, "Humans" = 1, "Sentient beings" = 2), grep = TRUE, values = c("famil|self", "religion|Community|commun", "Americans|Fellow citizens|citoyens|Citizens|Françai", "Humans|humain", "Sentient|sentient"), df = e)
    e <- create_item("group_defended_world", labels = c("Family and self" = -2, "Community (region, gender...)" = -1, "Fellow citizens" = 0, "Humans" = 1, "Sentient beings" = 2), values = c("Ma famille et moi", "communauté", "Français", "Les humains", "sentients"), grep = TRUE, df = e) # c("Ma famille et moi" = -2, "ma communauté" = -1, "Les Français" = 0, "Les humains" = 1, "êtres sentients" = 2)
    # Tax support: Budget uses Oui/Non (not 5-level Likert)
    e <- create_item("solidary_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
    e <- create_item("national_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
    e <- create_item("intl_tax_support", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
    e <- create_item("couple", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)

    e$variant_sustainable_future <- e$sustainable_future <- NA
    e$variant_sustainable_future[!is.na(e$sustainable_future_a)] <- "full"
    e$variant_sustainable_future[!is.na(e$sustainable_future_n)] <- "symbol"
    e$variant_sustainable_future[!is.na(e$sustainable_future_e)] <- "none"
    e$sustainable_future[!is.na(e$sustainable_future_a)] <- grepl("A", e$sustainable_future_a[!is.na(e$sustainable_future_a)])
    e$sustainable_future[!is.na(e$sustainable_future_n)] <- grepl("A", e$sustainable_future_n[!is.na(e$sustainable_future_n)])
    e$sustainable_future[!is.na(e$sustainable_future_e)] <- grepl("A", e$sustainable_future_e[!is.na(e$sustainable_future_e)])

    e <- create_item("top8_tax_support", labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), grep = T, values = c("Très op", "Plutôt op", "Indiff", "Plutôt fav", "Très fav"), df = e) # c("Très opposé⋅e" = -2, "Plutôt opposé⋅e" = -1, "Indifférent⋅e" = 0, "Plutôt favorable" = 1, "Très favorable" = 2
    e <- create_item("top5_tax_support", labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), grep = T, values = c("Très op", "Plutôt op", "Indiff", "Plutôt fav", "Très fav"), df = e)
    e$variant_top_tax <- "top5"
    e$variant_top_tax[!is.na(e$top5_tax_support)] <- "top8"
    e$top_tax_support <- e$top5_tax_support # ifelse(e$variant_top_tax == "top5", e$top5_tax_support, e$top8_tax_support)
    e$top_tax_support[e$variant_top_tax == "top8"] <- e$top8_tax_support[e$variant_top_tax == "top8"]
    
    e$variant_wealth_tax[!is.na(e$solidary_tax_support)] <- "solidary"
    e$variant_wealth_tax[!is.na(e$national_tax_support)] <- "national"
    e$variant_wealth_tax[!is.na(e$intl_tax_support)] <- "intl"
    e$wealth_tax_support <- e$solidary_tax_support
    e$wealth_tax_support[!is.na(e$national_tax_support)] <- e$national_tax_support[!is.na(e$national_tax_support)]
    e$wealth_tax_support[!is.na(e$intl_tax_support)] <- e$intl_tax_support[!is.na(e$intl_tax_support)]

    e$owner <- e$home_owner == T | e$home_landlord == T
    label(e$owner) <- "owner: Owner or Landlord renting out property to: Are you a homeowner or a tenant?"
    
    e <- create_item(variables_effect_program, labels = c("Beaucoup moins favorable" = -2, "Moins favorable" = -1, "Ne changerait rien" = 0, "Plus favorable" = 1, "Beaucoup plus favorable" = 2),
                         values = c("Beaucoup moins", "Moins favorable", "Ne changerait rien", "Plus favorable", "Beaucoup plus"), grep = TRUE, df = e)
    
    e <- create_item(variables_budget, labels = c("Inacceptable" = -1, "Ne sais pas" = -0.1, "Supportable" = 0, "Convenable" = 1, "Souhaitable" = 2),
                         values = c("Inacceptable", "Ne sais pas", "Supportable", "Convenable", "Souhaitable"), grep = TRUE, missing.values = -0.1, df = e)
    
    
    # for (i in seq_len(nrow(budget_policies))) {
    #     id <- budget_policies$id[i]
    #     new_name <- budget_policies$variable_name[i]
    #     old_name <- paste0("budget_progressistes_", id)
    #     if (old_name %in% names(e) && !new_name %in% names(e)) {
    #         names(e)[names(e) == old_name] <- new_name
    #     }
    # }
    # Sums of amounts (G€) by acceptability: Souhaitable=2, Convenable=1, Supportable=0
    e$sum_souhaitable <- rowSums(sapply(variables_budget, function(v) ifelse(n(e[[v]]) == 2, budget_policies_amounts[v], 0)), na.rm = TRUE) 
    e$sum_convenable <- rowSums(sapply(variables_budget, function(v) ifelse(n(e[[v]]) >= 1, budget_policies_amounts[v], 0)), na.rm = TRUE)
    e$sum_supportable <- rowSums(sapply(variables_budget, function(v) ifelse(n(e[[v]]) >= 0, budget_policies_amounts[v], 0)), na.rm = TRUE)
    label(e$sum_souhaitable) <- "sum_souhaitable: Sum of budget policy amounts rated Souhaitable only (G€)."
    label(e$sum_convenable) <- "sum_convenable: Sum of budget policy amounts rated Souhaitable or Convenable (G€)."
    label(e$sum_supportable) <- "sum_supportable: Sum of budget policy amounts rated Souhaitable, Convenable or Supportable (G€)."

    names(e)[names(e) == "wtp_contribution"] <- "variant_wtp"
    lab <- Label(e$wtp)
    e$wtp <- grepl("POUR", e$wtp)
    label(e$wtp) <- lab
    for (i in unique(e$variant_wtp)) e[[paste0("wtp_", i)]][e$variant_wtp == i] <- e$wtp[e$variant_wtp == i]

    e <- create_item("climate_belief", labels = c("CC pas réel" = -3, "Principalement naturel" = -2, "Autant humain que naturel" = -1, "Principalement humain" = 0, "Entièrement humain" = 1),
                                       values = c("n'est pas une r", "principalement dû.*variabilité", "autant dû", "principalement dû.*activité", "entièrement dû"), grep = TRUE, df = e)
    e <- create_item("gcs_support_no_info", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
    e <- create_item("gcs_support_info", labels = c("No" = 0, "Yes" = 1), values = c("Non", "Oui"), df = e)
    e$variant_gcs[!is.na(e$gcs_support_no_info)] <- "No info"
    e$variant_gcs[!is.na(e$gcs_support_info)] <- "Info"
    e$gcs_support <- e$gcs_support_no_info
    e$gcs_support[!is.na(e$gcs_support_info)] <- e$gcs_support_info[!is.na(e$gcs_support_info)]
    
    e <- create_item(variables_intl_policy, labels = c("Très mauvais" = -5, "Mauvais" = -3, "Plutôt mauvais" = -1, "Plutôt bien" = 1, "Bien" = 3, "Très bien" = 5),
                         values = c("Très mauvais", "Mauvais", "Plutôt mauvais", "Plutôt bien", "Bien", "Très bien"), grep = TRUE, df = e)
    
    e <- create_item("group_considered",labels = c("Propres intérêts" = -1, "Communauté/pays" = 0, "Tous les humains" = 1),
                                               values = c("Leurs propres", "communauté|pays", "tous les humains"), grep = TRUE, df = e)

    e <- create_item(variables_group_identified, labels = c("Pas du tout" = -5, "Très peu" = -3, "Peu" = -1, "Plutôt" = 1, "Beaucoup" = 3, "Tout à fait" = 5),
                         values = c("Pas du tout", "Très peu", "Peu", "Plutôt", "Beaucoup", "Tout à fait"), grep = TRUE, df = e)

    for (v in var_suffix_inheritance_type) {
        e[[paste0("inheritance_agg_", v)]] <- e[[paste0("inheritance_type_", v)]]
        e[[paste0("inheritance_agg_", v)]][e[[paste0("inheritance_type_", v)]] > 50 & e[[paste0("inheritance_type_", v)]] < 100] <- 75
        e[[paste0("inheritance_agg_", v)]][e[[paste0("inheritance_type_", v)]] >= 30 & e[[paste0("inheritance_type_", v)]] < 50] <- 37.5
        e[[paste0("inheritance_agg_", v)]][e[[paste0("inheritance_type_", v)]] >= 20 & e[[paste0("inheritance_type_", v)]] < 30] <- 22.5
        e[[paste0("inheritance_agg_", v)]][e[[paste0("inheritance_type_", v)]] >= 10 & e[[paste0("inheritance_type_", v)]] < 20] <- 12.5
        e <- create_item(paste0("inheritance_agg_", v), labels = c("0%" = 0, "5%" = 5, "10-15%" = 12.5, "20-25%" = 22.5, "30-45%" = 37.5, "50%" = 50, "55-95%" = 75, "100%" = 100), 
                         values = c(0, 5, 12.5, 22.5, 37.5, 50, 75, 100), missing.values = NA, df = e)
    }
        
    # e$variant_group_defended <- NA
    e$variant_group_defended <- "base"
    e$variant_group_defended[!is.na(e$group_defended_world)] <- "world"
    e$group_defended <- e$group_defended_base
    e$group_defended[!is.na(e$group_defended_world)] <- e$group_defended_world[!is.na(e$group_defended_world)]
    
    e <- create_item(variables_intl_governance, labels = c("Complètement défavorable" = -5, "Défavorable" = -3, "Plutôt défavorable" = -1, "Plutôt favorable" = 1, "Favorable" = 3, "Complètement favorable" = 5),
                     values = c("Complètement défavorable", "Défavorable", "Plutôt défavorable", "Plutôt favorable", "Favorable", "Complètement favorable"), grep = TRUE, df = e)

    e <- create_item(variables_assembly_outcome, labels = c("Complètement défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Complètement favorable" = 2),
                     values = c("Complètement défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Complètement favorable"), grep = TRUE, df = e)
    e <- create_item("custom_redistr", labels = c("Très défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Très favorable" = 2),
                values = c("Très défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Très favorable"), grep = TRUE, df = e)

    variables_tax_policy <- intersect(c("tax_business_bequest", "inter_vivo_gifts", "net_wealth_tax", "tax_millionaires"), names(e))
    e <- create_item(variables_tax_policy, labels = c("Très défavorable" = -2, "Défavorable" = -1, "Indécis⋅e" = 0, "Favorable" = 1, "Très favorable" = 2),
                         values = c("Très défavorable", "Défavorable", "Indécis⋅e", "Favorable", "Très favorable"), grep = TRUE, df = e)

    for (v in c("custom_losers", "custom_winners", "custom_min_income")) {
        # lab <- attr(e[[v]], "label")
        # txt <- paste0(v, "_1_TEXT")
        # raw <- if (txt %in% names(e)) as.character(e[[txt]]) else as.character(e[[v]])
        e[[v]][grepl("NSP", e[[v]], ignore.case = TRUE)] <- -.1 # "NSP"
        e[[v]][!is.na(e[[v]]) & e[[v]] != -.1] <- as.numeric(gsub("[^0-9.]", "", gsub(",", ".", e[[paste0(v, "_1_TEXT")]])))[!is.na(e[[v]]) & e[[v]] != -.1]
        e[[v]][is.na(e[[v]]) & is.na(e$custom_redistr)] <- -.1
        e[[v]] <- as.item(n(e[[v]]), missing.values = -.1)
        # label(e[[v]]) <- label(e[[paste0(v, "_agg")]]) <- lab
    }
    e <- create_item("custom_losers", new_var = "custom_losers_agg", labels = c("0% - 4%" = 2, "5%" = 5, "6% - 9%" = 7.5, "10%" = 10, "11% - 19%" = 15, "20% - 30%" = 25, "> 30%" = 50),
                     values = list("0% - 4%" = c(0.5, 0:4), "5%" = 5, "6% - 9%" = 6:9, "10%" = 10, "11% - 19%" = 11:19, "20% - 30%" = 20:30, "> 30%" = 31:10440), df = e)
    e <- create_item("custom_winners", new_var = "custom_winners_agg", labels = c("< 20%" = 2, "20% - 40%" = 5, "41% - 49%" = 7.5, "50%" = 10, "51% - 59%" = 15, "60% - 75%" = 25, "> 75%" = 50),
                     values = list("< 20%" = c(0:19), "20% - 40%" = 20:40, "41% - 49%" = 41:49, "50%" = 50, "51% - 59%" = 51:59, "60% - 75%" = 60:75, "> 75%" = 76:1900), df = e)
    # e$custom_min_income[!is.na(e$custom_min_income) & e$custom_min_income != -.1] <- round(e$custom_min_income[!is.na(e$custom_min_income) & e$custom_min_income != -.1])
    e <- create_item("custom_min_income", new_var = "custom_min_income_agg", labels = c("< 60" = 30, "60 - 149" = 100, "150 - 249" = 200, "250 - 499" = 300, "500" = 500, "501 - 749" = 600, "750 - 999" = 800, "> 1000" = 1300),
                     values = list("< 60" = 0:59, "60 - 149" = 60:149, "150 - 249" = 150:249, "250 - 499" = 250:499, "500" = 500, "501 - 749" = 501:749, "750 - 999" = 750:999, "1000" = 1000, "> 1000" = 1001:1e8), df = e)
    
        # for (v in c("custom_slider_win", "custom_slider_lose")) {
    #     lab <- attr(e[[v]], "label")
    #     e[[v]] <- as.numeric(as.character(e[[v]]))
    #     if (!is.null(lab)) label(e[[v]]) <- lab
    # }
    # for (v in grep("^difficulty_[0-9]+$", names(e), value = TRUE)) {
    #     lab <- attr(e[[v]], "label")
    #     e[[v]] <- !is.na(e[[v]]) & as.character(e[[v]]) != ""
    #     if (!is.null(lab)) label(e[[v]]) <- lab
    # }
    
    e$income_exact_individualized <- e$income_exact / e$uc
    label(e$income_exact_individualized) <- "income_exact_individualized: Individualized income (income_exact/uc) (€/month)."
    e$income_exact_equal_split <- e$income_exact / (1 + (e$couple > 0))
    label(e$income_exact_equal_split) <- "income_exact_equal_split: Equal-split income (income_exact/(1+couple)) (€ per month)."
    e$income_exact_affected_top_tax <- e$income_exact_affected_top5_tax <- e$income_exact_affected_top8_tax <- e$income_exact_equal_split > ifelse(e$variant_top_tax == "top5", 4000, 3000)
    label(e$income_exact_affected_top_tax) <- "income_exact_affected_top_tax: T/F Respondent's household is affected by the global income top_tax (i.e. their income_exact_equal_split > €4k or €3k depending on variant_top_tax)."
    e$income_exact_affected_top5_tax[e$variant_top_tax == "top8"] <- NA
    label(e$income_exact_affected_top5_tax) <- "income_exact_affected_top5_tax: T/F Respondent's household is affected by the global income top1_tax (i.e. their income_exact_equal_split > €4k or €3k depending on variant_top_tax)."
    e$income_exact_affected_top8_tax[e$variant_top_tax == "top5"] <- NA
    label(e$income_exact_affected_top8_tax) <- "income_exact_affected_top8_tax: T/F Respondent's household is affected by the global income top3_tax (i.e. their income_exact_equal_split > €4k or €3k depending on variant_top_tax)."
    e$top5_tax_support_affected[!e$income_exact_affected_top5_tax %in% T] <- NA
    e$top8_tax_support_affected[!e$income_exact_affected_top8_tax %in% T] <- NA
    e$top_tax_support_affected[!e$income_exact_affected_top_tax %in% T] <- NA
    label(e$top5_tax_support_affected) <- "top5_tax_support_affected: -2-2. [Among responents affected by the tax; other: NA] Supports an additional income tax on the top 5% to finance poverty reduction for the bottom 3.5 billion people (10% > 4k; 25% > 5k; 40% > 6k; 65% > 50k/month)."
    label(e$top8_tax_support_affected) <- "top8_tax_support_affected: -2-2. [Among responents affected by the tax; other: NA] Supports an additional income tax on the top 8% to finance poverty reduction for the bottom 4 billion people (10% > 3k; 25% > 5k; 40% > 7.5k; 90% > 50k/month)."
    label(e$top_tax_support_affected) <- "top_tax_support_affected: -2-2. [Among responents affected by the tax; other: NA] Supports an additional income tax on the top 5 or 8% (depending on variant_top_tax) to finance poverty reduction for the bottom 3.5 or 4 billion people."
    e$custom_redistr_affected <- e$income_exact_equal_split > 2100
    label(e$custom_redistr_affected) <- "custom_redistr_affected: T/F Respondent's household is affected by the custom_redistr (i.e. their income_exact_equal_split > €2.1k)."
    e$custom_redistr_among_affected <- e$custom_redistr_among_non_affected <- e$custom_redistr
    e$custom_redistr_among_affected[!e$custom_redistr_affected] <- NA
    e$custom_redistr_among_non_affected[e$custom_redistr_affected] <- NA
    # e$income_exact_equal_split_dollar <- ifelse(e$variant_top_tax == "top3", 8e4, 12e4) * e$income_exact_equal_split / as.numeric(gsub("[^0-9]*", "", features[ifelse(e$variant_top_tax == "top3", "lcu_80k", "lcu_120k"), languages_country[[country]][1]]))
    # label(e$income_exact_equal_split_dollar) <- "income_exact_equal_split_dollar: T/F Equal-split income (income_exact/(1+couple)) ($/year)."
    
    # e$income_exact_decile <- 1+rowSums(e$income_exact_individualized > matrix(rep(t(income_deciles[c(1,2,4:8,10,11), languages_country[[country]][1]]), nrow(e)), nrow = nrow(e), byrow = T))
    # e$income_exact_quartile <- 1+rowSums(e$income_exact_individualized > matrix(rep(t(income_deciles[c(3,6,9), languages_country[[country]][1]]), nrow(e)), nrow = nrow(e), byrow = T))
    # e$income_answers_spread <- e$income_decile - e$income_exact_decile # Some (positive) spread is expected because income is before taxes (even gross in DE, GB, JP, SA, US) but after taxes in income_exact
    # label(e$income_answers_spread) <- "income_answers_spread: income_decile - income_exact_decile"
    # e$income_answers_decile_coincide <- e$income_answers_spread == 0
    # label(e$income_answers_decile_coincide) <- "income_answers_decile_coincide: income_answers_spread == 0"
    # e$income_answers_quartile_coincide <- e$income_exact_quartile == e$income_quartile
    # label(e$income_answers_quartile_coincide) <- "income_answers_quartile_coincide: income_exact_quartile == income_quartile"
    
    e <- e[, vapply(e, function(col) !all(is.na(col)), logical(1L)), drop = FALSE] # Drop variables that are entirely NA
    return(e)
}

e <- prepare(fetch = T, weighting = T, remove_id = T)
# a <- prepare(scope = "all", fetch = F, weighting = F, convert = T, remove_id = T)
# for (i in c(17:37, 100,104, 110:116, 128:129, 140,141, 149:161, 165,166,168,170,177:179, 186:189, 191, 219, 252:289)) { print(names(e)[i]); print(decrit(e[[i]])) }

export_quotas() # https://docs.google.com/spreadsheets/d/1EkkyVWX3LgLjyw-lm7c6r5iBXe1VZy2EMI3oR7ALgXI/edit?gid=265678696#gid=265678696
stats_exclude()


##### Codebook #####
export_codebook(e, "../data_ext/codebook.csv", stata = FALSE) #, omit = c(1, 2, 7, 9:13, 197))
# export_codebook(all, "../questionnaire/codebook.csv", stata = FALSE, omit = c(2, 7, 9:13, 197))


##### Save #####
save.image(".RData")


