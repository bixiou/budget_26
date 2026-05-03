### Analyses complètes — paquets majoritaires et préférences budgétaires
### (1)/(1bis)/(1ter) Paquet majoritaire avec la plus grande économie
### (2) Proportion max P soutenant conjointement un paquet ≥ 90 Mds€
### (3)/(3bis) Paquets majoritaires par groupe de votants (SCS)
### (3ter) Paquets majoritaires par coalition (SCS)
### (3quater) Paquets majoritaires en CS (blocs, paires, coalitions)
### (4) Paquet maximisant la somme des utilités sous contrainte ≥ 90 Mds€
### (5) Positionnement idéologique × soutien (conv+souh)
### (6) Notes moyennes par mesure et groupe de votants (effect_program et budget)
### (7) Matrice de distances entre groupes de votants (blocs + coalitions)

source('.Rprofile')
load('.RData')
e$no_weight <- 1

library(dplyr)
library(ggplot2)
library(ggrepel)

start <- Sys.time()
## ── Constantes ──────────────────────────────────────────────────────────────
THRESHOLD     <- 0.5   # seuil majorité
THRESHOLD_LOW <- 0.336  # seuil bas pour partie (2) (0.249 si NSP exclus)
AMOUNT_TARGET <- 90    # Mds€

## ── Variables et montants ───────────────────────────────────────────────────
vars    <- variables_budget
short   <- sub("budget_", "", vars)
m       <- length(vars)
amounts <- budget_policies$amount[match(vars, budget_policies$variable_name)]
pkg_amount <- function(cols) sum(amounts[cols], na.rm = TRUE)

## ── Labels français pour les variables budget et effect_program ────────────
labels_budget_fr <- c(
  aligner_tva_restauration                      = "Aligner TVA restauration",
  augmenter_age_retraite_65                     = "Augmenter âge retraite à 65 ans",
  augmenter_cotisations_salaires_moyens         = "Augmenter cotisations sur salaires moyens",
  augmenter_csg_1pt                             = "Augmenter CSG (+1 pt)",
  augmenter_duree_travail_droit_chomage         = "Augmenter durée travail pour droit chômage",
  augmenter_impot_heritages_eleves              = "Augmenter impôt héritages élevés",
  augmenter_impot_revenu_aises                  = "Augmenter impôt revenu aisés",
  augmenter_impot_revenu_tous                   = "Augmenter impôt revenu (tous)",
  augmenter_impot_societes                      = "Augmenter impôt sociétés",
  augmenter_taxe_revenus_capital                = "Augmenter taxe revenus du capital",
  augmenter_tva_1pt                             = "Augmenter TVA (+1 pt)",
  diminuer_credit_impot_recherche               = "Diminuer Crédit Impôt Recherche",
  diminuer_subventions_ecole_privee             = "Diminuer subventions école privée",
  eliminer_doublons_territoriaux                = "Éliminer doublons territoriaux",
  geler_aides_sociales                          = "Geler aides sociales",
  geler_depenses_etat_collectivites             = "Geler dépenses État/collectivités",
  reduire_aides_apprentissage                   = "Réduire aides apprentissage",
  reduire_depenses_educatives_demographie       = "Réduire dépenses éducatives (démographie)",
  reduire_depenses_militaires                   = "Réduire dépenses militaires",
  reduire_pensions_retraite                     = "Réduire pensions de retraite",
  reduire_remboursement_soins                   = "Réduire remboursement des soins",
  restaurer_taxe_habitation_aises               = "Restaurer taxe d'habitation pour aisés",
  retablir_isf                                  = "Rétablir l'ISF",
  retirer_aides_sociales_etrangers              = "Retirer aides sociales aux étrangers",
  soumettre_livret_a_impot                      = "Soumettre intérêts livret A à l'impôt",
  supprimer_abattement_ir_retraites             = "Supprimer abattement d'impôt retraités",
  supprimer_ame                                 = "Supprimer l'Aide Médicale d'État",
  supprimer_avantages_fiscaux_complements_salaire = "Fiscaliser les compléments de salaire",
  supprimer_exonerations_taxes_carburants       = "Supprimer exonérations taxes carburants",
  tva_luxe                                      = "TVA augmentée sur le luxe"
)

labels_effect_program_fr <- c(
  reduire_aide_developpement     = "Réduire aide au développement",
  taxe_millionaires_onu          = "Taxe ONU sur millionnaires",
  fin_dutreil                    = "Fin du pacte Dutreil",
  education_sante                = "Augmenter budget éducation & santé",
  augmenter_allocs_familiales    = "Augmenter allocations familiales",
  reduire_deficit                = "Réduire le déficit",
  reduire_depenses_fonctionnement = "Réduire dépenses de fonctionnement",
  restreindre_aides_etrangers    = "Restreindre aides aux étrangers",
  appliquer_oqtf                 = "Appliquer les OQTF",
  regulariser_sans_papiers       = "Régulariser sans-papiers",
  peines_planchers_recidive      = "Peines planchers (récidive)",
  retraite_65_ans                = "Retraite à 65 ans",
  retraite_62_ans                = "Retraite à 62 ans",
  augmenter_smic                 = "Augmenter le SMIC",
  ric                            = "Référendum d'initiative citoyenne",
  proportionnelle                = "Proportionnelle",
  maintenir_green_deal           = "Maintenir le Green Deal"
)

## ── Coalitions (variables binaires à partir de vote_original) ──────────────
party_lfi   <- "La France insoumise"
party_eelv  <- "Les Écologistes – EÉLV"
party_pcf   <- "Parti Communiste Français"
party_ps    <- "Parti Socaliste & Place publique"   # sic : orthographe du questionnaire
party_centre <- "Renaissance, MoDem & Horizons"
party_lr    <- "Les Républicains"
party_rn    <- "Rassemblement National"
party_recon <- "Reconquête"

coalition_defs <- list(
  LFI                  = party_lfi,
  LFI_EELV_PCF         = c(party_lfi, party_eelv, party_pcf),
  EELV                 = party_eelv,
  EELV_PS_centre       = c(party_eelv, party_ps, party_centre),
  centre               = party_centre,
  PS_centre            = c(party_ps, party_centre),
  PS                   = party_ps,
  PS_centre_LR         = c(party_ps, party_centre, party_lr),
  EELV_PS_centre_LR    = c(party_eelv, party_ps, party_centre, party_lr),
  LR                   = party_lr,
  LR_RN_Reconquete     = c(party_lr, party_rn, party_recon)
)
for (cn in names(coalition_defs)) {
  e[[cn]] <- as.integer(e$vote_original %in% coalition_defs[[cn]])
  label(e[[cn]]) <- paste0(cn, ": coalition binaire (1 = vote_original in {", paste(coalition_defs[[cn]], collapse = ", "), "})")
}
cat("\nCoalitions créées (part des répondants) :\n")
for (cn in names(coalition_defs))
  cat(sprintf("  %-22s %.1f%% (n=%d)\n", cn,
              100 * weighted.mean(e[[cn]], e$no_weight, na.rm = TRUE),
              sum(e[[cn]] == 1, na.rm = TRUE)))

## ── Fonctions de support binaire (1=soutien, 0=rejet, NA=NSP comptés comme soutien) ──
to_bin <- function(x, sup_cats, rej_cats) {
  ifelse(x %in% sup_cats, 1L, ifelse(x %in% rej_cats, 0L, NA_integer_))
}
support_SCS <- function(x) to_bin(x, c("Souhaitable","Convenable","Supportable"), "Inacceptable")
support_CS  <- function(x) to_bin(x, c("Souhaitable","Convenable"), c("Inacceptable","Supportable"))
support_S   <- function(x) to_bin(x, "Souhaitable", c("Inacceptable","Supportable","Convenable"))

make_mat <- function(fn) sapply(vars, function(v) fn(e[[v]]))
mat_SCS  <- make_mat(support_SCS)
mat_CS   <- make_mat(support_CS)
mat_S    <- make_mat(support_S)

## ── Joint support ────────────────────────────────────────────────────────────
## Convention : un NSP compte comme un soutien. Un répondant soutient le paquet
## ssi aucune des mesures n'est marquée « rejet » (0). Les NSP (NA) sont donc
## assimilés à un 1 pour la détermination du soutien conjoint.
joint_support <- function(cols, mat, wgt = e$no_weight) {
  sub   <- mat[, cols, drop = FALSE]
  zeros <- rowSums(sub == 0L, na.rm = TRUE)
  joint <- as.integer(zeros == 0L)
  weighted.mean(joint, wgt, na.rm = TRUE)
}

## ── Apriori générique ─────────────────────────────────────────────────────────
# Retourne $all_feasible : liste de vecteurs d'indices dans vars (1:m)
run_apriori <- function(mat, threshold = THRESHOLD, wgt = e$no_weight, label = "") {
  js       <- function(cols) joint_support(cols, mat, wgt)
  ind_supp <- sapply(seq_len(m), js)
  frequent <- which(ind_supp > threshold)
  nf       <- length(frequent)
  cat(sprintf("\n[%s | >%.0f%%] %d mesures fréquentes\n", label, threshold * 100, nf))
  if (nf == 0) return(list(all_feasible = list()))

  feasible_k   <- as.list(seq_len(nf))
  all_feasible <- lapply(feasible_k, function(i) frequent[i])

  k <- 1L
  repeat {
    k   <- k + 1L
    if (length(feasible_k) < 2) break
    nfp <- length(feasible_k)
    candidates <- list()
    for (i in seq_len(nfp - 1L)) {
      for (j in seq(i + 1L, nfp)) {
        s1 <- feasible_k[[i]]; s2 <- feasible_k[[j]]
        if (k >= 3 && !identical(s1[-length(s1)], s2[-length(s2)])) next
        candidates[[length(candidates) + 1L]] <- c(s1, s2[length(s2)])
      }
    }
    if (length(candidates) == 0) break
    feasible_k <- list()
    for (cand in candidates) {
      cv <- frequent[cand]; sv <- js(cv)
      if (!is.na(sv) && sv > threshold) {
        feasible_k[[length(feasible_k) + 1L]] <- cand
        all_feasible[[length(all_feasible) + 1L]] <- cv
      }
    }
    cat(sprintf("  k=%d: %d faisables\n", k, length(feasible_k)))
    if (length(feasible_k) == 0) break
  }
  list(all_feasible = all_feasible)
}

## ── Rapport d'un paquet (plus grande économie parmi les faisables) ───────────
report_best_economy <- function(res, mat, wgt = e$no_weight, label = "") {
  feas <- res$all_feasible
  if (length(feas) == 0) { cat(sprintf("[%s] Aucun paquet faisable.\n", label)); return(invisible(NULL)) }
  amts <- sapply(feas, pkg_amount)
  best <- feas[[which.max(amts)]]
  js   <- joint_support(best, mat, wgt)
  cat(sprintf("\n[%s] Plus grande économie : %d mesures | %.1f Mds€ | soutien %.1f%%\n  %s\n",
    label, length(best), pkg_amount(best), js * 100,
    paste(short[best], collapse = " + ")))
  # Lister aussi tous les paquets faisables triés par économie décroissante
  ord <- order(amts, decreasing = TRUE)
  cat("  Tous les paquets faisables (économie décroissante) :\n")
  for (idx in ord[seq_len(min(10, length(ord)))]) {
    pkg <- feas[[idx]]
    cat(sprintf("    %.1f Mds€ | %.1f%% | %s\n",
      pkg_amount(pkg), joint_support(pkg, mat, wgt) * 100,
      paste(short[pkg], collapse = " + ")))
  }
  invisible(best)
}

## ═══════════════════════════════════════════════════════════════════════════
## (1)  supp+conv+souh — plus grande économie parmi les paquets majoritaires
## (1bis) conv+souh
## (1ter) souh
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(1) supp+conv+souh — paquet majoritaire à plus grande économie\n")
r_SCS <- run_apriori(mat_SCS, THRESHOLD, label = "SCS")
report_best_economy(r_SCS, mat_SCS, label = "SCS ≥50%")

cat("\n══════════════════════════════════════════════════════════\n")
cat("(1bis) conv+souh — paquet majoritaire à plus grande économie\n")
r_CS  <- run_apriori(mat_CS,  THRESHOLD, label = "CS")
report_best_economy(r_CS,  mat_CS,  label = "CS ≥50%")

cat("\n══════════════════════════════════════════════════════════\n")
cat("(1ter) souh — paquet majoritaire à plus grande économie\n")
r_S   <- run_apriori(mat_S,   THRESHOLD, label = "S")
report_best_economy(r_S,   mat_S,   label = "S ≥50%")

## ═══════════════════════════════════════════════════════════════════════════
## (2) Proportion max P pour un paquet ≥ 90 Mds€ (supp+conv+souh)
## /!\ ~30h to run
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(2) Proportion max P pour paquet ≥ 90 Mds€ (supp+conv+souh)\n")
r_low   <- run_apriori(mat_SCS, THRESHOLD_LOW, label = "SCS_low")
feas_90 <- Filter(function(pkg) pkg_amount(pkg) >= AMOUNT_TARGET, r_low$all_feasible)
if (length(feas_90) > 0) {
  js_90  <- sapply(feas_90, function(pkg) joint_support(pkg, mat_SCS))
  best90 <- feas_90[[which.max(js_90)]]
  cat(sprintf("\nP = %.1f%% | %.1f Mds€ | %d mesures\n  %s\n",
    max(js_90) * 100, pkg_amount(best90), length(best90),
    paste(short[best90], collapse = " + ")))
  cat(sprintf("\n  Top 5 paquets ≥ 90 Mds€ (soutien décroissant) :\n"))
  for (i in order(js_90, decreasing = TRUE)[seq_len(min(5, length(js_90)))]) {
    pkg <- feas_90[[i]]
    cat(sprintf("    %.1f%% | %.1f Mds€ | %s\n",
      js_90[i] * 100, pkg_amount(pkg), paste(short[pkg], collapse = " + ")))
  }
} else {
  cat(sprintf("\nAucun paquet faisable (supp+conv+souh > %.0f%%) avec économie ≥ %d Mds€.\n",
    THRESHOLD_LOW * 100, AMOUNT_TARGET))
}

# P = 25.0% | 90.1 Mds€ | 10 mesures
# eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_ame + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# 
# Top 5 paquets ≥ 90 Mds€ (soutien décroissant) :
#   25.0% | 90.1 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_ame + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# 24.5% | 90.7 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + augmenter_duree_travail_droit_chomage + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# 24.2% | 90.8 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + supprimer_avantages_fiscaux_complements_salaire + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# 24.2% | 90.0 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_taxe_revenus_capital + augmenter_impot_revenu_aises
# 23.8% | 91.7 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + augmenter_duree_travail_droit_chomage + augmenter_age_retraite_65 + supprimer_exonerations_taxes_carburants + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe

## ═══════════════════════════════════════════════════════════════════════════
## (3) Paquets majoritaires par groupe de votants (supp+conv+souh)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(3) Paquets majoritaires au sein de chaque groupe de votants\n")
voter_groups <- list(
  "Left"             = !is.na(e$vote_agg) & e$vote_agg == 0,
  "Center-right"     = !is.na(e$vote_agg) & e$vote_agg == 1,
  "Far right"        = !is.na(e$vote_agg) & e$vote_agg == 2
)
for (gname in names(voter_groups)) {
  mask  <- voter_groups[[gname]]
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Groupe : %s (n=%d) ──\n", gname, sum(mask)))
  rg <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = gname)
  report_best_economy(rg, mat_SCS, wgt = wgt_g, label = gname)
}

## ═══════════════════════════════════════════════════════════════════════════
## (3bis) Paquets majoritaires par paire de groupes de votants
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(3bis) Paquets majoritaires au sein de paires de groupes\n")
pair_groups <- list(
  "Left + Far right"          = !is.na(e$vote_agg) & e$vote_agg %in% c(0, 2),
  "Center-right + Far right"  = !is.na(e$vote_agg) & e$vote_agg %in% c(1, 2),
  "Center-right + Left"       = !is.na(e$vote_agg) & e$vote_agg %in% c(0, 1)
)
for (gname in names(pair_groups)) {
  mask  <- pair_groups[[gname]]
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Paire : %s (n=%d) ──\n", gname, sum(mask)))
  rg <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = gname)
  report_best_economy(rg, mat_SCS, wgt = wgt_g, label = gname)
}

## ═══════════════════════════════════════════════════════════════════════════
## (3ter) Paquets majoritaires par coalition (supp+conv+souh)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(3ter) Paquets majoritaires au sein de chaque coalition (SCS)\n")
for (cn in names(coalition_defs)) {
  mask  <- e[[cn]] == 1 & !is.na(e[[cn]])
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Coalition : %s (n=%d) ──\n", cn, sum(mask)))
  rg <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = cn)
  report_best_economy(rg, mat_SCS, wgt = wgt_g, label = cn)
}

## ═══════════════════════════════════════════════════════════════════════════
## (3quater) Paquets majoritaires en CS (conv+souh) par bloc, paire, coalition
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(3quater-a) Paquets majoritaires CS — par bloc de votants\n")
for (gname in names(voter_groups)) {
  mask  <- voter_groups[[gname]]
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Bloc : %s (n=%d) ──\n", gname, sum(mask)))
  rg <- run_apriori(mat_CS, THRESHOLD, wgt = wgt_g, label = paste0("CS_", gname))
  report_best_economy(rg, mat_CS, wgt = wgt_g, label = paste0("CS ", gname))
}

cat("\n══════════════════════════════════════════════════════════\n")
cat("(3quater-b) Paquets majoritaires CS — par paire de blocs\n")
for (gname in names(pair_groups)) {
  mask  <- pair_groups[[gname]]
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Paire : %s (n=%d) ──\n", gname, sum(mask)))
  rg <- run_apriori(mat_CS, THRESHOLD, wgt = wgt_g, label = paste0("CS_", gname))
  report_best_economy(rg, mat_CS, wgt = wgt_g, label = paste0("CS ", gname))
}

cat("\n══════════════════════════════════════════════════════════\n")
cat("(3quater-c) Paquets majoritaires CS — par coalition\n")
for (cn in names(coalition_defs)) {
  mask  <- e[[cn]] == 1 & !is.na(e[[cn]])
  wgt_g <- ifelse(mask, e$no_weight, 0)
  cat(sprintf("\n── Coalition : %s (n=%d) ──\n", cn, sum(mask)))
  rg <- run_apriori(mat_CS, THRESHOLD, wgt = wgt_g, label = paste0("CS_", cn))
  report_best_economy(rg, mat_CS, wgt = wgt_g, label = paste0("CS ", cn))
}

## ═══════════════════════════════════════════════════════════════════════════
## (4) Paquet maximisant la somme des utilités sous contrainte ≥ 90 Mds€
##     Utilité d'une mesure = moyenne pondérée des valeurs (-1/0/1/2)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(4) Paquet maximisant l'utilité totale sous contrainte ≥ 90 Mds€\n")

util_val <- function(x) case_when(
  x == "Souhaitable"  ~  3,
  x == "Convenable"   ~  1,
  x == "Supportable"  ~ -1,
  x == "Inacceptable" ~ -3,
  TRUE ~ NA_real_
)
u_mean <- sapply(vars, function(v) weighted.mean(util_val(e[[v]]), e$no_weight, na.rm = TRUE))
names(u_mean) <- short

# 0/1 knapsack DP : maximiser ∑ u_mean[i] pour i ∈ S sous ∑ amounts[i] ≥ 90
amt_int  <- pmax(1L, as.integer(round(amounts * 10)))  # unités de 0.1 Mds
tot_int  <- sum(amt_int)
tgt_int  <- as.integer(AMOUNT_TARGET * 10)             # 900 unités

# dp[j+1] = max utilité totale avec exactement j unités d'économie
dp        <- rep(-Inf, tot_int + 1L)
dp[1L]    <- 0
items_dp  <- vector("list", tot_int + 1L)
items_dp[[1L]] <- integer(0)

for (i in seq_len(m)) {
  ai <- amt_int[i]; ui <- u_mean[i]
  for (j in rev(seq(ai, tot_int))) {   # sens inverse : knapsack 0/1
    prev <- j - ai + 1L; curr <- j + 1L
    if (dp[prev] > -Inf && dp[prev] + ui > dp[curr]) {
      dp[curr]        <- dp[prev] + ui
      items_dp[[curr]] <- c(items_dp[[prev]], i)
    }
  }
}

valid_j <- which(seq_len(tot_int + 1L) - 1L >= tgt_int & dp > -Inf)
if (length(valid_j) > 0) {
  best_j    <- valid_j[which.max(dp[valid_j])]
  best_pkg4 <- items_dp[[best_j]]
  cat(sprintf("\nUtilité totale : %.3f | %.1f Mds€ | %d mesures\n  %s\n",
    dp[best_j], (best_j - 1L) / 10, length(best_pkg4),
    paste(short[best_pkg4], collapse = " + ")))
  cat("  Détail par mesure (u = utilité moyenne) :\n")
  ord_u <- order(u_mean[best_pkg4], decreasing = TRUE)
  for (i in best_pkg4[ord_u])
    cat(sprintf("    %-42s  u=%+.3f  %4.1f Mds€\n", short[i], u_mean[i], amounts[i]))
  cat(sprintf("\n  Note : utilité moyenne par mesure sur l'ensemble (%d mesures) :\n", m))
  for (i in order(u_mean, decreasing = TRUE))
    cat(sprintf("    %-42s  u=%+.3f  %4.1f Mds€\n", short[i], u_mean[i], amounts[i]))
} else {
  cat("  Aucune solution knapsack trouvée.\n")
}

## ═══════════════════════════════════════════════════════════════════════════
## (5) Graphique : positionnement idéologique × soutien (conv+souh)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(5) Graphique positionnement × soutien (conv+souh)\n")

# x = moyenne(vote_agg - 1) parmi non-PNR/Other ET conv+souh = 1
#   → Left=-1, Center-right=0, Far right=+1
pos_x <- sapply(seq_len(m), function(i) {
  cs   <- mat_CS[, i]
  mask <- !is.na(e$vote_agg) & e$vote_agg != -1 & !is.na(cs) & cs == 1L
  if (sum(mask) == 0) return(NA_real_)
  weighted.mean(e$vote_agg[mask] - 1, e$no_weight[mask])
})

# y = taux de soutien conv+souh (tous répondants, pondéré, NSP = soutien)
cs_rate <- sapply(seq_len(m), function(i) joint_support(i, mat_CS))

df5 <- data.frame(measure = short, x = pos_x, y = cs_rate, stringsAsFactors = FALSE)
df5 <- df5[!is.na(df5$x), ]

p5 <- ggplot(df5, aes(x = x, y = y, label = measure)) +
  geom_point(size = 2.5, color = "steelblue") +
  geom_text_repel(size = 2.8, max.overlaps = 30, segment.color = "grey60") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(NA, NA), expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_continuous(limits = c(-1.2, 1.2),
    breaks = c(-1, 0, 1),
    labels = c("Gauche\n(−1)", "Centre-droit\n(0)", "Extrême-droite\n(+1)")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    x     = "Positionnement idéologique des partisans (conv+souh) — moyenne(vote_agg − 1)",
    y     = "Taux de soutien conv+souh (%, pondéré, NSP = soutien)",
    title = "Positionnement politique des partisans vs taux de soutien"
  ) +
  theme_bw(base_size = 11)

ggsave("../figures/positionnement_soutien.pdf", p5, width = 11, height = 7)
cat("  → ../figures/positionnement_soutien.pdf\n")

## ═══════════════════════════════════════════════════════════════════════════
## (6) Notes moyennes par mesure et groupe de votants
##     effect_program (−2→+2) et budget (−1→+2)
##     Lignes par mesure, 4 points (Overall / Left / Center-right / Far right)
##     Barres d'erreur = ±1 SE (erreur standard de la moyenne pondérée)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════\n")
cat("(6) Notes moyennes par mesure et groupe de votants\n")

ep_score  <- function(x) case_when(
  x == "Beaucoup plus favorable"  ~  2, x == "Plus favorable"           ~  1,
  x == "Ne changerait rien"       ~  0, x == "Moins favorable"          ~ -1,
  x == "Beaucoup moins favorable" ~ -2, TRUE ~ NA_real_)
bud_score <- function(x) case_when(
  x == "Souhaitable"  ~  2, x == "Convenable"   ~  1,
  x == "Supportable"  ~  0, x == "Inacceptable" ~ -1,
  TRUE ~ NA_real_)

wt_mean_mad_asym <- function(x, w) {
  # Mean + asymmetric mean-abs-deviation from the mean:
  #   mad_lo = E[μ − x | x < μ]   ;   mad_hi = E[x − μ | x > μ]
  ok <- !is.na(x) & w > 0
  xv <- x[ok]; wv <- w[ok]
  if (length(xv) < 2) return(c(mean = mean(xv, na.rm = TRUE),
                               mad_lo = NA_real_, mad_hi = NA_real_))
  mu <- weighted.mean(xv, wv)
  below <- xv < mu; above <- xv > mu
  mad_lo <- if (any(below)) sum(wv[below] * (mu - xv[below])) / sum(wv[below]) else 0
  mad_hi <- if (any(above)) sum(wv[above] * (xv[above] - mu)) / sum(wv[above]) else 0
  c(mean = mu, mad_lo = mad_lo, mad_hi = mad_hi)
}

group_defs <- list(
  "Overall"       = rep(TRUE, nrow(e)),
  "Left"          = !is.na(e$vote_agg) & e$vote_agg == 0,
  "Center-right"  = !is.na(e$vote_agg) & e$vote_agg == 1,
  "Far right"     = !is.na(e$vote_agg) & e$vote_agg == 2
)

compute_stats <- function(variables, score_fn, label_map) {
  rows <- lapply(variables, function(v) {
    sc    <- score_fn(e[[v]])
    key   <- sub("^budget_|^effect_program_", "", v)
    vname <- if (key %in% names(label_map)) unname(label_map[key]) else gsub("_", " ", key)
    lapply(names(group_defs), function(gname) {
      w_g <- e$no_weight * ifelse(group_defs[[gname]], 1, 0)
      ms  <- wt_mean_mad_asym(sc, w_g)
      data.frame(measure = vname, group = gname,
                 mean = ms["mean"], mad_lo = ms["mad_lo"], mad_hi = ms["mad_hi"],
                 stringsAsFactors = FALSE, row.names = NULL)
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

df_ep  <- compute_stats(variables_effect_program, ep_score,  labels_effect_program_fr)
df_bud <- compute_stats(variables_budget,         bud_score, labels_budget_fr)

## ── Rendu des graphiques "notes_groupes" ───────────────────────────────────
# - Décalage vertical des groupes (position_dodge) pour éviter le chevauchement
# - Grille horizontale entre les items (minor breaks aux demi-entiers)
# - Traduction des groupes en français
# - Overall en noir ; Gauche prend l'ancienne couleur d'Overall (rouge par défaut)
group_levels_fr <- c("Overall" = "Ensemble",
                     "Left" = "Gauche",
                     "Center-right" = "Centre-droit / droite",
                     "Far right" = "Extrême-droite")
# Couleurs inspirées de ggplot hue_pal()(4) pour Left/Center-right/Far right,
# avec "Ensemble" ramené au noir et "Gauche" repositionnée sur l'ancien rouge d'Overall.
colors_groups <- c(
  "Ensemble"              = "black",
  "Gauche"                = "#F8766D",
  "Centre-droit / droite" = "#619CFF",
  "Extrême-droite"        = "#A020F0"   # violet
)

plot_lines <- function(df, title, xlab, show_vline = TRUE) {
  # Ordre des mesures par la note moyenne d'Ensemble (décroissant)
  order_df <- df[df$group == "Overall", ]
  order_df <- order_df[order(order_df$mean), ]
  df$measure <- factor(df$measure, levels = order_df$measure)
  df$group_fr <- factor(group_levels_fr[df$group],
                        levels = unname(group_levels_fr))
  dodge <- position_dodge(width = 0.7)
  n_items <- nlevels(df$measure)
  minor_y <- seq(0.5, n_items - 0.5, by = 1)
  # Axe x borné aux extrêmes mean - mad_lo / mean + mad_hi (avec petite marge)
  xmin <- min(df$mean - df$mad_lo, na.rm = TRUE)
  xmax <- max(df$mean + df$mad_hi, na.rm = TRUE)
  pad  <- (xmax - xmin) * 0.03
  xlim_range <- c(xmin - pad, xmax + pad)
  p <- ggplot(df, aes(y = measure, x = mean, color = group_fr, group = group_fr)) +
    geom_hline(yintercept = minor_y, color = "grey85", linewidth = 0.3) +
    geom_errorbarh(aes(xmin = mean - mad_lo, xmax = mean + mad_hi),
                   height = 0, alpha = 0.7, linewidth = 0.25,
                   position = dodge) +
    geom_point(size = 2.1, position = dodge) +
    scale_color_manual(values = colors_groups, drop = FALSE) +
    coord_cartesian(xlim = xlim_range) +
    labs(y = NULL, x = xlab,
         # title = title,
         color = "Groupe",
         caption = "Point = moyenne pondérée ; barres = écart moyen à la moyenne (asymétrique : en-dessous / au-dessus de μ)") +
    theme_bw(base_size = 10) +
    theme(
      legend.position    = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3)
    ) +
    scale_y_discrete(expand = expansion(add = 0.5))
  if (show_vline && xmin < 0 && xmax > 0) {
    p <- p + geom_vline(xintercept = 0, linetype = "dotted", color = "grey40")
  }
  p
}

p6_ep  <- plot_lines(df_ep,
                     "Effet d'une mesure dans un programme présidentiel (note moyenne par groupe)",
                     "Favorabilité moyenne à un candidat portant la mesure (-2 = beaucoup moins favorable ; +2 = beaucoup plus favorable)",
                     show_vline = TRUE)
p6_bud <- plot_lines(df_bud,
                     "Jugement sur les mesures budgétaires (note moyenne par groupe)",
                     "Jugement moyen (-1 = inacceptable ; 0 = supportable ; 1 = convenable ; 2 = souhaitable)",
                     show_vline = FALSE)

# Format A4 portrait (largeur 8.27")
ggsave("../figures/notes_groupes_effect_program.pdf", p6_ep,  width = 8.27, height = 7)
ggsave("../figures/notes_groupes_budget.pdf",         p6_bud, width = 8.27, height = 11)
cat("  → ../figures/notes_groupes_effect_program.pdf\n")
cat("  → ../figures/notes_groupes_budget.pdf\n")

## ═══════════════════════════════════════════════════════════════════════════
## (7) Matrice de distances entre groupes de votants
##     Distance = somme des |écarts| de notes moyennes sur toutes les mesures
##     (effect_program + budget, scales naturelles : −2/+2 et −1/+2)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════\n")
cat("(7) Matrice de distances entre groupes de votants\n")

dist_groups <- c(
  list(
    "Overall"                  = rep(TRUE, nrow(e)),
    "Left"                     = !is.na(e$vote_agg) & e$vote_agg == 0,
    "Center-right"             = !is.na(e$vote_agg) & e$vote_agg == 1,
    "Far right"                = !is.na(e$vote_agg) & e$vote_agg == 2,
    "Left + Far right"         = !is.na(e$vote_agg) & e$vote_agg %in% c(0, 2),
    "Center-right + Far right" = !is.na(e$vote_agg) & e$vote_agg %in% c(1, 2),
    "Center-right + Left"      = !is.na(e$vote_agg) & e$vote_agg %in% c(0, 1)
  ),
  # Ajouter les coalitions
  lapply(coalition_defs, function(parties) {
    !is.na(e$vote_original) & e$vote_original %in% parties
  })
)

# Labels français pour l'affichage des matrices de distances (axe y : complet)
group_labels_fr <- c(
  "Overall"                  = "Ensemble",
  "Left"                     = "Gauche",
  "Center-right"             = "Centre + LR",
  "Far right"                = "Extrême-droite",
  "Left + Far right"         = "Gauche + Extr. droite",
  "Center-right + Far right" = "Centre + LR + Extr. droite",
  "Center-right + Left"      = "Gauche + Centre + LR",
  "LFI"                      = "LFI",
  "LFI_EELV_PCF"             = "LFI + LÉ + PCF",
  "EELV"                     = "LÉ",
  "EELV_PS_centre"           = "LÉ + PS + Centre",
  "centre"                   = "Centre",
  "PS_centre"                = "PS + Centre",
  "PS"                       = "PS",
  "PS_centre_LR"             = "PS + Centre + LR",
  "EELV_PS_centre_LR"        = "LÉ + PS + Centre + LR",
  "LR"                       = "LR",
  "LR_RN_Reconquete"         = "LR + RN + Reconquête"
)
# Labels abrégés (axe x, haut + bas)
group_labels_short <- c(
  "Overall"                  = "Ens.",
  "Left"                     = "G",
  "Center-right"             = "C+LR",
  "Far right"                = "ED",
  "Left + Far right"         = "G+ED",
  "Center-right + Far right" = "C+LR+ED",
  "Center-right + Left"      = "G+C+LR",
  "LFI"                      = "LFI",
  "LFI_EELV_PCF"             = "LFI+LÉ+PCF",
  "EELV"                     = "LÉ",
  "EELV_PS_centre"           = "LÉ+PS+C",
  "centre"                   = "C",
  "PS_centre"                = "PS+C",
  "PS"                       = "PS",
  "PS_centre_LR"             = "PS+C+LR",
  "EELV_PS_centre_LR"        = "LÉ+PS+C+LR",
  "LR"                       = "LR",
  "LR_RN_Reconquete"         = "LR+RN+Rec."
)

group_mean_vec <- function(variables, score_fn) {
  # Retourne matrice : mesures × groupes
  sapply(names(dist_groups), function(gname) {
    w_g <- e$no_weight * ifelse(dist_groups[[gname]], 1, 0)
    sapply(variables, function(v) weighted.mean(score_fn(e[[v]]), w_g, na.rm = TRUE))
  })
}

means_ep  <- group_mean_vec(variables_effect_program,  ep_score)   # (n_ep)  × 4
means_bud <- group_mean_vec(variables_budget, bud_score)  # (n_bud) × 4
means_all <- rbind(means_ep, means_bud)                            # all measures × 4

gnames  <- names(dist_groups)
ng      <- length(gnames)
dist_mat <- matrix(0, ng, ng, dimnames = list(gnames, gnames))
for (i in seq_len(ng))
  for (j in seq_len(ng))
    if (i != j) dist_mat[i, j] <- sum(abs(means_all[, i] - means_all[, j]), na.rm = TRUE)

cat("\nMatrice de distances (∑|Δ note| sur toutes les mesures effect_program + budget) :\n")
print(round(dist_mat, 3))

## Distance moyenne entre deux individus (tirés indépendamment, pondérés)
## Pour chaque mesure : E[|X_i − X_j|] = ∑_{a,b} p_a p_b |v_a − v_b|
## Puis somme sur toutes les mesures — même échelle que la matrice ci-dessus.
avg_indiv_dist <- function(variables, score_fn) {
  sum(sapply(variables, function(v) {
    x  <- score_fn(e[[v]])
    ok <- !is.na(x) & e$no_weight > 0
    xv <- x[ok]; wv <- e$no_weight[ok]
    if (length(xv) < 2) return(0)
    tab <- tapply(wv, xv, sum)
    p   <- as.numeric(tab) / sum(tab)
    vs  <- as.numeric(names(tab))
    sum(outer(vs, vs, function(a, b) abs(a - b)) * outer(p, p))
  }))
}
d_indiv_ep  <- avg_indiv_dist(variables_effect_program, ep_score)
d_indiv_bud <- avg_indiv_dist(variables_budget,         bud_score)
d_indiv_tot <- d_indiv_ep + d_indiv_bud
cat(sprintf(
  "\nDistance moyenne entre deux individus (référence, même échelle) : %.3f\n  dont effect_program : %.3f | budget : %.3f\n",
  d_indiv_tot, d_indiv_ep, d_indiv_bud))
cat("  → Ratio distance inter-groupes / distance inter-individus :\n")
print(round(dist_mat / d_indiv_tot, 3))

## ── Matrice de distances inter-individuelles entre (et au sein des) coalitions ──
## d(A, B) = E[|X_i − X_j|] avec i tiré (pondéré) dans A et j (indépendamment) dans B
## Pour chaque mesure : ∑_{a,b} p_A(a) p_B(b) |v_a − v_b|, puis somme sur mesures.
## Sur la diagonale : distance moyenne entre deux individus du même groupe.
cat("\nMatrice de distances inter-individuelles (par paires de tirages A×B) :\n")
score_list <- c(
  lapply(variables_effect_program, function(v) ep_score(e[[v]])),
  lapply(variables_budget,         function(v) bud_score(e[[v]]))
)
# Pré-calcul : pour chaque groupe × mesure, distribution pondérée (v, p)
group_dist <- lapply(dist_groups, function(mask) {
  lapply(score_list, function(x) {
    ok <- mask & !is.na(x) & e$no_weight > 0
    if (!any(ok)) return(list(v = numeric(0), p = numeric(0)))
    xv <- x[ok]; wv <- e$no_weight[ok]
    tab <- tapply(wv, xv, sum)
    list(v = as.numeric(names(tab)), p = as.numeric(tab) / sum(tab))
  })
})
dist_mat_indiv <- matrix(0, ng, ng, dimnames = list(gnames, gnames))
for (i in seq_len(ng)) for (j in seq(i, ng)) {
  d <- 0
  for (k in seq_along(score_list)) {
    dA <- group_dist[[i]][[k]]; dB <- group_dist[[j]][[k]]
    if (length(dA$v) == 0 || length(dB$v) == 0) next
    d <- d + sum(outer(dA$v, dB$v, function(a, b) abs(a - b)) * outer(dA$p, dB$p))
  }
  dist_mat_indiv[i, j] <- d
  dist_mat_indiv[j, i] <- d
}
print(round(dist_mat_indiv, 3))

## ── Export des matrices en heatmaps (cellules d'autant plus sombres que d est faible) ──
# Ordre d'affichage des lignes/colonnes dans les matrices
display_order <- c(
  "Overall",
  "Left", "Center-right", "Far right",
  "Center-right + Left", "Center-right + Far right", "Left + Far right",
  "LFI", "LFI_EELV_PCF",
  "EELV", "PS",
  "EELV_PS_centre", "PS_centre", "centre",
  "PS_centre_LR", "EELV_PS_centre_LR",
  "LR", "LR_RN_Reconquete"
)

plot_dist_heatmap <- function(mat, title, outfile) {
  # Réordonner selon display_order
  ord <- display_order[display_order %in% rownames(mat)]
  mat <- mat[ord, ord, drop = FALSE]
  # Labels : complets sur l'axe y, abrégés sur l'axe x (haut + bas)
  rn_fr    <- group_labels_fr[rownames(mat)]
  cn_short <- group_labels_short[colnames(mat)]
  df <- as.data.frame(as.table(mat))
  names(df) <- c("A", "B", "dist")
  df$A <- factor(group_labels_fr[as.character(df$A)],    levels = rev(rn_fr))
  df$B <- factor(group_labels_short[as.character(df$B)], levels = cn_short)
  nr <- length(levels(df$A)); nc <- length(levels(df$B))
  # Texte blanc quand la cellule est très foncée (|écart| élevé)
  text_thresh <- max(abs(range(df$dist, na.rm = TRUE))) * 0.55
  p <- ggplot(df, aes(x = B, y = A, fill = dist)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.1f", dist),
                  color = abs(dist) > text_thresh),
              size = 1.9, show.legend = FALSE) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black")) +
    scale_fill_gradient2(low = "#2b6cb0", mid = "white", high = "#c53030",
                         midpoint = 0, name = "Écart vs\nintra-Ensemble",
                         labels = function(x) sprintf("%+.0f%%", x)) +
    coord_fixed(clip = "off") +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title         = element_text(size = 9, hjust = 0,
                                         margin = margin(t = -32, b = 22, l = -55)),
      axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5),
      axis.text.y        = element_text(size = 6.5),
      panel.grid         = element_blank(),
      legend.position    = "right",
      legend.key.height  = grid::unit(0.8, "cm"),
      plot.margin        = margin(t = 25, r = 5, b = 5, l = 5)
    ) +
    ggplot2::annotate("text",
             x = seq_len(nc),
             y = nr + 0.85,
             label = levels(df$B),
             angle = 45, hjust = 0, vjust = 0.5, size = 2.1)
  ggsave(outfile, p, width = 6.5, height = 5.5)
  cat("  →", outfile, "\n")
}

# Normalisation par la distance intra-Ensemble (≈ 54.7 : distance moyenne entre
# deux individus de la population complète) pour rendre les valeurs comparables
# et centrées sur 1.
ref_dist <- dist_mat_indiv["Overall", "Overall"]
cat(sprintf("\nDistance de référence (intra-Ensemble) : %.3f — écart exprimé en %% (d/ref − 1).\n", ref_dist))
# Écart en points de % par rapport à la distance intra-Ensemble
dist_mat_norm       <- (dist_mat       / ref_dist - 1) * 100
dist_mat_indiv_norm <- (dist_mat_indiv / ref_dist - 1) * 100

plot_dist_heatmap(
  dist_mat_norm,
  "Distance entre groupes (∑ |Δ moyennes|) : écart vs. intra-Ensemble (en %)",
  "../figures/distance_matrix_means.pdf"
)
plot_dist_heatmap(
  dist_mat_indiv_norm,
  # "Distance inter-individuelle moyenne entre (et au sein des) groupes : écart vs. intra-Ensemble (en %)",
  NULL,
  "../figures/distance_matrix_pairwise.pdf"
)

cat("\nTerminé.\n")
Sys.time() - start # 15h

# [SCS ≥50%] Plus grande économie : 6 mesures | 68.1 Mds€ | soutien 50.5% | 
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe
#   also feasible among: CR+L (52.5%), L+FR (50.2%), L (?), LFI_EELV_PCF (?), EELV_PS_centre_LR (50.3%), EELV_PS_centre (53%), PS_centre (51.1%)
# [Left] Plus grande économie : 9 mesures | 83.3 Mds€ | soutien 50.3%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_taxe_revenus_capital + augmenter_impot_revenu_aises
# [Center-right] Plus grande économie : 8 mesures | 71.9 Mds€ | soutien 50.0%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + augmenter_duree_travail_droit_chomage + geler_aides_sociales + augmenter_age_retraite_65 + retablir_isf + tva_luxe
# [Far right] Plus grande économie : 7 mesures | 68.3 Mds€ | soutien 50.6%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_ame + supprimer_exonerations_taxes_carburants + retablir_isf + augmenter_impot_revenu_aises
# [Left + Far right] Plus grande économie : 6 mesures | 68.1 Mds€ | soutien 50.2%
#   same as full sample
# [Center-right + Far right] Plus grande économie : 7 mesures | 67.1 Mds€ | soutien 51.4%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_ame + supprimer_exonerations_taxes_carburants + retablir_isf + tva_luxe
# [Center-right + Left] Plus grande économie : 6 mesures | 68.4 Mds€ | soutien 50.6%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + tva_luxe
# [LFI_EELV_PCF] Plus grande économie : 8 mesures | 85.1 Mds€ | soutien 50.8%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + reduire_depenses_militaires + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + tva_luxe + augmenter_impot_revenu_aises
# [PS_centre] Plus grande économie : 6 mesures | 68.9 Mds€ | soutien 50.5%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + augmenter_age_retraite_65 + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe
# [EELV_PS_centre] Plus grande économie : 8 mesures | 69.1 Mds€ | soutien 50.8%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_taxe_revenus_capital + augmenter_impot_revenu_aises
# [PS_centre_LR] Plus grande économie : 6 mesures | 63.2 Mds€ | soutien 53.6%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe
# [EELV_PS_centre_LR] Plus grande économie : 6 mesures | 68.1 Mds€ | soutien 50.3%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe
# [LR_RN_Reconquete] Plus grande économie : 7 mesures | 67.7 Mds€ | soutien 50.4%
#   eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + augmenter_duree_travail_droit_chomage + supprimer_exonerations_taxes_carburants + retablir_isf + tva_luxe
# LFI: 95.3
# Centre: 75.6
# [CS ≥50%] Plus grande économie : 3 mesures | 41.4 Mds€ | soutien 52.7% | liminer_doublons_territoriaux + geler_depenses_etat_collectivites + retablir_isf
# Left: 41.8; CR: 45.4; FR: 49.4; L+FR: 41.4; CR+FR:41.4; L+CR: 33.9; LFI+EELV+PCF: 46.5; PS+centre: 33.9; EELV+PS+centre: 33.9; PS+centre+LR: 37.1; EELV+PS+centre+LR: 33.9; LR+FR: 48.1; LFI: 55
# Top 5 paquets ≥ 90 Mds€ 
# 1st: 33.7% | 90.1 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + retirer_aides_sociales_etrangers + supprimer_ame + supprimer_exonerations_taxes_carburants + restaurer_taxe_habitation_aises + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# 5th: 31.9% | 90.8 Mds€ | eliminer_doublons_territoriaux + geler_depenses_etat_collectivites + supprimer_exonerations_taxes_carburants + supprimer_avantages_fiscaux_complements_salaire + retablir_isf + augmenter_impot_heritages_eleves + tva_luxe + augmenter_impot_revenu_aises
# Utilité totale : 10.895 | 96.9 Mds€ | 13 mesures
# 3 blocs font sens: EELV et PS sont plus proches de LFI que du centre, centre plus proche de LR que de PS ou EELV, LR plus proche de centre que d'ED
# Distance moyenne entre deux individus : 54.7