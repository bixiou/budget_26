library(Hmisc)
load(".RData")
library(ggplot2)

vote_agg_n <- as.numeric(e$vote_agg)

h_defs <- list(
  "Overall"           = NULL,
  "Left"              = NULL,
  "Center-right"      = NULL,
  "Far right"         = NULL,
  "EELV_PS_centre"    = c("Les Écologistes – EÉLV", "Parti Socaliste & Place publique", "Renaissance, MoDem & Horizons"),
  "PS_centre"         = c("Parti Socaliste & Place publique", "Renaissance, MoDem & Horizons"),
  "EELV_PS_centre_LR" = c("Les Écologistes – EÉLV", "Parti Socaliste & Place publique", "Renaissance, MoDem & Horizons", "Les Républicains"),
  "LR_RN_Reconquete"  = c("Les Républicains", "Rassemblement National", "Reconquête"),
  "LFI"               = "La France insoumise",
  "EELV"              = "Les Écologistes – EÉLV",
  "centre"            = "Renaissance, MoDem & Horizons",
  "PS"                = "Parti Socaliste & Place publique",
  "LR"                = "Les Républicains"
)
h_lbl <- c(
  "Overall" = "Ensemble", "Left" = "Gauche", "Center-right" = "Centre + LR",
  "Far right" = "Extrême-droite", "EELV_PS_centre" = "LÉ + PS + C",
  "PS_centre" = "PS + C", "EELV_PS_centre_LR" = "LÉ + PS + C + LR",
  "LR_RN_Reconquete" = "LR + Extr.-droite",
  "LFI" = "LFI", "EELV" = "LÉ", "centre" = "Centre", "PS" = "PS", "LR" = "LR"
)
h_masks <- lapply(names(h_defs), function(cn) {
  if (cn == "Overall")       rep(TRUE, nrow(e))
  else if (cn == "Left")     !is.na(vote_agg_n) & vote_agg_n == 0
  else if (cn == "Center-right") !is.na(vote_agg_n) & vote_agg_n == 1
  else if (cn == "Far right") !is.na(vote_agg_n) & vote_agg_n == 2
  else !is.na(e$vote_original) & e$vote_original %in% h_defs[[cn]]
})
names(h_masks) <- names(h_defs)

ba_h <- as.data.frame(sapply(variables_budget, function(v)
  ifelse(as.character(e[[v]]) %in% c("Souhaitable", "Convenable"), 1L,
         ifelse(as.character(e[[v]]) %in% c("Supportable", "Inacceptable"), 0L, NA_integer_))))

h_amt   <- setNames(budget_policies$amount[match(variables_budget, budget_policies$variable_name)],
                    variables_budget)
h_short <- sub("budget_", "", variables_budget)

h_lbf <- c(
  aligner_tva_restauration = "Aligner TVA restauration",
  augmenter_age_retraite_65 = "Augmenter âge retraite à 65 ans",
  augmenter_cotisations_salaires_moyens = "Augmenter cotisations salaires moyens",
  augmenter_csg_1pt = "Augmenter CSG (+1 pt)",
  augmenter_duree_travail_droit_chomage = "Augmenter durée travail/chômage",
  augmenter_impot_heritages_eleves = "Augmenter impôt héritages élevés",
  augmenter_impot_revenu_aises = "Augmenter impôt revenu aisés",
  augmenter_impot_revenu_tous = "Augmenter impôt revenu (tous)",
  augmenter_impot_societes = "Augmenter impôt sociétés",
  augmenter_taxe_revenus_capital = "Augmenter taxe revenus du capital",
  augmenter_tva_1pt = "Augmenter TVA (+1 pt)",
  diminuer_credit_impot_recherche = "Diminuer Crédit Impôt Recherche",
  diminuer_subventions_ecole_privee = "Diminuer subventions école privée",
  eliminer_doublons_territoriaux = "Éliminer doublons territoriaux",
  geler_aides_sociales = "Geler aides sociales",
  geler_depenses_etat_collectivites = "Geler dépenses État/collectivités",
  reduire_aides_apprentissage = "Réduire aides apprentissage",
  reduire_depenses_educatives_demographie = "Réduire dépenses éducatives",
  reduire_depenses_militaires = "Réduire dépenses militaires",
  reduire_pensions_retraite = "Réduire pensions de retraite",
  reduire_remboursement_soins = "Réduire remboursement soins",
  restaurer_taxe_habitation_aises = "Restaurer taxe habitation aisés",
  retablir_isf = "Rétablir l'ISF",
  retirer_aides_sociales_etrangers = "Retirer aides aux étrangers",
  soumettre_livret_a_impot = "Livret A à l'impôt",
  supprimer_abattement_ir_retraites = "Supprimer abattement IR retraites",
  supprimer_ame = "Supprimer l'AME",
  supprimer_avantages_fiscaux_complements_salaire = "Fiscaliser compléments de salaire",
  supprimer_exonerations_taxes_carburants = "Supprimer ex. taxes carburants",
  tva_luxe = "TVA augmentée sur le luxe"
)

cs_mat <- sapply(names(h_masks), function(cn) {
  wg <- ifelse(h_masks[[cn]], e$weight, 0)
  sapply(variables_budget, function(v) {
    y <- ba_h[[v]]; ok <- !is.na(y) & wg > 0
    if (!any(ok)) return(NA_real_)
    sum(y[ok] * wg[ok]) / sum(wg[ok])
  })
})
rownames(cs_mat) <- variables_budget

row_ord  <- order(cs_mat[, "Overall"])
pol_lbl  <- paste0(h_lbf[h_short], " (", gsub("\\.", ",", sprintf("%.1f", h_amt)), " Mds€)")
names(pol_lbl) <- variables_budget
pol_levs <- pol_lbl[variables_budget[row_ord]]

df_h <- expand.grid(measure = variables_budget, coalition = names(h_masks), stringsAsFactors = FALSE)
df_h$rate    <- mapply(function(m, c) cs_mat[m, c], df_h$measure, df_h$coalition)
df_h$pol_lbl <- factor(pol_lbl[df_h$measure], levels = pol_levs)
df_h$col_lbl <- factor(h_lbl[df_h$coalition],  levels = h_lbl)
df_h$txt_col <- ifelse(is.na(df_h$rate) | df_h$rate < 0.55, "black", "white")
face_x_h     <- ifelse(h_lbl == h_lbl["Overall"], "bold", "plain")

p_coal_supp <- ggplot(df_h, aes(x = col_lbl, y = pol_lbl, fill = rate)) +
  geom_tile(color = "white", linewidth = 0.3, width = 0.92) +
  geom_text(aes(label = ifelse(is.na(rate), "", sprintf("%.0f", rate * 100)),
                color = I(txt_col)), size = 2.1) +
  scale_fill_gradient(low = "white", high = "#1f3a93", na.value = "grey90",
                      name = "Conv+Souh (%)",
                      labels = function(x) paste0(round(x * 100), "%")) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL,
       title = "Taux de soutien (conv+souh) par mesure et par coalition",
       subtitle = "Moyenne pondérée, NSP exclus du dénominateur") +
  theme_bw(base_size = 8.5) +
  theme(
    axis.text.x         = element_text(angle = 35, hjust = 0, size = 7.5, face = face_x_h),
    axis.text.y         = element_text(size = 7.5),
    legend.position     = "bottom",
    panel.grid          = element_blank(),
    plot.title          = element_text(size = 9.5, face = "bold", hjust = 0),
    plot.subtitle       = element_text(size = 7.5, color = "grey40", hjust = 0),
    plot.title.position = "plot",
    plot.margin         = margin(t = 5, r = 60, b = 5, l = 5)
  )

ggsave("../figures/coalition_support_heatmap.pdf", p_coal_supp,
       width = 7, height = 9, device = cairo_pdf)
cat("→ ../figures/coalition_support_heatmap.pdf generated.\n")
