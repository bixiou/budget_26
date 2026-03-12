# One-off: create data_ext/budget_policies.xlsx from the JS budget_policies object.
# Run from code_budget: source("create_budget_policies_xlsx.R")

if (basename(getwd()) != "code_budget") setwd("code_budget")
dir.create("../data_ext", showWarnings = FALSE)

# Policy id, amount (G€), label (French). Order as in JS (1..30, with 16/17 order in list).
policies <- data.frame(
  id = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 17L, 18L, 16L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L),
  amount = c(9, 21.4, 12, 1.5, 4.3, 5.3, 6.7, 1.3, 3.5, 3.4, 1.9, 3.4, 4.8, 4.4, 15, 14.2, 2.2, 4.2, 6, 14.6, 11.9, 11.4, 18, 4.4, 9.3, 11, 9, 3.5, 1.2, 4.7),
  label = c(
    "Éliminer les doublons entre échelons territoriaux",
    "Geler pendant deux ans les dépenses de l'État et des collectivités territoriales",
    "Réduire les dépenses militaires (annuler la hausse prévue)",
    "Diminuer le crédit d'impôt recherche pour les grandes entreprises",
    "Réduire les aides aux entreprises pour les contrats d'apprentissage",
    "Réduire le remboursement de certains soins, des indemnités d'arrêt maladie, et du transport des patients",
    "Retirer les aides sociales aux étrangers non-européens (RSA, allocations familiales, APL, minimum vieillesse)",
    "Supprimer l'Aide Médicale de l'État, qui couvre les soins urgents des étrangers en situation irrégulière",
    "Diminuer d'un tiers des subventions à l'enseignement privé",
    "Réduire les dépenses éducatives en proportion de la baisse démographique",
    "Augmenter de 4 mois de la durée de travail pour avoir droit au chômage",
    "Geler les aides sociales (hors APL), diminuer les Aides Pour le Logement de 5% et les restreindre aux ménages modestes",
    "Réduire d'un demi-point la revalorisation des pensions de retraites pendant 4 ans",
    "Supprimer l'abattement de 10% sur les retraites pour le calcul de l'impôt sur le revenu",
    "Augmenter l'âge légal de départ à la retraite de 64 à 65 ans sauf pour les carrières longues ou avec pénibilité",
    "Supprimer les exonérations de taxes (sur les carburants et autres) dans certains secteurs (maritime, aviation, routiers, agriculture) et augmenter la taxe sur les billets d'avion",
    "Soumettre les intérêts du Livret A et du LDDS à l'impôt sur le revenu et aux prélèvements sociaux",
    "Aligner le taux de TVA dans la restauration (10%) sur le taux normal (20%)",
    "Augmenter d'un point le taux normal de TVA (de 20% à 21%)",
    "Augmenter d'un point le taux de CSG (impôt qui s'applique à presque tous les revenus)",
    "Augmenter d'un point les taux de l'impôt sur le revenu et abaisser les seuils pour élargir le nombre de contribuables",
    "Augmenter les cotisations sur les salaires moyens (réduire les allègements de charges du CICE)",
    "Supprimer les avantages fiscaux aux compléments de salaire (intéressement, participation, tickets-restaurants, etc.)",
    "Augmenter le taux d'imposition des sociétés de 25% à 33,5%",
    "Restaurer la taxe d'habitation sur les 20 % les plus aisés",
    "Rétablir une version renforcée de l'impôt sur la fortune des millionnaires (avec un taux de 2% pour les milliardaires)",
    "Augmenter l'impôt sur l'héritage pour les 10% d'héritages les plus élevés",
    "Créer un taux de TVA à 25 % pour les biens de luxe (montres, yachts, voitures de sport...)",
    "Augmenter la taxe sur les revenus du capital de 30% à 33%",
    "Augmenter l'impôt sur les revenus des plus aisés en rajoutant des tranches, avec un taux maximal à 65 %"
  ),
  stringsAsFactors = FALSE
)

# variable_name: budget_ + short snake_case summary of label
variable_name <- c(
  "budget_eliminer_doublons_territoriaux",
  "budget_geler_depenses_etat_collectivites_2ans",
  "budget_reduire_depenses_militaires",
  "budget_diminuer_credit_impot_recherche_grandes_entreprises",
  "budget_reduire_aides_apprentissage",
  "budget_reduire_remboursement_soins_arret_maladie_transport",
  "budget_retirer_aides_sociales_etrangers_non_europeens",
  "budget_supprimer_ame",
  "budget_diminuer_subventions_enseignement_prive",
  "budget_reduire_depenses_educatives_baisse_demographique",
  "budget_augmenter_duree_travail_droit_chomage",
  "budget_geler_aides_sociales_restreindre_apl",
  "budget_reduire_revalorisation_pensions_4ans",
  "budget_supprimer_abattement_10_retraites_impot",
  "budget_augmenter_age_retraite_65",
  "budget_supprimer_exonerations_taxes_carburants_avion",
  "budget_soumettre_livret_a_ldds_impot",
  "budget_aligner_tva_restauration_20",
  "budget_augmenter_tva_1_point",
  "budget_augmenter_csg_1_point",
  "budget_augmenter_impot_revenu_abaisser_seuils",
  "budget_augmenter_cotisations_reduire_cice",
  "budget_supprimer_avantages_fiscaux_complements_salaire",
  "budget_augmenter_impot_societes_33_5",
  "budget_restaurer_taxe_habitation_20_aises",
  "budget_retablir_isf_millionnaires",
  "budget_augmenter_impot_heritage_10_hauts",
  "budget_tva_25_luxe",
  "budget_augmenter_taxe_revenus_capital_33",
  "budget_augmenter_impot_revenus_aises_tranches_65"
)
policies$variable_name <- variable_name

# Reorder so id is 1..30 (column order in survey is budget_progressistes_1, _2, ... _30)
policies <- policies[order(policies$id), ]
rownames(policies) <- NULL

if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Install openxlsx: install.packages('openxlsx')")
openxlsx::write.xlsx(policies, "../data_ext/budget_policies.xlsx", sheetName = "budget_policies")
message("Written ../data_ext/budget_policies.xlsx with ", nrow(policies), " rows.")
