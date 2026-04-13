##### Budget 26 analyses #####
# Results documented below from analyse_budget.R (run April 2026)

# Key findings:
# 1. SUPPORT BY VOTE (conv+souh, order: Non-voter/PNR/Other | Center-right | Far right | Left):
#    - Most popular overall: eliminer_doublons (92%), tva_luxe (81%), retablir_isf (77%)
#    - Anti-immigration: retirer_aides_etrangers (77|75|98|41%), supprimer_ame (72|68|99|36%)
#    - Broad left-right consensus: tva_luxe (80|77|78|81%), retablir_isf (83|64|74|78%)
#    - Divisive: reduire_depenses_militaires (47|19|38|45%), augmenter_age_retraite_65 (31|65|38|27%)
#
# 2. KEY REGRESSION RESULTS (support ~ vote + income + age + gender + education):
#    - Left voters: -14.7pp on geler_depenses, -36pp on supprimer_ame, -57pp on retirer_aides_etrangers
#    - Center-right: +34pp on augmenter_age_retraite_65, -16pp on reduire_depenses_militaires
#    - Youth (18-24): -17pp on eliminer_doublons, -16pp on supprimer_ame, -17pp on geler_depenses
#    - Income: mostly non-significant
#
# 3. TOP CORRELATIONS BETWEEN MEASURES:
#    - retirer_aides_etrangers <-> supprimer_ame: r=0.716 (anti-immigration bloc)
#    - augmenter_tva_1pt <-> augmenter_csg_1pt: r=0.534 (broad tax bloc)
#    - augmenter_taxe_revenus_capital <-> augmenter_impot_revenu_aises: r=0.521 (wealth tax bloc)
#    - reduire_pensions_retraite <-> supprimer_abattement_ir_retraites: r=0.480 (pension reform bloc)
#    - retablir_isf <-> augmenter_impot_heritages_eleves: r=0.441 (wealth tax bloc)
#
# 4. K-MEANS CLUSTERING (k=4):
#    - Cluster 1 (n=166, 40% center-right, 32% far right): "Conservative liberal"
#      strong anti-immigration, spending cuts, weak wealth taxes
#    - Cluster 2 (n=96, 29% center-right, 28% left): "Pragmatic"
#      accepts almost everything including broad tax rises (TVA 74%, CSG 71%)
#    - Cluster 3 (n=131, 57% left): "Progressive"
#      strong wealth taxes (ISF 93%), low anti-immigration support (AME 11%)
#    - Cluster 4 (n=209, 35% far right, 27% non-voter): "Populist fiscalizer"
#      hybrid: supports both anti-immigration (AME 92%) AND wealth taxes (ISF 94%, TVA luxe 87%)
#
# 5. EFFECT_PROGRAM (overall score, -2 to +2):
#    - Most positive: reduire_depenses_fonctionnement +1.19, peines_planchers +1.12, appliquer_oqtf +1.11
#    - Universal (all groups): education_sante +1.01 (NV=0.97, CR=0.90, FR=0.97, L=1.22)
#    - Negative: augmenter_allocs_familiales -0.31, retraite_65_ans -0.50
#    - Divisive: appliquer_oqtf (FR=1.83 vs L=0.44), retraite_65_ans (CR=+0.46 vs NV=-0.90/FR=-0.69/L=-0.66)
#
# 6. MAJORITY PACKAGES (see paquets_majoritaires.R for full analysis):
#
#    TWO DEFINITIONS TESTED:
#    (A) conv+souh only (xPNR): "convenable ou souhaitable" = support; NSP = excluded
#    (B) supp+conv+souh (xPNR): "supportable ou convenable ou souhaitable" = support; NSP = excluded
#
#    === DEFINITION A: conv+souh (xPNR) ===
#    12 frequent measures (individual support > 50%). Algorithm: exhaustive 2^12 = 4096 search.
#    Maximum package size: 3 measures.
#    6 optimal 3-measure packages found:
#      58.8% | 17.0 Mds€ | doublons + retirer_aides_etrangers + supprimer_ame  ← highest joint support
#      57.9% | 23.5 Mds€ | doublons + retablir_isf + tva_luxe
#      51.7% | 37.1 Mds€ | doublons + geler_depenses + retirer_aides_etrangers
#      51.0% | 19.2 Mds€ | doublons + retirer_aides_etrangers + tva_luxe
#      50.6% | 29.4 Mds€ | geler_depenses + retirer_aides_etrangers + supprimer_ame
#      50.1% | 11.5 Mds€ | retirer_aides_etrangers + supprimer_ame + tva_luxe
#    Vote profile of the highest joint-support package (doublons + aides_etrangers + AME, 58.8%):
#      NV 27.7%, Center-right 24.8%, Far right 34.9%, Left 12.7%
#      vs overall: NV 31%, Center-right 22.1%, Far right 23.9%, Left 23%
#      → strongly right-leaning majority (FR +11pp, Left -10pp)
#
#    === DEFINITION B: supp+conv+souh (xPNR) ===
#    23 frequent measures (individual support > 50%). Algorithm: Apriori level-by-level,
#    2912 evaluations (out of 2^23 = 8,388,608 possible subsets).
#    Maximum package size: 6 measures. 15 optimal 6-measure packages found.
#    Best (highest joint support):
#      54.8% | 52.9 Mds€ | doublons + geler_depenses + aides_etrangers + AME + ISF + tva_luxe
#    All 15 packages of size 6 are listed in paquets_majoritaires.R output.
#    Vote profile of the best package (54.8%):
#      NV 27.4%, Center-right 26.3%, Far right 28.4%, Left 17.9%
#      vs overall: NV 31%, Center-right 22.1%, Far right 23.9%, Left 23%
#      → moderately right-leaning (FR +4.5pp, CR +4.2pp, Left -5.1pp)
#    No 7-measure package reaches 50% joint support.
#
#    KEY FINDING: With definition B, a transpartisan 6-measure package exists (54.8%),
#    combining measures from left (ISF, TVA luxe) and right (aides étrangers, AME, gel dépenses).
#    Its vote profile is more balanced than definition A's 3-measure packages.


##### GCS #####
summary(lm(gcs_support == "Yes" ~ variant_gcs, data = e, weights = weight)) # -.10**
summary(lm(gcs_support == "Yes" ~ gcs_understood, data = e, weights = weight)) # -.02
summary(lm(gcs_support == "Yes" ~ (climate_belief >= 0), data = e, weights = weight)) # .29***
summary(lm(gcs_support == "Yes" ~ (climate_belief > 0), data = e, weights = weight)) # .23***


##### Wealth tax #####
summary(lm(wealth_tax_support ~ variant_wealth_tax, data = e, weights = weight)) # .09* / .05


##### Sustainable future #####
summary(lm(sustainable_future ~ variant_sustainable_future, data = e, weights = weight)) # -.01 


#### Group defended #####
summary(lm((group_defended > 0) ~ variant_group_defended, data = e, weights = weight)) # .12***
summary(lm((group_defended == 0) ~ variant_group_defended, data = e, weights = weight)) # -.13***
summary(lm((group_defended == 1) ~ variant_group_defended, data = e, weights = weight)) # .07**


##### WTP #####
summary(lm(wtp ~ factor(variant_wtp), data = e, weights = weight)) # 1%: .09.; 10%: -15**


##### Representativeness #####
countries <- "FR"
representativeness_table(df = e)
