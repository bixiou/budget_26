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


e$no_weight <- 1

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
quotas$FR <- c(quotas$default, "vote_factor")
representativeness_table(df = e, omit = c("Not 25-64", "Employment_18_64: Employed", "Employment_18_64: 65+", "Urban: FALSE"))


##### Budget; Claude Code #####
# Create numeric matrix of budget support
budget_mat <- as.data.frame(sapply(variables_budget, function(v) e[[v]]))
budget_mat[budget_mat == -.1] <- NA


# Also create binary: accept = Souhaitable or Convenable
budget_accept <- sapply(variables_budget, function(v) {
  ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1,
         ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0, NA))
})
budget_accept <- as.data.frame(budget_accept)

##### 1. Weighted means of budget support by sociodem #####
cat("\n=== Weighted mean support (conv+souh) by vote_factor ===\n")
for (v in variables_budget) {
  means <- tapply(budget_accept[[v]] * e$no_weight, e$vote_factor, function(x) sum(x, na.rm=TRUE)) /
    tapply(!is.na(budget_accept[[v]]) * e$no_weight, e$vote_factor, function(x) sum(x, na.rm=TRUE))
  cat(v, ":", round(means, 2), "\n")
}

cat("\n=== Regressions: support ~ vote + income + age + gender + education ===\n")
results <- list()
for (v in variables_budget) {
  df_reg <- data.frame(
    y = budget_accept[[v]],
    vote = e$vote_factor,
    income = e$income_quartile,
    age = e$age_factor,
    gender = e$man,
    education = e$education,
    no_weight = e$no_weight
  )
  df_reg <- df_reg[!is.na(df_reg$y), ]
  tryCatch({
    mod <- lm(y ~ vote + income + age + gender + education, data = df_reg, weights = no_weight)
    s <- summary(mod)$coefficients
    results[[v]] <- s
    cat("\n---", v, "---\n")
    print(round(s, 3))
  }, error = function(err) cat("Error for", v, ":", err$message, "\n"))
}

##### 2. Correlation matrix of budget support #####
cat("\n=== Correlation matrix of budget policy support ===\n")
cor_mat <- cor(budget_accept, use = "pairwise.complete.obs")
# Show only top correlated pairs
cor_pairs <- which(upper.tri(cor_mat), arr.ind = TRUE)
cor_vals <- cor_mat[cor_pairs]
top_pos <- order(cor_vals, decreasing = TRUE)[1:20]
cat("\nTop 20 positive correlations:\n")
for (i in top_pos) {
  r <- cor_pairs[i, 1]; c <- cor_pairs[i, 2]
  cat(sprintf("  %s <-> %s: %.3f\n",
              sub("budget_", "", rownames(cor_mat)[r]),
              sub("budget_", "", colnames(cor_mat)[c]),
              cor_vals[i]))
}
# Visualize with corrplot (reordered by hierarchical clustering of correlations)
pdf("../figures/budget_correlations.pdf", width = 14, height = 14)
corrplot(cor_mat, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.55, tl.col = "black", addCoef.col = "black", number.cex = 0.45,
         diag = FALSE, col = colorRampPalette(c("#c0392b", "white", "#2c3e50"))(200))
dev.off()
cat("→ ../figures/budget_correlations.pdf\n")

##### 2b. Sociodemographic determinants: significant coefs and variance decomposition (lmg) #####
cat("\n=== Sociodemographic determinants: signif. coefs + lmg variance shares ===\n")
ep_score <- function(x) {
  case_when(
    x == "Beaucoup plus favorable" ~ 2,
    x == "Plus favorable" ~ 1,
    x == "Ne changerait rien" ~ 0,
    x == "Moins favorable" ~ -1,
    x == "Beaucoup moins favorable" ~ -2,
    TRUE ~ NA_real_
  )
}

# determinants <- c("vote_factor", "income_factor", "age_factor", "man", "education_original", "urbanity_factor")
# determinants <- c("vote_original", "vote_factor", "man", "age_factor", "income_factor", "education_original", "urbanity_factor", "vote_factor:age_factor", "income_factor:age_factor", "man:age_factor") 
determinants <- c("no.na(vote_agg)",  "man", "age_factor", "income_factor", "urbanity_factor", "as.factor(region)", "as.factor(education)", "no.na(wealth_quartile_5)", "employment_agg", "Nb_children__14", "hh_size", "voted")
# determinants <- c("vote_original")
# determinants <- determinants[determinants %in% names(e)]

fit_decomp <- function(y, df = e, det = determinants) {
  # if (!grepl("sum_", y)) y <- paste(y, "> 0") # Explain 10-11% of variance instead of 13%
  mod <- tryCatch(lm(as.formula(paste("as.numeric(", y, ") ~", paste(det, collapse = '+'))), data = df, weights = no_weight), error = function(e) NULL)
  if (is.null(mod)) return(NULL)
  s <- summary(mod)$coefficients
  # sig <- sapply(det, function(v) sum(grepl(paste0("^", v), rownames(s)) & s[, 4] < 0.05))
  sig <- s[, 4] < 0.05
  lmg <- if (length(det) == 1) {
    setNames(summary(mod)$r.squared, det)
  } else {
    tryCatch(calc.relimp(mod, type = "lmg", rela = FALSE, rank = FALSE)@lmg,
             error = function(e) setNames(rep(NA_real_, length(det)), det))
  }
  list(sig = sig, lmg = lmg, R2 = summary(mod)$r.squared)
}

for (det in list(determinants, "no.na(vote_agg)", "vote_original", "education_original", determinants[2:11])) {
  temp <- fit_decomp("sum_convenable", det = det)
  total_sig <- setNames(integer(length(temp$sig)), names(temp$sig))
  total_lmg <- setNames(numeric(length(temp$lmg)), names(temp$lmg))
  attitudes <- c(variables_budget, variables_effect_program, "sum_convenable", "sum_souhaitable")
  for (v in attitudes) {
    res <- fit_decomp(v, det = det)
    if (!is.null(res)) {
      total_sig <- total_sig + (res$sig > 0)
      total_lmg <- total_lmg + res$lmg
    }
  }
  print(det)
  print(sort(total_sig)) # Number of significant coefs
  print(sort(round(100 * total_lmg / length(attitudes), 2))) # R^2 explained
  print(round(sum(100 * total_lmg / length(attitudes)), 2)) # Total R^2
}


##### 2c. LMG and isolated-R² figures #####
{
  det_labels <- c(
    "no.na(vote_agg)"          = "Bloc politique",
    "man"                      = "Genre",
    "age_factor"               = "Âge",
    "income_factor"            = "Niveau de vie",
    "urbanity_factor"          = "Urbanité",
    "as.factor(region)"        = "Région",
    "as.factor(education)"     = "Diplôme",
    "no.na(wealth_quartile_5)" = "Patrimoine",
    "employment_agg"           = "Statut d'emploi",
    "Nb_children__14"          = "Nb enfants < 14 ans",
    "hh_size"                  = "Taille du ménage",
    "voted"                    = "A voté (Européennes)"
  )

  # Collect average LMG and average isolated R² across a set of attitude variables.
  # Returns list(avg_lmg, avg_iso) as named numeric vectors (% of variance, names = determinants).
  collect_lmg_iso <- function(vars_set) {
    lmg_mat <- matrix(NA_real_, nrow = length(determinants), ncol = length(vars_set),
                      dimnames = list(determinants, vars_set))
    iso_mat  <- lmg_mat

    for (v in vars_set) {
      # Full LMG (joint regression on all determinants)
      res_full <- fit_decomp(v, det = determinants)
      if (!is.null(res_full)) {
        shared <- intersect(determinants, names(res_full$lmg))
        lmg_mat[shared, v] <- res_full$lmg[shared]
      }
      # Isolated R² (one regressor at a time)
      for (d in determinants) {
        res_iso <- fit_decomp(v, det = d)
        if (!is.null(res_iso)) iso_mat[d, v] <- res_iso$R2
      }
    }
    list(
      avg_lmg = rowMeans(lmg_mat, na.rm = TRUE) * 100,
      avg_iso = rowMeans(iso_mat,  na.rm = TRUE) * 100
    )
  }

  # Build and save a horizontal bar figure.
  bar_fig <- function(df_vals, title, subtitle, xlab, tag) {
    df <- data.frame(
      label = factor(det_labels[determinants],
                     levels = det_labels[determinants[order(df_vals)]]),
      value = df_vals,
      stringsAsFactors = FALSE
    )
    p <- ggplot(df, aes(x = value, y = label)) +
      geom_col(fill = "#2c6fad", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", value)), hjust = -0.1, size = 2.8) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                         labels = function(x) paste0(x, "%")) +
      labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
      theme_bw(base_size = 9) +
      theme(panel.grid.major.y  = element_blank(),
            panel.grid.minor    = element_blank(),
            plot.title          = element_text(size = 9.5, face = "bold", hjust = 0),
            plot.subtitle       = element_text(size = 7.5, color = "black", hjust = 0),
            plot.title.position = "plot",
            plot.margin         = margin(t = 5, r = 20, b = 5, l = 5))
    ggsave(sprintf("../figures/%s.pdf", tag), p, width = 6, height = 4.5, device = cairo_pdf)
    cat("→ ../figures/", tag, ".pdf\n", sep = "")
  }

  make_lmg_figs <- function(vars_set, tag, title_base) {
    cat("\nComputing LMG/R² for", tag, "(", length(vars_set), "variables) ...\n")
    res <- collect_lmg_iso(vars_set)
    bar_fig(res$avg_lmg, tag = paste0("lmg_", tag),
            title    = paste0(title_base, " — LMG"),
            subtitle = sprintf("R² total moyen : %.1f%%", sum(res$avg_lmg)),
            xlab     = "Part moyenne de variance expliquée (LMG, %)")
    bar_fig(res$avg_iso, tag = paste0("r2_iso_", tag),
            title    = paste0(title_base, " — R² isolé"),
            subtitle = "Régression séparée avec un seul régresseur à la fois",
            xlab     = "R² moyen (%)")
  }

  # --- Set 1: all attitudes ---
  attitudes <- c(variables_budget, variables_effect_program, "sum_convenable", "sum_souhaitable")
  make_lmg_figs(attitudes, "attitudes", "Déterminants des attitudes")

  # --- Set 2: effect_program only ---
  make_lmg_figs(variables_effect_program, "effect_program",
                "Déterminants du programme électoral")

  # --- Set 3: top 10 most politically polarized attitudes ---
  vote_r2 <- sapply(attitudes, function(v) {
    res <- fit_decomp(v, det = "no.na(vote_agg)")
    if (is.null(res)) 0 else res$R2
  })
  top8_polarized <- names(sort(vote_r2, decreasing = TRUE))[1:8]
  cat("\nTop 10 attitudes les plus polarisées politiquement :\n")
  print(round(sort(vote_r2, decreasing = TRUE)[1:8], 3))
  make_lmg_figs(top8_polarized, "attitudes_polarisees",
                "Déterminants des 10 attitudes les plus polarisées")
}


##### 3. Clustering of respondents #####
# Cluster 1: frugaux (20%), 2: nationalistes (49%), 3: progressistes (31%)
cat("\n=== K-means clustering of respondents (k selected by silhouette) ===\n")
# Cluster on the Likert scores (Inacceptable=-1, Supportable=0, Convenable=1, Souhaitable=2).
# Impute NAs (Ne sais pas) with the column mean so all respondents are kept.
budget_mat_imputed <- budget_mat
for (v in names(budget_mat_imputed)) {
  col_mean <- mean(budget_mat_imputed[[v]], na.rm = TRUE)
  budget_mat_imputed[[v]][is.na(budget_mat_imputed[[v]])] <- col_mean
}

# Pick k by maximizing mean silhouette width over k = 2..10.
d_eucl <- dist(budget_mat_imputed)
k_range <- 2:10
sil_width <- sapply(k_range, function(k) {
  set.seed(42)
  km_k <- kmeans(budget_mat_imputed, centers = k, nstart = 20)
  mean(silhouette(km_k$cluster, d_eucl)[, 3])
})
names(sil_width) <- k_range
cat("Mean silhouette width by k:\n")
print(round(sil_width, 3))
k_opt <- as.integer(names(sil_width)[which.max(sil_width)])
cat(sprintf("Optimal k = %d (silhouette = %.3f)\n", k_opt, max(sil_width)))

set.seed(42)
km <- kmeans(budget_mat_imputed, centers = k_opt, nstart = 20)
e$cluster <- factor(km$cluster)
label(e$cluster) <- "cluster: k-means cluster on budget Likert scores (NAs imputed by column mean; k by silhouette)."
cat("Cluster sizes:", table(km$cluster), "\n")

# Vote profile by cluster (within a cluster, share of each vote group)
cat("\nVote profile by cluster:\n")
for (cl in seq_len(k_opt)) {
  cat(sprintf("Cluster %d (n=%d): ", cl, sum(km$cluster == cl, na.rm=TRUE)))
  vote_tbl <- prop.table(table(e$vote_factor[km$cluster == cl]))
  cat(paste(round(vote_tbl*100, 1), names(vote_tbl), sep="% ", collapse=", "), "\n")
}

# Cluster profile by vote (within a vote group, share in each cluster)
cat("\nCluster profile by vote:\n")
cluster_by_vote <- prop.table(table(e$vote_factor, km$cluster), margin = 1)
print(round(cluster_by_vote * 100, 1))

# Mean support (binary conv+souh) by cluster, on observed values only
cat("\nMean support by cluster:\n")
cluster_means <- aggregate(budget_accept, by = list(cluster = km$cluster), FUN = mean, na.rm = TRUE)
print(round(cluster_means, 2))

cat("\nMean sum_convenable (G€) by cluster:\n")
print(round(tapply(e$sum_convenable, km$cluster, mean, na.rm = TRUE), 2))

##### 3b. Alternative clustering: Ward hierarchical on an ordinal+NSP distance #####
# Cluster (n=40) of PNR, cluster of right (74%, incl. 7% très frugaux) and left (22%)
# Custom Gower-like distance per variable:
#   - both Likert (-1,0,1,2): |x - y| / 3 (normalized rank distance, uses ordering)
#   - exactly one NSP: 1 (NSP is maximally different from any opinion)
#   - both NSP: 0
# Averaged across variables.
cat("\n=== Hierarchical Ward clustering (ordinal+NSP distance, k by silhouette) ===\n")
ord_nsp_dist <- function(mat) {
  n <- nrow(mat)
  p <- ncol(mat)
  d <- matrix(0, n, n)
  for (v in seq_len(p)) {
    x <- mat[, v]
    nsp <- is.na(x)
    x0 <- x; x0[nsp] <- 0
    d_ord <- abs(outer(x0, x0, "-")) / 3
    nsp_xor <- outer(nsp, nsp, FUN = function(a, b) xor(a, b))
    nsp_both <- outer(nsp, nsp, FUN = "&")
    dv <- d_ord
    dv[nsp_xor] <- 1
    dv[nsp_both] <- 0
    d <- d + dv
  }
  d / p
}
d_ord <- as.dist(ord_nsp_dist(as.matrix(budget_mat)))

# Optimize k by silhouette width over k = 2..10
k_range_h <- 2:10
sil_width_h <- sapply(k_range_h, function(k) {
  hc_k <- hclust(d_ord, method = "ward.D2")
  cl_k <- cutree(hc_k, k = k)
  mean(silhouette(cl_k, d_ord)[, 3])
})
names(sil_width_h) <- k_range_h
cat("Mean silhouette width by k:\n")
print(round(sil_width_h, 3))
k_opt_h <- as.integer(names(sil_width_h)[which.max(sil_width_h)])
cat(sprintf("Optimal k = %d (silhouette = %.3f)\n", k_opt_h, max(sil_width_h)))

k_opt_h <- 4
hc <- hclust(d_ord, method = "ward.D2")
cluster_h_int <- cutree(hc, k = k_opt_h)
e$cluster_h <- factor(cluster_h_int)
label(e$cluster_h) <- "cluster_h: Ward hierarchical cluster on ordinal+NSP distance (|rank diff|/3 Likert; NSP vs opinion = 1)."
cat("Cluster sizes:", table(cluster_h_int), "\n")

cat("\nVote profile by hierarchical cluster:\n")
for (cl in seq_len(k_opt_h)) {
  cat(sprintf("Cluster %d (n=%d): ", cl, sum(cluster_h_int == cl)))
  vote_tbl <- prop.table(table(e$vote_factor[cluster_h_int == cl]))
  cat(paste(round(vote_tbl*100, 1), names(vote_tbl), sep="% ", collapse=", "), "\n")
}

cat("\nHierarchical cluster profile by vote:\n")
cluster_by_vote_h <- prop.table(table(e$vote_factor, cluster_h_int), margin = 1)
print(round(cluster_by_vote_h * 100, 1))

cat("\nNSP share by hierarchical cluster (per measure):\n")
nsp_mat <- as.data.frame(lapply(budget_mat, function(x) as.integer(is.na(x))))
nsp_share <- aggregate(nsp_mat, by = list(cluster = cluster_h_int), FUN = mean)
print(round(nsp_share, 2))

cat("\nMean support (conv+souh, observed only) by hierarchical cluster:\n")
cluster_means_h <- aggregate(budget_accept, by = list(cluster = cluster_h_int), FUN = mean, na.rm = TRUE)
print(round(cluster_means_h, 2))

cat("\nMean sum_convenable (G€) by hierarchical cluster:\n")
print(round(tapply(e$sum_convenable, cluster_h_int, mean, na.rm = TRUE), 2))

##### 3c. Clustering of measures (policy clustering) #####
# Cluster 1: right-wing, 64G€, 2: unpopular, 121G€, 3: left-wing, 39G€
# Cluster the budget measures themselves, using 1 - pairwise correlation of support as distance.
# k is chosen by maximizing mean silhouette width over k = 2..8 on Ward hierarchical clustering.
cat("\n=== Clustering of budget measures (Ward on 1 - cor) ===\n")
d_meas <- as.dist(1 - cor_mat)
hc_meas <- hclust(d_meas, method = "ward.D2")
k_range_m <- 2:8
sil_meas <- sapply(k_range_m, function(k) {
  cl <- cutree(hc_meas, k = k)
  mean(silhouette(cl, d_meas)[, 3])
})
names(sil_meas) <- k_range_m
cat("Mean silhouette width by k (measures):\n")
print(round(sil_meas, 3))
k_opt_m <- as.integer(names(sil_meas)[which.max(sil_meas)])
cat(sprintf("Optimal k for measures = %d (silhouette = %.3f)\n", k_opt_m, max(sil_meas)))

k_opt_m <- 3
meas_cluster <- cutree(hc_meas, k = k_opt_m)
cat("\nMeasure membership per cluster:\n")
for (cl in seq_len(k_opt_m)) {
  members <- names(meas_cluster)[meas_cluster == cl]
  cat(sprintf("Cluster %d (%d measures): %s\n", cl, length(members),
              paste(sub("budget_", "", members), collapse = ", ")))
}

# Total budget savings (Mds€) per measure cluster, from budget_policies$amount.
cat("\nTotal budget savings (Mds€) per measure cluster:\n")
savings_by_cluster <- sapply(seq_len(k_opt_m), function(cl) {
  members <- names(meas_cluster)[meas_cluster == cl]
  sum(budget_policies$amount[budget_policies$variable_name %in% members], na.rm = TRUE)
})
names(savings_by_cluster) <- paste0("meas_cl", seq_len(k_opt_m))
print(round(savings_by_cluster, 1))
cat(sprintf("Grand total across clusters: %.1f Mds€\n", sum(savings_by_cluster)))

# Per-respondent mean support on each measure cluster (using binary accept).
cluster_support <- sapply(seq_len(k_opt_m), function(cl) {
  members <- names(meas_cluster)[meas_cluster == cl]
  rowMeans(budget_accept[, members, drop = FALSE], na.rm = TRUE)
})
colnames(cluster_support) <- paste0("meas_cl", seq_len(k_opt_m))

# Mean support per measure cluster, by vote_factor (weighted).
cat("\nMean support on measure clusters, by vote_factor:\n")
supp_by_vote <- sapply(seq_len(k_opt_m), function(cl) {
  s <- cluster_support[, cl]
  tapply(s * e$no_weight, e$vote_factor, function(x) sum(x, na.rm = TRUE)) /
    tapply(!is.na(s) * e$no_weight, e$vote_factor, function(x) sum(x, na.rm = TRUE))
})
colnames(supp_by_vote) <- paste0("meas_cl", seq_len(k_opt_m))
print(round(supp_by_vote, 2))

# Mean support per measure cluster, by respondent k-means cluster (section 3).
cat("\nMean support on measure clusters, by respondent k-means cluster:\n")
supp_by_rcl <- sapply(seq_len(k_opt_m), function(cl) {
  tapply(cluster_support[, cl], e$cluster, mean, na.rm = TRUE)
})
colnames(supp_by_rcl) <- paste0("meas_cl", seq_len(k_opt_m))
print(round(supp_by_rcl, 2))

# Mean support per measure cluster, by respondent hierarchical cluster (section 3b).
cat("\nMean support on measure clusters, by respondent hierarchical cluster:\n")
supp_by_rcl_h <- sapply(seq_len(k_opt_m), function(cl) {
  tapply(cluster_support[, cl], e$cluster_h, mean, na.rm = TRUE)
})
colnames(supp_by_rcl_h) <- paste0("meas_cl", seq_len(k_opt_m))
print(round(supp_by_rcl_h, 2))

# Joint majority per measure cluster: share of respondents supporting ALL members.
# NSP counts as support: a respondent is "supporting the whole cluster" iff no member
# is marked Inacceptable/Supportable (budget_accept == 0). Weighted.
cat("\nJoint majority per measure cluster (share supporting ALL members; NSP = support, weighted):\n")
joint_overall <- numeric(k_opt_m)
joint_by_vote <- matrix(NA_real_, nlevels(e$vote_factor), k_opt_m,
                        dimnames = list(levels(e$vote_factor), paste0("meas_cl", seq_len(k_opt_m))))
for (cl in seq_len(k_opt_m)) {
  members <- names(meas_cluster)[meas_cluster == cl]
  sub <- budget_accept[, members, drop = FALSE]
  supp_all <- as.integer(rowSums(sub == 0L, na.rm = TRUE) == 0L)
  w <- e$no_weight
  joint_overall[cl] <- sum(supp_all * w) / sum(w)
  num <- tapply(supp_all * w, e$vote_factor, sum)
  den <- tapply(w, e$vote_factor, sum)
  joint_by_vote[names(num), cl] <- num / den
}
names(joint_overall) <- paste0("meas_cl", seq_len(k_opt_m))
cat("Overall:\n")
print(round(joint_overall, 3))
cat("By vote_factor:\n")
print(round(joint_by_vote, 3))

##### 4. Effect_program analyses #####
cat("\n=== Effect program: mean favorability by vote_factor ===\n")
# ep_score() is defined in section 2b.
for (v in variables_effect_program) {
  score <- ep_score(e[[v]])
  overall <- weighted.mean(score, e$no_weight, na.rm = TRUE)
  cat(v, ": overall =", round(overall, 3))
  means <- tapply(score * e$no_weight, e$vote_factor, function(x) sum(x, na.rm=TRUE)) /
    tapply(!is.na(score) * e$no_weight, e$vote_factor, function(x) sum(x, na.rm=TRUE))
  cat("  by vote:", round(means, 2), "\n")
}

##### 5. Majority packages summary #####
cat("\n=== Majority packages summary ===\n")
cat("Majority souhaitable (", length(budget_majorite_souhaitable), "measures):\n")
cat(paste(budget_majorite_souhaitable, collapse="\n"), "\n")

# Budget amounts for majority packages
bp <- budget_policies
if(!is.null(bp) && "variable_name" %in% names(bp)) {
  total_souhaitable <- sum(bp$amount[bp$variable_name %in% budget_majorite_souhaitable], na.rm=TRUE)
  total_convenable <- sum(bp$amount[bp$variable_name %in% budget_majorite_convenable], na.md=TRUE)
  cat("Total budget savings souhaitable:", total_souhaitable, "Mds€\n")
  cat("Total budget savings convenable:", total_convenable, "Mds€\n")
}

##### 6. Coalition packages matrix figure #####
# Requires paquets_majoritaires.R to have been run first (provides mat_SCS, THRESHOLD,
# amounts, vars, short, m, run_apriori, labels_budget_fr, coalition_defs,
# group_labels_fr, group_labels_short).
{
  # Coalition masks — keys match group_labels_fr; exclude pairs and two party coalitions
  vote_bloc_masks <- list(
    "Overall"      = rep(TRUE, nrow(e)),
    "Left"         = !is.na(e$vote_agg) & e$vote_agg == 0,
    "Center-right" = !is.na(e$vote_agg) & e$vote_agg == 1,
    "Far right"    = !is.na(e$vote_agg) & e$vote_agg == 2
  )
  party_coal_keys <- c("EELV_PS_centre", "PS_centre", "PS_centre_LR", "centre_LR_RN_Reconquete",
                        "LR_RN_Reconquete", "LFI", "EELV", "PS", "centre", "LR")
  coal_masks <- c(
    vote_bloc_masks,
    setNames(lapply(party_coal_keys, function(cn)
               !is.na(e$vote_original) & e$vote_original %in% coalition_defs[[cn]]),
             party_coal_keys)
  )

  # Best package per coalition: max savings among SCS ≥50% feasible packages
  best_by_savings <- function(mask) {
    wgt_g <- ifelse(mask, e$no_weight, 0)
    capture.output(feas <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = "")$all_feasible)
    if (!length(feas)) return(integer(0))
    feas[[which.max(sapply(feas, function(p) sum(amounts[p], na.rm = TRUE)))]]
  }

  cat("\n=== Coalition packages (SCS ≥50%, max savings) ===\n")
  pkg_res <- setNames(lapply(names(coal_masks), function(cn) {
    bp  <- best_by_savings(coal_masks[[cn]])
    amt <- sum(amounts[bp], na.rm = TRUE)
    cat(sprintf("  %-22s: %d mesures | %.1f Mds€\n", cn, length(bp), amt))
    list(vnames = vars[bp], savings = amt)
  }), names(coal_masks))

  # Filter to policies appearing in at least one package
  vars_f  <- intersect(vars, unique(unlist(lapply(pkg_res, `[[`, "vnames"))))
  short_f <- sub("budget_", "", vars_f)
  pol_amt <- amounts[match(vars_f, vars)]

  # Row ordering: ascending package count (most frequent at top)
  pkg_count <- sapply(vars_f, function(v) sum(sapply(pkg_res, function(p) v %in% p$vnames)))
  pol_lbl <- setNames(
    paste0(labels_budget_fr[short_f], " (",
           gsub("\\.", ",", sprintf("%.1f", pol_amt)), " Mds€)"),
    vars_f
  )
  pol_levs <- c(pol_lbl[vars_f[order(pkg_count)]],
                "Soutien au paquet d'Ensemble (%)", "Économies (Mds€)")

  # Column display labels and savings values
  col_levs    <- names(coal_masks)
  col_display <- group_labels_fr[col_levs]
  col_display["LR_RN_Reconquete"] <- "LR + Extrême-droite"
  col_display["centre_LR_RN_Reconquete"] <- "Centre + LR + Extrême-droite"
  savings_vec <- sapply(pkg_res, `[[`, "savings")

  # Support of Ensemble's package within each coalition (% SCS joint support)
  overall_pkg_idx <- match(pkg_res$Overall$vnames, vars)
  support_ens_pct <- sapply(names(coal_masks), function(cn) {
    mask  <- coal_masks[[cn]]
    wgt_g <- ifelse(mask, e$no_weight, 0)
    joint_support(overall_pkg_idx, mat_SCS, wgt_g) * 100
  })

  # Savings gradient colors: white (0 Mds€) → blue (120 Mds€), fixed scale
  blue_pal <- colorRampPalette(c("#ffffff", "#1f3a93"))
  sav_idx  <- pmin(100, pmax(1, round(savings_vec / 120 * 99) + 1))
  sav_hex  <- blue_pal(100)[sav_idx]
  sav_keys <- paste0("sav_", seq_along(savings_vec))

  # Support row gradient: white (0 %) → blue (100 %)
  sup_idx  <- pmin(100, pmax(1, round(support_ens_pct / 100 * 99) + 1))
  sup_hex  <- blue_pal(100)[sup_idx]
  sup_keys <- paste0("sup_", seq_along(support_ens_pct))

  # Build data frames
  df_tile6 <- expand.grid(policy = vars_f, coalition = col_levs, stringsAsFactors = FALSE)
  df_tile6$in_pkg   <- mapply(function(p, c) p %in% pkg_res[[c]]$vnames,
                               df_tile6$policy, df_tile6$coalition)
  df_tile6$pol_lbl  <- factor(pol_lbl[df_tile6$policy], levels = pol_levs)
  df_tile6$col_disp <- factor(col_display[df_tile6$coalition], levels = col_display)
  df_tile6$fill_cat <- ifelse(df_tile6$in_pkg, "in_pkg", "out_pkg")

  df_sav6 <- data.frame(
    col_disp = factor(col_display, levels = col_display),
    pol_lbl  = factor("Économies (Mds€)", levels = pol_levs),
    lbl_txt  = sprintf("%.1f", savings_vec),
    fill_cat = sav_keys,
    txt_col  = "black",
    stringsAsFactors = FALSE
  )

  df_sup6 <- data.frame(
    col_disp = factor(col_display, levels = col_display),
    pol_lbl  = factor("Soutien au paquet d'Ensemble (%)", levels = pol_levs),
    lbl_txt  = sprintf("%.0f", support_ens_pct),
    fill_cat = sup_keys,
    txt_col  = ifelse(support_ens_pct > 50, "white", "black"),
    stringsAsFactors = FALSE
  )

  fill_vals <- c(
    in_pkg  = "#2c6fad",
    out_pkg = "grey92",
    setNames(sav_hex, sav_keys),
    setNames(sup_hex, sup_keys)
  )

  # Bold "Ensemble" column header and the "Économies" row label
  face_x <- ifelse(col_display == col_display["Overall"], "bold", "plain")
  face_y <- ifelse(pol_levs %in% c("Économies (Mds€)", "Soutien au paquet d'Ensemble (%)"),
                   "bold", "plain")

  p_coal_matrix <- ggplot() +
    geom_tile(data = rbind(df_tile6[, c("col_disp","pol_lbl","fill_cat")],
                            df_sav6[,  c("col_disp","pol_lbl","fill_cat")],
                            df_sup6[,  c("col_disp","pol_lbl","fill_cat")]),
              aes(x = col_disp, y = pol_lbl, fill = fill_cat),
              color = "white", linewidth = 0.35, width = 0.92) +
    geom_text(data = rbind(df_sav6, df_sup6),
              aes(x = col_disp, y = pol_lbl, label = lbl_txt, color = I(txt_col)),
              size = 2.3, fontface = "bold") +
    geom_hline(yintercept = length(pol_levs) - 1.5, color = "grey45", linewidth = 0.5) +
    scale_fill_manual(
      values = fill_vals,
      breaks = c("in_pkg", "out_pkg"),
      labels = c(in_pkg = "Dans le paquet", out_pkg = "Hors du paquet"),
      name   = NULL
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         # title = "Mesures dans le paquet majoritaire à plus grande économie, par coalition",
         # subtitle = "Soutien conjoint ≥50% (supp+conv+souh, NSP=soutien), économies maximisées (Mds€)"
         ) +
    theme_bw(base_size = 8.5) +
    theme(
      axis.text.x          = element_text(angle = 35, hjust = 0, size = 7.5, face = face_x),
      axis.text.y          = element_text(size = 7.5, face = face_y),
      legend.position      = "bottom",
      legend.text          = element_text(size = 8),
      panel.grid           = element_blank(),
      plot.title.position  = "plot",
      plot.caption.position = "plot",
      plot.title           = element_text(size = 9.5, face = "bold", hjust = 0),
      plot.subtitle        = element_text(size = 7.5, color = "black", hjust = 0),
      plot.caption         = element_text(size = 6.5, color = "black", hjust = 0),
      plot.margin          = margin(t = 5, r = 60, b = 5, l = 5)
    )

  ggsave("../figures/coalition_packages_matrix.pdf", p_coal_matrix,
         width = 6.5, height = 6.5, device = cairo_pdf)
  cat("→ ../figures/coalition_packages_matrix.pdf\n")
}

##### 6b. Coalition support heatmap: Conv+Souh rate per measure per coalition #####
{
  h_defs <- list(
    "Overall"                 = NULL,
    "Left"                    = NULL,
    "Center-right"            = NULL,
    "Far right"               = NULL,
    "EELV_PS_centre"          = NULL,
    "PS_centre"               = NULL,
    "PS_centre_LR"            = NULL,
    "centre_LR_RN_Reconquete" = NULL,
    "LR_RN_Reconquete"        = NULL,
    "LFI"                     = NULL,
    "EELV"                    = NULL,
    "PS"                      = NULL,
    "centre"                  = NULL,
    "LR"                      = NULL
  )
  h_lbl <- c(
    "Overall" = "Ensemble", "Left" = "Gauche", "Center-right" = "Centre + LR",
    "Far right" = "Extrême-droite", "EELV_PS_centre" = "LÉ + PS + C",
    "PS_centre" = "PS + C", "PS_centre_LR" = "PS + C + LR",
    "centre_LR_RN_Reconquete" = "Centre + LR + Extr.-droite",
    "LR_RN_Reconquete" = "LR + Extr.-droite",
    "LFI" = "LFI", "EELV" = "LÉ", "PS" = "PS", "centre" = "Centre", "LR" = "LR"
  )
  h_masks <- lapply(names(h_defs), function(cn) {
    if (cn == "Overall")           rep(TRUE, nrow(e))
    else if (cn == "Left")         !is.na(e$vote_agg) & e$vote_agg == 0
    else if (cn == "Center-right") !is.na(e$vote_agg) & e$vote_agg == 1
    else if (cn == "Far right")    !is.na(e$vote_agg) & e$vote_agg == 2
    else e[[cn]] == 1L & !is.na(e[[cn]])
  })
  names(h_masks) <- names(h_defs)

  # binary Conv+Souh (recompute to be self-contained)
  ba_h <- as.data.frame(sapply(variables_budget, function(v)
    ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1L,
           ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0L, NA_integer_))))

  h_amt   <- setNames(budget_policies$amount[match(variables_budget, budget_policies$variable_name)],
                      variables_budget)
  h_short <- sub("budget_", "", variables_budget)
  h_lbf   <- if (exists("labels_budget_fr")) labels_budget_fr else c(
    aligner_tva_restauration = "Aligner TVA restauration",
    augmenter_age_retraite_65 = "Augmenter âge retraite à 65 ans",
    augmenter_cotisations_salaires_moyens = "Augmenter cotisations salaires moyens",
    augmenter_csg_1pt = "Augmenter CSG (+1 pt)",
    augmenter_duree_travail_droit_chomage = "Augmenter durée travail/chômage",
    augmenter_impot_heritages_eleves = "Augmenter impôt héritages élevés",
    augmenter_impot_revenu_aises = "Augmenter impôt revenu aisés",
    augmenter_impot_revenu_tous = "Augmenter impôt revenu (tous)",
    augmenter_impot_societes = "Augmenter impôt sociétés",
    augmenter_taxe_revenus_capital = "Augmenter taxe revenus du capital",
    augmenter_tva_1pt = "Augmenter TVA (+1 pt)",
    diminuer_credit_impot_recherche = "Diminuer Crédit Impôt Recherche",
    diminuer_subventions_ecole_privee = "Diminuer subventions école privée",
    eliminer_doublons_territoriaux = "Éliminer doublons territoriaux",
    geler_aides_sociales = "Geler aides sociales",
    geler_depenses_etat_collectivites = "Geler dépenses État/collectivités",
    reduire_aides_apprentissage = "Réduire aides apprentissage",
    reduire_depenses_educatives_demographie = "Réduire dépenses éducatives",
    reduire_depenses_militaires = "Réduire dépenses militaires",
    reduire_pensions_retraite = "Réduire pensions de retraite",
    reduire_remboursement_soins = "Réduire remboursement des soins",
    restaurer_taxe_habitation_aises = "Restaurer taxe d'habitation aisés",
    retablir_isf = "Rétablir l'ISF",
    retirer_aides_sociales_etrangers = "Retirer aides aux étrangers",
    soumettre_livret_a_impot = "Livret A à l'impôt",
    supprimer_abattement_ir_retraites = "Supprimer abattement IR retraites",
    supprimer_ame = "Supprimer l'AME",
    supprimer_avantages_fiscaux_complements_salaire = "Fiscaliser compléments de salaire",
    supprimer_exonerations_taxes_carburants = "Supprimer ex. taxes carburants",
    tva_luxe = "TVA augmentée sur le luxe"
  )

  # Rate matrix: measures × coalitions (weighted, NSP excluded)
  cs_mat <- sapply(names(h_masks), function(cn) {
    wg <- ifelse(h_masks[[cn]], e$no_weight, 0)
    sapply(variables_budget, function(v) {
      y <- ba_h[[v]]; ok <- !is.na(y) & wg > 0
      if (!any(ok)) return(NA_real_)
      sum(y[ok] * wg[ok]) / sum(wg[ok])
    })
  })
  rownames(cs_mat) <- variables_budget

  # Row order: ascending overall rate so most popular is at the top
  row_ord  <- order(cs_mat[, "Overall"])
  pol_lbl  <- paste0(h_lbf[h_short], " (", gsub("\\.", ",", sprintf("%.1f", h_amt)), " Mds€)")
  names(pol_lbl) <- variables_budget

  # Weighted median per coalition for the 3 acceptability sums (Mds€)
  med_metrics <- c("Médiane Souhaitable (Mds€)" = "sum_souhaitable",
                   "Médiane Convenable (Mds€)"  = "sum_convenable",
                   "Médiane Acceptable (Mds€)"  = "sum_supportable")
  m_mat <- sapply(names(h_masks), function(cn) {
    sapply(med_metrics, function(v) {
      ok <- h_masks[[cn]] & !is.na(e[[v]]) & !is.na(e$no_weight) & e$no_weight > 0
      if (!any(ok)) return(NA_real_)
      as.numeric(Hmisc::wtd.quantile(e[[v]][ok], e$no_weight[ok], probs = 0.5, na.rm = TRUE))
    })
  })
  rownames(m_mat) <- names(med_metrics)

  # y-axis levels: policies (bottom) → medians (top, "Souhaitable" highest)
  med_levs <- c("Médiane Acceptable (Mds€)", "Médiane Convenable (Mds€)", "Médiane Souhaitable (Mds€)")
  pol_levs <- c(pol_lbl[variables_budget[row_ord]], med_levs)

  # Shared blue palette; rate normalized to [0,1], medians normalized to global max
  blue_pal100 <- colorRampPalette(c("#ffffff", "#1f3a93"))(100)
  hex_from <- function(x) ifelse(is.na(x), "grey90",
                                 blue_pal100[pmin(100, pmax(1, round(x * 99) + 1))])
  m_max <- max(m_mat, na.rm = TRUE)

  df_h <- expand.grid(measure = variables_budget, coalition = names(h_masks), stringsAsFactors = FALSE)
  df_h$rate     <- mapply(function(m, c) cs_mat[m, c], df_h$measure, df_h$coalition)
  df_h$pol_lbl  <- factor(pol_lbl[df_h$measure], levels = pol_levs)
  df_h$col_lbl  <- factor(h_lbl[df_h$coalition],  levels = h_lbl)
  df_h$fill_hex <- hex_from(df_h$rate)
  df_h$lbl_txt  <- ifelse(is.na(df_h$rate), "", sprintf("%.0f", df_h$rate * 100))
  df_h$txt_col  <- ifelse(is.na(df_h$rate) | df_h$rate < 0.55, "black", "white")

  df_m <- expand.grid(metric = names(med_metrics), coalition = names(h_masks), stringsAsFactors = FALSE)
  df_m$value    <- mapply(function(m, c) m_mat[m, c], df_m$metric, df_m$coalition)
  df_m$pol_lbl  <- factor(df_m$metric, levels = pol_levs)
  df_m$col_lbl  <- factor(h_lbl[df_m$coalition], levels = h_lbl)
  df_m$fill_hex <- hex_from(df_m$value / m_max)
  df_m$lbl_txt  <- ifelse(is.na(df_m$value), "", sprintf("%.0f", df_m$value))
  df_m$txt_col  <- ifelse(is.na(df_m$value) | df_m$value / m_max < 0.55, "black", "white")

  face_x_h <- ifelse(h_lbl == h_lbl["Overall"], "bold", "plain")
  face_y_h <- ifelse(pol_levs %in% med_levs, "bold", "plain")

  p_coal_supp <- ggplot() +
    geom_tile(data = df_h, aes(x = col_lbl, y = pol_lbl, fill = fill_hex),
              color = "white", linewidth = 0.3, width = 0.92) +
    geom_text(data = df_h, aes(x = col_lbl, y = pol_lbl, label = lbl_txt, color = I(txt_col)),
              size = 2.1) +
    geom_tile(data = df_m, aes(x = col_lbl, y = pol_lbl, fill = fill_hex),
              color = "white", linewidth = 0.3, width = 0.92) +
    geom_text(data = df_m, aes(x = col_lbl, y = pol_lbl, label = lbl_txt, color = I(txt_col)),
              size = 2.1, fontface = "bold") +
    geom_hline(yintercept = length(variables_budget) + 0.5,
               color = "grey45", linewidth = 0.5) +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 8.5) +
    theme(
      axis.text.x         = element_text(angle = 35, hjust = 0, size = 7.5, face = face_x_h),
      axis.text.y         = element_text(size = 7.5, face = face_y_h),
      legend.position     = "none",
      panel.grid          = element_blank(),
      plot.title          = element_text(size = 9.5, face = "bold", hjust = 0),
      plot.subtitle       = element_text(size = 7.5, color = "black", hjust = 0),
      plot.title.position = "plot",
      plot.margin         = margin(t = 5, r = 60, b = 5, l = 5)
    )

  ggsave("../figures/coalition_support_heatmap.pdf", p_coal_supp,
         width = 7, height = 9, device = cairo_pdf)
  cat("→ ../figures/coalition_support_heatmap.pdf\n")
}

cat("\nAnalyses complete.\n")
