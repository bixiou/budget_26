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
# Also create binary: accept = Souhaitable or Convenable
budget_accept <- sapply(variables_budget, function(v) {
  ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1,
         ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0, NA))
})
budget_accept <- as.data.frame(budget_accept)
program_favorable <- as.data.frame(sapply(variables_effect_program, function(v) {  e[[v]] > 0  }))

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
  # Per-variable: TRUE if at least one of the variable's categories is significant
  sig_by_var <- setNames(sapply(det, function(d) {
    matches <- startsWith(rownames(s), d)
    if (!any(matches)) FALSE else any(sig[matches], na.rm = TRUE)
  }), det)
  lmg <- if (length(det) == 1) {
    setNames(summary(mod)$r.squared, det)
  } else {
    tryCatch(calc.relimp(mod, type = "lmg", rela = FALSE, rank = FALSE)@lmg,
             error = function(e) setNames(rep(NA_real_, length(det)), det))
  }
  list(sig = sig, sig_by_var = sig_by_var, lmg = lmg, R2 = summary(mod)$r.squared)
}

# Stocke la proportion d'attitude regressions où ≥1 catégorie d'une variable
# est significative — un vecteur nommé par configuration de déterminants.
prop_sig_by_var_list <- list()

for (det in list("no.na(vote_agg)", "vote_original", "education_original", determinants[2:11], determinants)) {
  temp <- fit_decomp("sum_convenable", det = det)
  total_sig <- setNames(integer(length(temp$sig)), names(temp$sig))
  total_sig_by_var <- setNames(integer(length(det)), det)
  total_lmg <- setNames(numeric(length(temp$lmg)), names(temp$lmg))
  attitudes <- c(variables_budget, variables_effect_program, "sum_convenable", "sum_souhaitable")
  n_reg <- 0L
  for (v in attitudes) {
    res <- fit_decomp(v, det = det)
    if (!is.null(res)) {
      total_sig <- total_sig + (res$sig > 0)
      total_sig_by_var <- total_sig_by_var + as.integer(res$sig_by_var)
      total_lmg <- total_lmg + res$lmg
      n_reg <- n_reg + 1L
    }
  }
  prop_sig_by_var <- total_sig_by_var / n_reg
  prop_sig_by_var_list[[paste(det, collapse = "+")]] <- prop_sig_by_var
  print(det)
  print(sort(total_sig)) # Number of significant coefs
  cat("Proportion of attitude regressions with ≥1 significant coef per determinant variable (n=", n_reg, "):\n", sep = "")
  print(sort(round(prop_sig_by_var, 3)))
  # print(sort(round(100 * total_lmg / length(attitudes), 2))) # R^2 explained
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
    # Toute valeur manquante (NaN/NA — ex. déterminant systématiquement aliasé
    # par calc.relimp) → 0 ; sinon le bar correspondant ne s'affiche pas.
    df_vals[!is.finite(df_vals)] <- 0
    df <- data.frame(
      label = factor(det_labels[determinants],
                     levels = det_labels[determinants[order(df_vals)]]),
      value = df_vals,
      stringsAsFactors = FALSE
    )
    p <- ggplot(df, aes(x = value, y = label)) +
      geom_col(fill = "#2c6fad", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", value)), hjust = -0.1, size = 2.4, color = "black") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                         labels = function(x) paste0(x, "%")) +
      labs(
           # title = title, subtitle = subtitle,
           x = xlab, y = NULL) +
      theme_bw(base_size = 8) +
      theme(panel.grid.major.y  = element_blank(),
            panel.grid.minor    = element_blank(),
            # Cadre retiré, remplacé par un cadre en L (axe x bas + axe y gauche)
            # — garantit que les bars de longueur ~0 restent ancrées et visibles.
            panel.border        = element_blank(),
            axis.line           = element_line(color = "black", linewidth = 0.3),
            axis.ticks          = element_line(color = "black", linewidth = 0.3),
            axis.text           = element_text(color = "black"),
            axis.title          = element_text(color = "black"),
            plot.title.position = "plot",
            plot.margin         = margin(t = 3, r = 12, b = 3, l = 3))
    ggsave(sprintf("../figures/%s.pdf", tag), p, width = 2.5, height = 1.9, device = cairo_pdf)
    cat("→ ../figures/", tag, ".pdf\n", sep = "")
  }

  make_lmg_figs <- function(vars_set, tag, title_base) {
    cat("\nComputing LMG/R² for", tag, "(", length(vars_set), "variables) ...\n")
    res <- collect_lmg_iso(vars_set)
    bar_fig(res$avg_lmg, tag = paste0("lmg_", tag),
            title    = paste0(title_base, " — LMG"),
            subtitle = sprintf("R² total moyen : %.1f%%", sum(res$avg_lmg)),
            xlab     = "Part moyenne de variance expliquée (%)             ")
    # xlab     = "Part moyenne de variance expliquée (LMG, %)                           ")
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

  # --- Set 3: top 10 most dispersed attitudes (variance / max possible variance) ---
  # max_var = ((max - min) / 2)^2 normalises for different scale widths across variable types
  norm_var <- sapply(attitudes, function(v) {
    x <- as.numeric(e[[v]])
    x <- x[!is.na(x)]
    if (length(x) < 2) return(0)
    rng <- range(x)
    max_var <- ((rng[2] - rng[1]) / 2)^2
    if (max_var == 0) return(0)
    var(x) / max_var
  })
  top15_polarized <- names(sort(norm_var, decreasing = TRUE))[1:15]
  cat("\nTop 15 attitudes les plus dispersées (variance / variance max) :\n")
  print(round(sort(norm_var, decreasing = TRUE)[1:15], 3))
  make_lmg_figs(top15_polarized, "attitudes_dispersees",
                "Déterminants des 15 attitudes les plus dispersées")
}



##### 3. Clustering of respondents #####
# Cluster 1: frugaux (20%), 2: nationalistes (49%), 3: progressistes (31%)
cat("\n=== K-means clustering of respondents (k selected by silhouette) ===\n")
# Cluster on the Likert scores (Inacceptable=-1, Supportable=0, Convenable=1, Souhaitable=2).
# Impute NAs (Ne sais pas) with the column mean so all respondents are kept.
# Create numeric matrix of budget support
attitudes_binary <- c(budget_accept, program_favorable) # program_favorable # budget_accept # c(budget_accept, program_favorable)
mat_imputed <- as.data.frame(sapply(c(variables_budget, variables_effect_program), function(v) e[[v]])) # c(variables_budget, variables_effect_program) variables_budget variables_effect_program
mat_imputed[mat_imputed == -.1] <- NA
for (v in names(mat_imputed)) {
  col_mean <- mean(mat_imputed[[v]], na.rm = TRUE)
  mat_imputed[[v]][is.na(mat_imputed[[v]])] <- col_mean
}

# Pick k by maximizing mean silhouette width over k = 2..10.
d_eucl <- dist(mat_imputed)
k_range <- 2:10
sil_width <- sapply(k_range, function(k) {
  set.seed(42)
  km_k <- kmeans(mat_imputed, centers = k, nstart = 20)
  mean(silhouette(km_k$cluster, d_eucl)[, 3])
})
names(sil_width) <- k_range
cat("Mean silhouette width by k:\n")
print(round(sil_width, 3))
k_opt <- as.integer(names(sil_width)[which.max(sil_width)])
cat(sprintf("Optimal k = %d (silhouette = %.3f)\n", k_opt, max(sil_width)))

k_opt <- 3
set.seed(42)
km <- kmeans(mat_imputed, centers = k_opt, nstart = 20)
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
cluster_means <- aggregate(attitudes_binary, by = list(cluster = km$cluster), FUN = mean, na.rm = TRUE)
print(round(cluster_means, 2))


##### 3d. Programmes à majorité conjointe (SCS ≥50%) par cluster k-means #####
{
  mat_scs <- as.data.frame(sapply(variables_budget, function(v) {
    x <- e[[v]]
    ifelse(x %in% c("Souhaitable", "Convenable", "Supportable"), 1L,
           ifelse(x == "Inacceptable", 0L, NA_integer_))
  }))
  vars_b  <- variables_budget
  m_b     <- length(vars_b)
  short_b <- sub("budget_", "", vars_b)
  amt_b   <- budget_policies$amount[match(vars_b, budget_policies$variable_name)]

  js_cl <- function(cols, wgt) {
    sub   <- mat_scs[, cols, drop = FALSE]
    zeros <- rowSums(sub == 0L, na.rm = TRUE)
    weighted.mean(as.integer(zeros == 0L), wgt, na.rm = TRUE)
  }

  apriori_cl <- function(wgt, threshold = 0.5, label = "") {
    ind_s <- sapply(seq_len(m_b), function(i) js_cl(i, wgt))
    freq  <- which(ind_s > threshold)
    cat(sprintf("[Cluster %s] %d mesures fréquentes (>%.0f%%)\n", label, length(freq), threshold * 100))
    if (!length(freq)) return(list())
    feas_k   <- as.list(seq_len(length(freq)))
    all_feas <- lapply(feas_k, function(i) freq[i])
    k <- 1L
    repeat {
      k <- k + 1L
      if (length(feas_k) < 2) break
      nfp   <- length(feas_k)
      cands <- list()
      for (i in seq_len(nfp - 1L))
        for (j in seq(i + 1L, nfp)) {
          s1 <- feas_k[[i]]; s2 <- feas_k[[j]]
          if (k >= 3 && !identical(s1[-length(s1)], s2[-length(s2)])) next
          cands[[length(cands) + 1L]] <- c(s1, s2[length(s2)])
        }
      if (!length(cands)) break
      feas_k <- list()
      for (cand in cands) {
        sv <- js_cl(freq[cand], wgt)
        if (!is.na(sv) && sv > threshold) {
          feas_k[[length(feas_k) + 1L]] <- cand
          all_feas[[length(all_feas) + 1L]] <- freq[cand]
        }
      }
      cat(sprintf("  k=%d: %d faisables\n", k, length(feas_k)))
      if (!length(feas_k)) break
    }
    all_feas
  }

  cat("\n=== Programmes à majorité conjointe (SCS ≥50%) par cluster k-means ===\n")
  pkg_cluster <- list()
  for (cl in levels(e$cluster)) {
    wgt_cl <- ifelse(!is.na(e$cluster) & e$cluster == cl, e$no_weight, 0)
    feas   <- apriori_cl(wgt_cl, label = cl)
    if (!length(feas)) { cat(sprintf("Cluster %s: aucun paquet faisable.\n", cl)); next }
    amts   <- sapply(feas, function(p) sum(amt_b[p], na.rm = TRUE))
    js_all <- sapply(feas, function(p) js_cl(p, wgt_cl))
    best_s <- feas[[which.max(amts)]]
    best_a <- feas[[which.max(js_all)]]
    cat(sprintf("\nCluster %s (%d paquets faisables):\n", cl, length(feas)))
    cat(sprintf("  [Max économies] %d mesures | %.1f Mds€ | soutien %.1f%%\n    %s\n",
                length(best_s), sum(amt_b[best_s], na.rm = TRUE),
                js_cl(best_s, wgt_cl) * 100, paste(short_b[best_s], collapse = " + ")))
    cat(sprintf("  [Max soutien]   %d mesures | %.1f Mds€ | soutien %.1f%%\n    %s\n",
                length(best_a), sum(amt_b[best_a], na.rm = TRUE),
                js_cl(best_a, wgt_cl) * 100, paste(short_b[best_a], collapse = " + ")))
    cat("  Top 10 paquets (économie décroissante):\n")
    for (idx in order(amts, decreasing = TRUE)[seq_len(min(10, length(feas)))]) {
      cat(sprintf("    %.1f Mds€ | %.1f%% | %s\n",
                  amts[idx], js_all[idx] * 100, paste(short_b[feas[[idx]]], collapse = " + ")))
    }
    pkg_cluster[[cl]] <- best_s
  }
}


##### 3e. Figure: attitudes_binary les plus polarisées par cluster #####
{
  spread_cl <- apply(cluster_means[, -1], 2, function(x) diff(range(x, na.rm = TRUE)))
  top15_cl  <- names(sort(spread_cl, decreasing = TRUE))[1:15]
  cat("\nTop 15 attitudes_binary les plus polarisées (écart max−min entre clusters):\n")
  print(round(sort(spread_cl, decreasing = TRUE)[1:15], 3))

  lbl_ep_loc <- if (exists("labels_effect_program_fr")) labels_effect_program_fr else character(0)
  label_ep   <- function(v) {
    key <- sub("^effect_program_", "", v)
    if (length(lbl_ep_loc) && key %in% names(lbl_ep_loc)) unname(lbl_ep_loc[key])
    else gsub("_", " ", key)
  }

  cl_names  <- setNames(paste("Cluster", levels(e$cluster)), levels(e$cluster))
  cl_colors <- c("Cluster 1" = "#F8766D", "Cluster 2" = "#7CAE00",
                 "Cluster 3" = "#00BFC4", "Cluster 4" = "#C77CFF")

  df_pol <- do.call(rbind, lapply(top15_cl, function(v) {
    vname <- label_ep(v)
    do.call(rbind, lapply(levels(e$cluster), function(cl) {
      mask <- !is.na(e$cluster) & e$cluster == cl
      vals <- as.numeric(attitudes_binary[[v]])[mask]
      w    <- e$no_weight[mask]
      ok   <- !is.na(vals) & w > 0
      data.frame(measure = vname, cluster = cl_names[cl],
                 mean = if (any(ok)) weighted.mean(vals[ok], w[ok]) else NA_real_,
                 stringsAsFactors = FALSE, row.names = NULL)
    }))
  }))

  overall_mu <- colMeans(as.data.frame(lapply(attitudes_binary[top15_cl], as.numeric)), na.rm = TRUE)
  lbl_order  <- sapply(top15_cl[order(overall_mu)], label_ep)
  df_pol$measure <- factor(df_pol$measure, levels = lbl_order)
  df_pol$cluster <- factor(df_pol$cluster, levels = unname(cl_names))

  n_pol    <- length(top15_cl)
  minor_yp <- seq(0.5, n_pol - 0.5, by = 1)

  p_pol <- ggplot(df_pol, aes(y = measure, x = mean, color = cluster, group = cluster)) +
    geom_hline(yintercept = minor_yp, color = "grey85", linewidth = 0.3) +
    geom_point(size = 2.1, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = cl_colors, drop = FALSE) +
    scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"), limits = c(0, 1)) +
    labs(y = NULL, x = NULL, color = "Cluster") +
    theme_bw(base_size = 10) +
    theme(
      legend.position      = "top",
      legend.justification = c(1, 0),
      panel.grid.major.y   = element_blank(),
      panel.grid.minor.y   = element_blank(),
      panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.3),
      axis.text            = element_text(color = "black"),
      legend.text          = element_text(color = "black"),
      legend.title         = element_text(color = "black"),
      plot.margin          = margin(t = 5, r = 18, b = 5, l = 5)
    ) +
    scale_y_discrete(expand = expansion(add = 0.5))

  ggsave("../figures/notes_groupes_cluster_polarises.pdf", p_pol,
         width = 5.5, height = 5, device = cairo_pdf)
  cat("→ ../figures/notes_groupes_cluster_polarises.pdf\n")
}

cat("\nMean sum_convenable (G€) by cluster:\n")
print(round(tapply(e$sum_convenable, km$cluster, mean, na.rm = TRUE), 2))
print(round(tapply(e$sum_souhaitable, km$cluster, mean, na.rm = TRUE), 2))

# Clusters k-means
# attitudes_binary, k=2: 58% right (with ~45% for taxing the rich) vs. 42% left, similar sum_; most polarizing: immigration, green deal; best silhouette
# attitudes_binary, k=3: 27% left, 34% center-right (~50% taxing the rich, against SMIC rise), 39% far right (>50% taxing the rich), center-right highest sum_
# attitudes_binary, k=4: 19% center-right (seuls vmt contre réduire militaire), 35% far right, 23% left, 23% frugal
# budget, k=2: 47% right, 54% left-frugal
# budget, k=3: 21% frugal, 48% right, 31% left; best silhouette (slightly)
# budget, k=4: 21% left, 37% far right, 25% right (contre taxes riches), 18% frugal
# program, k=2: 57% right, 43% left; best silhouette (slightly)
# program, k=3: 26% right, 37% far right, 37% left
# => Quand on sépare en deux, on a une majorité plutôt à gauche sur le budget mais à droite sur le programme, donnant une majorité à droite avec forte minorité pour taxer les riches quand on combine
# => Quand on sépare en trois, on sépare la gauche en frugal et gauche pour le budget, et la droite en centre vs. extrême pour le total; pour le total c'est la combinaison où frugal ~ center-right
# => (Quand on sépare en quatre, on a les 4 catégories, avec far right plus gros, et right + far right > left + frugal)


##### 3f. Cluster comparison table (budget / effect_program / both × k=2,3,4) #####
{
  bud_num <- function(v) case_when(
    e[[v]] == "Souhaitable"              ~  2, e[[v]] == "Convenable"             ~  1,
    e[[v]] == "Supportable"             ~  0, e[[v]] == "Inacceptable"            ~ -1,
    TRUE ~ NA_real_)
  ep_num  <- function(v) case_when(
    e[[v]] == "Beaucoup plus favorable"  ~  2, e[[v]] == "Plus favorable"         ~  1,
    e[[v]] == "Ne changerait rien"      ~  0, e[[v]] == "Moins favorable"         ~ -1,
    e[[v]] == "Beaucoup moins favorable" ~ -2, TRUE ~ NA_real_)

  impute_col_means <- function(df) {
    for (v in names(df)) df[[v]][is.na(df[[v]])] <- mean(df[[v]], na.rm = TRUE)
    df
  }

  mat_b    <- impute_col_means(as.data.frame(sapply(variables_budget,         bud_num)))
  mat_e    <- impute_col_means(as.data.frame(sapply(variables_effect_program, ep_num)))
  mat_both <- cbind(mat_b, mat_e)
  mats     <- list(budget = mat_b, effect_program = mat_e, both = mat_both)

  # Leaning: -1=hurt rich, 0=cost everyone, 0.5=sectoral, 1=shrink welfare state, 2=hurt foreigners
  leaning_b <- setNames(
    budget_policies$leaning[match(variables_budget, budget_policies$variable_name)],
    variables_budget)
  lean_vals   <- sort(unique(leaning_b[!is.na(leaning_b)]))
  lean_nms    <- paste0("lean", lean_vals)
  lean_nms_df <- make.names(lean_nms)  # R sanitises "-" → "." in data.frame col names

  sil_scores <- list()
  rows <- list()
  for (vs in names(mats)) {
    for (k in 2:4) {
      set.seed(42)
      km_tmp <- kmeans(mats[[vs]], centers = k, nstart = 20)
      cl_vec <- km_tmp$cluster
      n_tot  <- length(cl_vec)
      sil_scores[[paste0(vs, "_", k)]] <- mean(cluster::silhouette(cl_vec, dist(mats[[vs]]))[, 3])

      for (j in seq_len(k)) {
        mask <- cl_vec == j
        lean_means <- setNames(vapply(lean_vals, function(lv) {
          vars_lv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
          if (!length(vars_lv)) return(NA_real_)
          round(100*mean(rowMeans(budget_accept[mask, vars_lv, drop = FALSE], na.rm = TRUE), na.rm = TRUE))
        }, numeric(1)), lean_nms)

        rows[[length(rows) + 1]] <- cbind(
          data.frame(
            vars_set = vs, k = k, cluster = j,
            n_pct    = round(sum(mask) / n_tot * 100, 0),
            vote_agg = round(mean(as.numeric(e$vote_agg)[mask], na.rm = TRUE), 2) - 1,
            sum_conv = round(mean(e$sum_convenable[mask], na.rm = TRUE), 0),
            stringsAsFactors = FALSE),
          as.data.frame(as.list(round(lean_means, 2))))
      }
    }
  }

  tbl_long <- do.call(rbind, rows)
  rownames(tbl_long) <- NULL
  best_k <- setNames(vapply(names(mats), function(vs) {
    which.max(sapply(2:4, function(k) sil_scores[[paste0(vs, "_", k)]])) + 1L
  }, integer(1)), names(mats))
  best_global     <- names(which.max(unlist(sil_scores)))
  best_global_vs  <- sub("_[0-9]+$", "", best_global)
  best_global_k   <- as.integer(sub(".*_", "", best_global))
  cat("best_k:", paste(names(best_k), best_k, sep="=", collapse=", "),
      "| best_global:", best_global, "\n")
  write.csv(tbl_long, "../tables/cluster_comparison.csv", row.names = FALSE)
  cat("→ ../tables/cluster_comparison.csv\n")

  # --- Post-process: sort, trim columns, assign labels, export LaTeX ---

  # Ensemble row: stats for all respondents combined
  lean_all_ens <- setNames(vapply(lean_vals, function(lv) {
    vars_lv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
    if (!length(vars_lv)) return(NA_real_)
    round(100 * mean(rowMeans(budget_accept[, vars_lv, drop = FALSE], na.rm = TRUE), na.rm = TRUE))
  }, numeric(1)), lean_nms)
  row_ens <- cbind(
    data.frame(vars_set = "all", k = NA_integer_, cluster = 0L,
               n_pct = 100, vote_agg = round(mean(as.numeric(e$vote_agg), na.rm = TRUE), 2) - 1,
               sum_conv = round(mean(e$sum_convenable, na.rm = TRUE), 0),
               stringsAsFactors = FALSE),
    as.data.frame(as.list(lean_all_ens)))

  # Sort within (vars_set, k) by vote_agg; prepend Ensemble row
  tbl_tex <- tbl_long[order(tbl_long$vars_set, tbl_long$k, tbl_long$vote_agg), ]
  tbl_tex <- rbind(row_ens, tbl_tex)
  rownames(tbl_tex) <- NULL

  # Drop lean0.5; reorder lean columns: lean.1, lean1, lean2 | lean0 last
  lean_ord <- lean_nms_df[lean_vals != 0.5]
  lean_ord <- c(lean_ord[lean_ord != "lean0"], "lean0")
  tbl_tex  <- tbl_tex[, c("vars_set","k","cluster","n_pct","vote_agg","sum_conv", lean_ord)]

  # Assign cluster descriptions and row colors based on lean scores (threshold: 50%)
  col_map <- c(Progressistes         = "cleft",
               Sociaux               = "mgt",
               "Libéraux-frugaux"     = "cdjc",
               Centre                = "cdjc",
               "Libéraux-nativistes" = "ccdroit",
               Conservateurs         = "cdroite",
               Nativistes            = "cdroite",
               "Sociaux-nativistes"  = "ced",
               Frugaux               = "cfrug"
               )

  b <- function(x) isTRUE(x > 50)   # TRUE iff x > 50 and not NA
  assign_label <- function(r) {
    l1 <- r[["lean.1"]]; ls <- r[["lean1"]]; ln <- r[["lean2"]]; l0 <- r[["lean0"]]
    sc <- r[["sum_conv"]]
    if      ( b(l1) &&  b(ls) &&  b(ln) &&  b(l0))                           "Frugaux"
    else if  ( b(l1) &&  b(ls) &&  b(ln) && !b(l0) && isTRUE(sc > 100))      "Libéraux-frugaux"
    else if  ( b(l1) &&  b(ls) &&  b(ln) && !b(l0))                          "Nativistes"
    else if  ( b(l1) && !b(ls) && !b(ln) && !b(l0))                          "Progressistes"
    else if  ( b(l1) && !b(ls) &&  b(ln) && !b(l0) && isTRUE(ln < 60))       "Sociaux"
    else if  ( b(l1) && !b(ls) &&  b(ln) && !b(l0))                          "Sociaux-nativistes"
    else if  (!b(l1) && (l1>30) && b(ln) && !b(l0) && sc < 100)              "Conservateurs" 
    else if  (!b(l1) &&            b(ln) && !b(l0))                          "Libéraux-nativistes"
    else "Centre"
  }

  tbl_tex$desc  <- ""
  tbl_tex$color <- ""
  tbl_tex$desc[tbl_tex$vars_set == "all"] <- "Ensemble"

  non_ens <- which(tbl_tex$vars_set != "all")
  for (i in non_ens) {
    lbl <- assign_label(tbl_tex[i, ])
    tbl_tex$desc[i]  <- lbl
    tbl_tex$color[i] <- col_map[lbl]
  }

  # Place Sociaux-nativistes immediately after Libéraux-nativistes within each (vars_set, k)
  ens_row  <- tbl_tex[tbl_tex$vars_set == "all", , drop = FALSE]
  body     <- tbl_tex[tbl_tex$vars_set != "all", , drop = FALSE]
  grp_keys <- unique(body[, c("vars_set", "k")])
  new_body <- body[0, ]
  for (gi in seq_len(nrow(grp_keys))) {
    sub <- body[body$vars_set == grp_keys$vars_set[gi] & body$k == grp_keys$k[gi], ]
    sn  <- which(sub$desc == "Sociaux-nativistes")
    ln  <- which(sub$desc == "Libéraux-nativistes")
    if (length(sn) == 1 && length(ln) == 1 && sn < ln) {
      rest <- setdiff(seq_len(nrow(sub)), sn)
      ln2  <- which(rest == ln)
      ord  <- c(rest[seq_len(ln2)], sn,
                if (ln2 < length(rest)) rest[(ln2 + 1):length(rest)] else integer(0))
      sub  <- sub[ord, ]
    }
    new_body <- rbind(new_body, sub)
  }
  tbl_tex <- rbind(ens_row, new_body)
  rownames(tbl_tex) <- NULL

  # LaTeX helpers
  vs_lbl <- c(budget         = "\\texttt{budget} (30)",
              effect_program = "\\texttt{programme} (19)",
              both           = "Toutes (49)",
              all            = "")
  neg  <- function(s) sub("^-", "$-$", s)
  fmt0 <- function(x) if (is.na(x) || !is.finite(x)) "" else neg(sprintf("%.0f", x))
  fmt1 <- function(x) if (is.na(x) || !is.finite(x)) "" else paste0(neg(sprintf("%.1f", x)), "\\hspace{1em}")

  to_row <- function(r, show_vs, bold_k = FALSE, bold_vs = FALSE) {
    cc <- if (nchar(r$color) > 0) sprintf("\\cellcolor{%s!15}", r$color) else ""
    cells <- c(
      if (show_vs) {
        lbl <- unname(vs_lbl[r$vars_set])
        if (bold_vs) paste0("{\\bfseries ", lbl, "}") else lbl
      } else "",
      if (show_vs && !is.na(r$k)) {
        k_s <- as.character(r$k)
        if (bold_k) paste0("\\textbf{", k_s, "}") else k_s
      } else "",
      paste0(cc, r$desc), paste0(cc, fmt0(r$n_pct)), paste0(cc, fmt1(r$vote_agg)),
      paste0(cc, fmt0(r$sum_conv)),
      paste0(cc, fmt0(r[["lean.1"]])), paste0(cc, fmt0(r[["lean1"]])),
      paste0(cc, fmt0(r[["lean2"]])),  paste0(cc, fmt0(r[["lean0"]])))
    paste0(paste(cells, collapse = " & "), " \\\\")
  }

  # Group order: budget, effect_program, both (then by k)
  vs_order <- c("budget", "effect_program", "both")
  grps <- unique(tbl_tex[!is.na(tbl_tex$k), c("vars_set","k"), drop = FALSE])
  grps$ord <- match(grps$vars_set, vs_order)
  grps <- grps[order(grps$ord, grps$k), ]
  grps <- grps[!(grps$vars_set == "effect_program" & grps$k == 4), ]  # programme k=4 excluded

  hdr_top <- paste0("& & & & & & \\multicolumn{4}{c}",
                    "{Soutien moyen (\\% de convenable + souhaitable)} \\\\")
  hdr_col <- paste(
    "\\makecell[l]{Variables\\\\utilisées\\\\(leur nombre)}",
    "\\makecell{Nombre\\\\de\\\\profils}",
    "\\makecell[l]{Description\\\\du profil}",
    "\\makecell{Taille\\\\du\\\\profil\\\\(\\%)}",
    "\\makecell{Bloc\\\\politique\\\\moyen}",
    "\\makecell{Moyenne\\\\du paquet\\\\soutenu\\\\(Mds~€)}",
    "\\makecell{Impôt\\\\sur les\\\\riches}",
    "\\makecell{Réduit\\\\l'État-\\\\providence}",
    "\\makecell{Nativiste}",
    "\\makecell{Impôt\\\\indifférencié}",
    sep = " & ")
  hdr_col <- paste0(hdr_col, " \\\\")

  tex <- c(
    "\\noindent\\makebox[\\textwidth][c]{%",
    "\\begin{tabular}{lclcrccccc}",
    "\\toprule",
    hdr_top,
    "\\cmidrule(lr){7-10}",
    hdr_col,
    "\\midrule"
  )

  # Ensemble row (no vars_set / k shown)
  r_ens <- tbl_tex[tbl_tex$vars_set == "all", ]
  tex   <- c(tex, to_row(r_ens, show_vs = FALSE), "\\midrule")

  # Clustering rows; \midrule between groups
  for (gi in seq_len(nrow(grps))) {
    vs_g <- as.character(grps$vars_set[gi]); k_g <- as.integer(grps$k[gi])
    idx  <- which(tbl_tex$vars_set == vs_g & !is.na(tbl_tex$k) & tbl_tex$k == k_g)
    bk   <- isTRUE(k_g == as.integer(best_k[[vs_g]]))
    for (ri in seq_along(idx))
      tex <- c(tex, to_row(tbl_tex[idx[ri], ], show_vs = (ri == 1),
                           bold_k = bk, bold_vs = bk))
    if (gi < nrow(grps)) tex <- c(tex, "\\midrule")
  }

  tex <- c(tex,
    "\\bottomrule",
    "\\end{tabular}}")

  writeLines(tex, "../tables/cluster_comparison.tex")
  cat("→ ../tables/cluster_comparison.tex\n")

  # --- Overlap analysis: budget k=2/3/4 ---
  cat("\n=== Budget cluster overlap (k=2/3/4) ===\n")
  set.seed(42); km_b2 <- kmeans(mat_b, centers = 2, nstart = 20)
  set.seed(42); km_b3 <- kmeans(mat_b, centers = 3, nstart = 20)
  set.seed(42); km_b4 <- kmeans(mat_b, centers = 4, nstart = 20)

  leans_for_cl <- function(km, j) {
    mask <- km$cluster == j
    v <- setNames(c(
      vapply(lean_vals, function(lv) {
        vv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
        if (!length(vv)) return(NA_real_)
        100 * mean(rowMeans(budget_accept[mask, vv, drop=FALSE], na.rm=TRUE), na.rm=TRUE)
      }, numeric(1)),
      mean(e$sum_convenable[mask], na.rm=TRUE)
    ), c(lean_nms_df, "sum_conv"))
    as.list(v)
  }

  lbls_b2 <- sapply(1:2, function(j) assign_label(leans_for_cl(km_b2, j)))
  lbls_b3 <- sapply(1:3, function(j) assign_label(leans_for_cl(km_b3, j)))
  lbls_b4 <- sapply(1:4, function(j) assign_label(leans_for_cl(km_b4, j)))
  cat("budget k=2:", paste(1:2, lbls_b2, sep="=", collapse=", "), "\n")
  cat("budget k=3:", paste(1:3, lbls_b3, sep="=", collapse=", "), "\n")
  cat("budget k=4:", paste(1:4, lbls_b4, sep="=", collapse=", "), "\n")

  j_ln2 <- which(lbls_b2 == "Libéraux-nativistes")
  j_co3 <- which(lbls_b3 == "Conservateurs")
  j_ln3 <- which(lbls_b3 == "Libéraux-nativistes")
  j_sn4 <- which(lbls_b4 == "Sociaux-nativistes")
  j_ln4 <- which(lbls_b4 == "Libéraux-nativistes")

  if (length(j_ln2) && length(j_co3)) {
    m2 <- km_b2$cluster == j_ln2; m3 <- km_b3$cluster == j_co3
    cat(sprintf("LN(k=2) ∩ Conservateurs(k=3): %d / %d LN (%.0f%%) / %d Co (%.0f%%)\n",
                sum(m2 & m3), sum(m2), 100*mean(m3[m2]), sum(m3), 100*mean(m2[m3])))
  } else {
    cat("Libéraux-nativistes ou Conservateurs absents dans la config demandée.\n")
    cat("  k=2 labels:", paste(lbls_b2, collapse=", "),
        "| k=3 labels:", paste(lbls_b3, collapse=", "), "\n")
  }

  if (length(j_ln4)) {
    m4   <- km_b4$cluster == j_ln4
    dist2 <- table(km_b2$cluster[m4])
    cat("Libéraux-nativistes (budget k=4) → budget k=2:\n")
    for (j in names(dist2))
      cat(sprintf("  k=2 cl%s (%s): %d (%.0f%%)\n",
                  j, lbls_b2[as.integer(j)], dist2[[j]], 100*dist2[[j]]/sum(dist2)))
  } else {
    cat("Libéraux-nativistes absents dans budget k=4.\n")
    cat("  k=4 labels:", paste(lbls_b4, collapse=", "), "\n")
  }

  if (length(j_ln3)) {
    m3ln <- km_b3$cluster == j_ln3
    dist4 <- table(km_b4$cluster[m3ln])
    cat("Libéraux-nativistes (budget k=3) → budget k=4:\n")
    for (j in names(dist4))
      cat(sprintf("  k=4 cl%s (%s): %d (%.0f%%)\n",
                  j, lbls_b4[as.integer(j)], dist4[[j]], 100*dist4[[j]]/sum(dist4)))
  } else {
    cat("Libéraux-nativistes absents dans budget k=3.\n")
    cat("  k=3 labels:", paste(lbls_b3, collapse=", "), "\n")
  }

  # Console summary
  cat("\n=== Cluster comparison (post-processed) ===\n")
  cat(sprintf("%-20s  k  %-22s  n%%  sumG  lean.-1  lean1  lean2  lean0\n", "vars_set", "desc"))
  for (i in seq_len(nrow(tbl_tex))) {
    r    <- tbl_tex[i, ]
    k_s  <- if (is.na(r$k)) " " else as.character(r$k)
    line <- sprintf("%-20s  %s  %-22s  %3.0f  %4.0f  %7.0f  %5.0f  %5.0f  %5.0f",
                    r$vars_set, k_s, r$desc, r$n_pct, r$sum_conv,
                    r[["lean.1"]], r[["lean1"]], r[["lean2"]], r[["lean0"]])
    cat(line, "\n")
  }
}


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
# Cluster 1: right-wing (no majority on left), 64G€, 2: unpopular, 121G€, 3: left-wing (though majority in all blocks), 39G€
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
      axis.text.x          = element_text(angle = 35, hjust = 0, size = 7.5, face = face_x, color = "black"),
      axis.text.y          = element_text(size = 7.5, face = face_y, color = "black"),
      legend.position      = "bottom",
      legend.text          = element_text(size = 8, color = "black"),
      legend.title         = element_text(color = "black"),
      panel.grid           = element_blank(),
      plot.title.position  = "plot",
      plot.caption.position = "plot",
      plot.title           = element_text(size = 9.5, face = "bold", hjust = 0, color = "black"),
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
      axis.text.x         = element_text(angle = 35, hjust = 0, size = 7.5, face = face_x_h, color = "black"),
      axis.text.y         = element_text(size = 7.5, face = face_y_h, color = "black"),
      legend.position     = "none",
      panel.grid          = element_blank(),
      plot.title          = element_text(size = 9.5, face = "bold", hjust = 0, color = "black"),
      plot.subtitle       = element_text(size = 7.5, color = "black", hjust = 0),
      plot.title.position = "plot",
      plot.margin         = margin(t = 5, r = 60, b = 5, l = 5)
    )

  ggsave("../figures/coalition_support_heatmap.pdf", p_coal_supp,
         width = 7, height = 9, device = cairo_pdf)
  cat("→ ../figures/coalition_support_heatmap.pdf\n")
}

cat("\nAnalyses complete.\n")
