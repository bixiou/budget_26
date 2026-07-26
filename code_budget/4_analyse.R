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
summary(lm(gcs_support == "Yes" ~ variant_gcs, data = e, weights = weight)) # -.10** Bandwagon effect!
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
program_favorable <- as.data.frame(sapply(unname(variables_effect_program), function(v) {  e[[v]] > 0  }))

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
attitudes_binary <- budget_accept # program_favorable # budget_accept # c(budget_accept, program_favorable)
mat_imputed <- as.data.frame(sapply(variables_budget, function(v) e[[v]])) # c(variables_budget, variables_effect_program) variables_budget variables_effect_program
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
      feas_k <- list(); found_110 <- FALSE
      for (cand in cands) {
        sv <- js_cl(freq[cand], wgt)
        if (!is.na(sv) && sv > threshold) {
          feas_k[[length(feas_k) + 1L]] <- cand
          all_feas[[length(all_feas) + 1L]] <- freq[cand]
          if (sum(amt_b[freq[cand]], na.rm = TRUE) > 110) { found_110 <- TRUE; break }
        }
      }
      cat(sprintf("  k=%d: %d faisables\n", k, length(feas_k)))
      if (!length(feas_k) || found_110) break
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

  # Vote-agg decomposition rows (inserted after Ensemble in the table)
  vote_vals  <- c("Left", "Center-right or Right", "Far right", "PNR or Other")
  vote_names <- c("Gauche", "Centre-droit/Droite", "Extrême-droite", "Non-réponse/Autre")
  rows_vote <- lapply(seq_along(vote_vals), function(i) {
    val  <- vote_vals[i]
    mask <- (e$vote_agg) == val
    lmeans <- setNames(vapply(lean_vals, function(lv) {
      vv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
      if (!length(vv)) return(NA_real_)
      round(100*mean(rowMeans(budget_accept[mask, vv, drop=FALSE], na.rm=TRUE), na.rm=TRUE))
    }, numeric(1)), lean_nms_df)[lean_ord]
    r_lst <- c(list(vars_set="vote", k=NA_integer_, cluster=NA_integer_,
                    n_pct=round(sum(mask)/nrow(e)*100, 0),
                    vote_agg=round(mean(as.numeric(e$vote_agg)[mask], na.rm=TRUE), 2) - 1,
                    sum_conv=round(mean(e$sum_convenable[mask], na.rm=TRUE), 0)),
               as.list(lmeans))
    lbl <- assign_label(r_lst)
    data.frame(vars_set="vote", k=NA_integer_, cluster=NA_integer_,
               n_pct=r_lst$n_pct, vote_agg=r_lst$vote_agg, sum_conv=r_lst$sum_conv,
               as.data.frame(as.list(lmeans), stringsAsFactors=FALSE),
               desc=vote_names[i], color=unname(col_map[lbl]),
               stringsAsFactors=FALSE)
  })
  tbl_vote <- do.call(rbind, rows_vote)
  tbl_tex  <- rbind(tbl_tex, tbl_vote)
  rownames(tbl_tex) <- NULL

  # LaTeX helpers
  vs_lbl <- c(budget         = "\\texttt{budget} (30)",
              effect_program = "\\texttt{programme} (17)",
              both           = "Toutes (47)",
              all            = "",
              vote           = "\\multicell{Bloc\\\\politique (1)}")
  neg  <- function(s) sub("^-", "$-$", s)
  fmt0 <- function(x) if (is.na(x) || !is.finite(x)) "" else neg(sprintf("%.0f", x))
  fmt1 <- function(x) {
    if (is.na(x) || !is.finite(x)) return("")
    s <- sub(",0$", "", sub("\\.", ",", sprintf("%.1f", x)))
    paste0(neg(s), "\\hspace{1em}")
  }

  to_row <- function(r, show_vs, bold_k = FALSE, bold_vs = FALSE, bold_row = FALSE) {
    cc  <- if (nchar(r$color) > 0) sprintf("\\cellcolor{%s!15}", r$color) else ""
    bf  <- function(s) if (bold_row && nchar(trimws(s)) > 0) paste0("\\textbf{", s, "}") else s
    cells <- c(
      if (show_vs) {
        lbl <- unname(vs_lbl[r$vars_set])
        if (bold_vs) paste0("{\\bfseries ", lbl, "}") else lbl
      } else "",
      if (show_vs && !is.na(r$k)) {
        k_s <- as.character(r$k)
        if (bold_k) paste0("\\textbf{", k_s, "}") else k_s
      } else "",
      paste0(cc, bf(r$desc)), paste0(cc, bf(fmt0(r$n_pct))), paste0(cc, bf(fmt1(r$vote_agg))),
      paste0(cc, bf(fmt0(r$sum_conv))),
      paste0(cc, bf(fmt0(r[["lean.1"]]))), paste0(cc, bf(fmt0(r[["lean1"]]))),
      paste0(cc, bf(fmt0(r[["lean2"]]))),  paste0(cc, bf(fmt0(r[["lean0"]]))))
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
    "\\makecell{Nombre\\\\de\\\\profils\\\\$k$}",
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
  tex   <- c(tex, to_row(r_ens, show_vs = FALSE, bold_row = TRUE), "\\midrule")

  # Vote-agg rows
  rows_v <- which(tbl_tex$vars_set == "vote")
  for (ri in seq_along(rows_v))
    tex <- c(tex, to_row(tbl_tex[rows_v[ri], ], show_vs = (ri == 1)))
  tex <- c(tex, "\\midrule")

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

  # --- Table: vote_agg distribution within budget clusters k=2/3/4 ---
  {
    # columns: Gauche, Centre-droit/Droite, Extrême-droite, Non-rép./Autre
    vote_col_nms  <- c("Gauche\\\\\\quad", "Centre-droit/\\\\Droite", "Extrême-\\\\droite", "Non-réponse/\\\\Autre")
    vote_col_vals <- c("Left", "Center-right or Right", "Far right", "PNR or Other")

    set.seed(42); km_bv2 <- kmeans(mat_b, centers = 2, nstart = 20)
    set.seed(42); km_bv3 <- kmeans(mat_b, centers = 3, nstart = 20)
    set.seed(42); km_bv4 <- kmeans(mat_b, centers = 4, nstart = 20)

    lbl_bv2 <- sapply(1:2, function(j) assign_label(leans_for_cl(km_bv2, j)))
    lbl_bv3 <- sapply(1:3, function(j) assign_label(leans_for_cl(km_bv3, j)))
    lbl_bv4 <- sapply(1:4, function(j) assign_label(leans_for_cl(km_bv4, j)))

    # share of profile j within each vote bloc v: sum(w[j & v]) / sum(w[v])
    profile_share_in_bloc <- function(cl_vec, j, val, w = e$no_weight) {
      vv <- as.character(e$vote_agg_factor)
      in_bloc    <- !is.na(vv) & vv == val
      in_profile <- cl_vec == j
      round(100 * sum(w[in_bloc & in_profile]) / sum(w[in_bloc]))
    }

    # Row ordering: Sociaux/Progressistes=1, Frugaux=2, others=3, Sociaux-nativistes=4
    row_priority <- function(lbl) {
      if (grepl("^(Sociaux|Progressistes)$", lbl))   1L
      else if (lbl == "Frugaux")                      2L
      else if (lbl == "Sociaux-nativistes")           4L
      else                                            3L
    }

    # Bloc shares among all respondents (for column headers)
    vv_all      <- as.character(e$vote_agg_factor)
    w_all       <- e$no_weight
    bloc_pcts   <- sapply(vote_col_vals, function(val)
      round(100 * sum(w_all[!is.na(vv_all) & vv_all == val]) / sum(w_all)))

    # Column headers: name + share on last line
    col_hdrs <- mapply(function(nm, pct)
      sprintf("\\makecell{%s\\\\(%d\\,\\%%)} ", nm, pct),
      vote_col_nms, bloc_pcts)

    tex_v <- c(
      "\\begin{tabular}{clccccc}",
      "\\toprule",
      paste0("& & & \\multicolumn{4}{c}{Part du profil dans le bloc politique (\\%)} \\\\"),
      "\\cmidrule(lr){4-7}",
      paste0(c("$k$", "\\makecell[l]{Description\\\\du profil}",
               "\\makecell{Taille\\\\du profil\\\\(\\%)}", col_hdrs), collapse = " & "),
      " \\\\",
      "\\midrule"
    )

    for (cfg in list(list(km=km_bv2, k=2, lbls=lbl_bv2),
                     list(km=km_bv3, k=3, lbls=lbl_bv3),
                     list(km=km_bv4, k=4, lbls=lbl_bv4))) {
      km_c <- cfg$km; k_c <- cfg$k; lbls_c <- cfg$lbls
      sub_c <- tbl_tex[tbl_tex$vars_set == "budget" & !is.na(tbl_tex$k) & tbl_tex$k == k_c, ]
      ord   <- order(sapply(lbls_c, row_priority))
      for (ri in seq_along(ord)) {
        ji     <- ord[ri]
        shares <- sapply(vote_col_vals, function(val)
          profile_share_in_bloc(km_c$cluster, ji, val))
        lbl    <- lbls_c[ji]
        color  <- unname(col_map[lbl])
        cc     <- if (nchar(color) > 0) sprintf("\\cellcolor{%s!15}", color) else ""
        desc   <- sub_c$desc[sub_c$cluster == ji]
        n_pct  <- sub_c$n_pct[sub_c$cluster == ji]
        k_cell <- if (ri == 1) as.character(k_c) else ""
        tex_v  <- c(tex_v, paste(
          c(k_cell, paste0(cc, desc),
            paste0(cc, n_pct),
            paste0(cc, shares)),
          collapse = " & "), " \\\\")
      }
      tex_v <- c(tex_v, "\\midrule")
    }
    tex_v[length(tex_v)] <- "\\bottomrule"
    tex_v <- c(tex_v, "\\end{tabular}")

    writeLines(tex_v, "../tables/cluster_vote_composition.tex")
    cat("→ ../tables/cluster_vote_composition.tex\n")
  }

  # --- K-means on measures ---
  {
    # Transposed matrices: rows=measures, cols=respondents
    tmats <- list(
      budget         = t(mat_b),
      effect_program = t(mat_e),
      both           = t(mat_both)
    )

    # Monetary amounts per measure (Mds€); NA/0 for programme measures
    amt_budget <- setNames(
      budget_policies$amount[match(variables_budget, budget_policies$variable_name)],
      variables_budget)
    amt_ep <- setNames(rep(0, ncol(mat_e)), colnames(mat_e))

    # Vote_agg bloc labels and matching strings
    bloc_vals  <- c("Left", "Center-right or Right", "Far right", "PNR or Other")
    bloc_nms   <- c("Gauche", "Centre-droit/\\\\Droite", "Extrême-\\\\droite", "Non-rép./\\\\Autre")

    # Weighted mean vote_agg (on -1..2 scale) among supporters of measure v
    mean_vote_sup <- function(v) {
      vals <- as.numeric(attitudes_binary[[v]])
      sup  <- !is.na(vals) & vals > 0
      if (!any(sup)) return(NA_real_)
      round(weighted.mean(as.numeric(e$vote_agg)[sup], e$no_weight[sup], na.rm = TRUE) - 1, 2)
    }

    # Weighted mean support rate for measure v among respondents in bloc
    support_in_bloc <- function(v, bloc_str) {
      vf   <- as.character(e$vote_agg_factor)
      mask <- !is.na(vf) & vf == bloc_str
      vals <- as.numeric(attitudes_binary[[v]])[mask]
      w    <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
      if (!any(ok)) return(NA_real_)
      weighted.mean(vals[ok], w[ok])
    }

    vs_lbl_m <- c(budget         = "\\texttt{budget} (30)",
                  effect_program = "\\texttt{programme} (17)",
                  both           = "Toutes (47)")

    rows_m <- list()
    sil_m  <- list()
    for (vs in names(tmats)) {
      for (k in 2:4) {
        set.seed(42)
        km_m   <- kmeans(tmats[[vs]], centers = k, nstart = 20)
        cl_vec <- km_m$cluster
        sil_m[[paste0(vs, "_", k)]] <- mean(
          cluster::silhouette(cl_vec, dist(tmats[[vs]]))[, 3])
        var_nms <- rownames(tmats[[vs]])   # measure variable names

        for (j in seq_len(k)) {
          vv <- var_nms[cl_vec == j]
          n_meas  <- length(vv)
          amt_j   <- if (vs == "effect_program") 0
                     else sum(amt_budget[intersect(vv, names(amt_budget))], na.rm = TRUE)
          vote_j  <- round(mean(sapply(vv, mean_vote_sup), na.rm = TRUE), 2)
          sup_j   <- sapply(bloc_vals, function(b)
            round(100 * mean(sapply(vv, support_in_bloc, bloc_str = b), na.rm = TRUE)))
          rows_m[[length(rows_m) + 1]] <- c(
            list(vars_set = vs, k = k, cluster = j, n_meas = n_meas,
                 amt = round(amt_j, 1), vote_agg = vote_j),
            as.list(sup_j))
        }
      }
    }
    tbl_m <- do.call(rbind, lapply(rows_m, as.data.frame))

    best_km <- setNames(vapply(names(tmats), function(vs) {
      which.max(sapply(2:4, function(k) sil_m[[paste0(vs, "_", k)]])) + 1L
    }, integer(1)), names(tmats))

    neg_m  <- function(s) sub("^-", "$-$", s)
    fmt_v  <- function(x) {
      if (is.na(x) || !is.finite(x)) return("")
      s <- sub(",0$", "", sub("\\.", ",", sprintf("%.1f", x)))
      paste0(neg_m(s), "\\hspace{1em}")
    }
    fmt_a  <- function(x) if (is.na(x) || !is.finite(x) || x == 0) "" else
                            neg_m(sub("\\.", ",", sprintf("%.1f", x)))
    fmt_n  <- function(x) if (is.na(x)) "" else as.character(x)

    tex_m <- c(
      # 9 columns: vars(l) k(c) taille(c) montant(r) bloc(c) + 4 support(c)
      "\\noindent\\makebox[\\textwidth][c]{%",
      sprintf("\\begin{tabular}{lccr%s}", paste(rep("c", 1 + length(bloc_vals)), collapse = "")),
      "\\toprule",
      paste0("& & & & & \\multicolumn{", length(bloc_vals),
             "}{c}{Soutien moyen (\\%)} \\\\"),
      sprintf("\\cmidrule(lr){6-%d}", 5 + length(bloc_vals)),
      paste(c("\\makecell[l]{Variables\\\\utilisées}", "$k$",
              "\\makecell{Taille\\\\(n mes.)}",
              "\\makecell{Montant\\\\(Mds~€)}",
              "\\makecell{Bloc\\\\pol.\\\\moyen}",
              sapply(bloc_nms, function(n) sprintf("\\makecell{%s}", n))),
            collapse = " & "),
      " \\\\",
      "\\midrule"
    )

    grps_m <- unique(tbl_m[, c("vars_set", "k")])
    grps_m <- grps_m[order(match(grps_m$vars_set, names(tmats)), grps_m$k), ]

    for (gi in seq_len(nrow(grps_m))) {
      vs_g <- as.character(grps_m$vars_set[gi])
      k_g  <- as.integer(grps_m$k[gi])
      bk   <- isTRUE(k_g == as.integer(best_km[[vs_g]]))
      idx  <- which(tbl_m$vars_set == vs_g & tbl_m$k == k_g)
      vs_s <- if (bk) paste0("{\\bfseries ", vs_lbl_m[vs_g], "}")
              else vs_lbl_m[vs_g]
      for (ri in seq_along(idx)) {
        r   <- tbl_m[idx[ri], ]
        sup_cells <- sapply(make.names(bloc_vals), function(b) fmt_n(r[[b]]))
        cells <- c(
          if (ri == 1) vs_s else "",
          if (ri == 1) fmt_n(k_g) else "",
          fmt_n(r$n_meas), fmt_a(r$amt), fmt_v(r$vote_agg),
          sup_cells)
        tex_m <- c(tex_m, paste(cells, collapse = " & "), " \\\\")
      }
      if (gi < nrow(grps_m)) tex_m <- c(tex_m, "\\midrule")
    }
    tex_m <- c(tex_m, "\\bottomrule", "\\end{tabular}}")

    writeLines(tex_m, "../tables/cluster_measures_comparison.tex")
    cat("→ ../tables/cluster_measures_comparison.tex\n")

    # --- Diagnostic + table for budget k=3 measure clusters ---
    lbl_b_m  <- if (exists("labels_budget_fr"))         labels_budget_fr         else character(0)
    lbl_ep_m <- if (exists("labels_effect_program_fr")) labels_effect_program_fr else character(0)
    label_meas <- function(v) {
      if (startsWith(v, "effect_program_")) {
        key <- sub("^effect_program_", "", v)
        if (length(lbl_ep_m) && key %in% names(lbl_ep_m)) unname(lbl_ep_m[key]) else gsub("_", " ", key)
      } else {
        key <- sub("^budget_", "", v)
        if (length(lbl_b_m) && key %in% names(lbl_b_m)) unname(lbl_b_m[key]) else gsub("_", " ", key)
      }
    }

    set.seed(42); km_m_b3    <- kmeans(tmats[["budget"]],         centers = 3, nstart = 20)
    set.seed(42); km_m_ep3   <- kmeans(tmats[["effect_program"]], centers = 3, nstart = 20)
    set.seed(42); km_m_both3 <- kmeans(tmats[["both"]],           centers = 3, nstart = 20)

    cat("\n=== Measures in budget k=3 ===\n")
    for (j in 1:3) {
      vv <- names(km_m_b3$cluster)[km_m_b3$cluster == j]
      cat(sprintf("Cluster %d (mean leaning=%.2f, n=%d):\n", j,
                  mean(leaning_b[intersect(vv, names(leaning_b))], na.rm = TRUE), length(vv)))
      for (v in vv) cat(sprintf("  %s\n", label_meas(v)))
    }
    cat("\n=== Measures in both k=3 ===\n")
    for (j in 1:3) {
      vv <- names(km_m_both3$cluster)[km_m_both3$cluster == j]
      cat(sprintf("Cluster %d (n=%d): %s\n", j, length(vv),
                  paste(sapply(vv, label_meas), collapse = ", ")))
    }
    cat("\n=== Programme k=3 ===\n")
    for (j in 1:3) {
      vv <- names(km_m_ep3$cluster)[km_m_ep3$cluster == j]
      cat(sprintf("Cluster %d (n=%d): %s\n", j, length(vv),
                  paste(sapply(vv, label_meas), collapse = ", ")))
    }
    cat("\n=== both k=3 × budget k=3 (measures, cross-tab) ===\n")
    ct_b <- table(both = km_m_both3$cluster[names(km_m_b3$cluster)],
                  budget = km_m_b3$cluster)
    # sort(km_m_both3$cluster) gives the cluster of both (k=3; 1: d/retr, 2: g/redistr, 3: impop/indif), which happens to be the union of budget(3) and program(3), 
    #   except for Réduire aide au dvlpt (cluster impop/indif dans both mais d/retranchement dans program)
    # sort(km_m_b3$cluster)
    # 2 g/redistr:   taxes riches (y.c. école privée, ONU), CIR, taxes carburants, [éducation & santé, retraite 62 ans, SMIC, RIC, proportionnelle]
    # 3 impop/indif: taxes indifférenciées, retraite 65 ans, hausse IS, TVA resto, réduire: militaire, apprentissage, remboursement soins, [hausse allocs, régulariser sans-papiers, Green Deal, éducation, retraites, APD]
    # 1 d/retranch:  consolidation (gel dépenses, fin doublons), anti-étrangers, gel aides sociales, augmenter durée travail droit chômage, [réduire déficit,  sécuritaire]
    print(ct_b)
    cat("\n=== both k=3 × programme k=3 (measures, cross-tab) ===\n")
    ct_ep <- table(both = km_m_both3$cluster[names(km_m_ep3$cluster)],
                   programme = km_m_ep3$cluster)
    print(ct_ep)
    cat("\n=== Mesures programme-3 cl.3 hors both-3 cl.1 ===\n")
    ep3_cl3 <- names(km_m_ep3$cluster)[km_m_ep3$cluster == 3]
    for (v in ep3_cl3) {
      both_cl <- km_m_both3$cluster[v]
      if (!is.na(both_cl) && both_cl != 1)
        cat(sprintf("  %s -> both cl.%d\n", label_meas(v), both_cl))
    }
    }

    # Assign cluster names by ascending mean leaning
    mean_lean3 <- sapply(1:3, function(j) {
      vv <- names(km_m_b3$cluster)[km_m_b3$cluster == j]
      mean(leaning_b[intersect(vv, names(leaning_b))], na.rm = TRUE)
    })
    cl3_order <- order(mean_lean3)
    # cl3_names <- c("Sociales-redistributives", "Co\\^uts indifférenciés", "Moins d'État-providence")
    # cl3_names <- c("Progressistes", "Frugales", "Conservatrices")
    # cl3_names <- c("Redistributives", "Indifférenciées", "Retranchement social")
    cl3_names <- c("De gauche", "Impopulaires", "De droite")

    # Budget k=3 respondent profiles
    set.seed(42); km_resp_b3 <- kmeans(mats[["budget"]], centers = 3, nstart = 20)
    sub_resp_b3 <- tbl_tex[tbl_tex$vars_set == "budget" & !is.na(tbl_tex$k) & tbl_tex$k == 3, ]
    resp3_j    <- sub_resp_b3$cluster
    resp3_desc <- sub_resp_b3$desc

    sup_in_resp <- function(vv, p) {
      mask <- km_resp_b3$cluster == p
      round(100 * mean(sapply(vv, function(v) {
        vals <- as.numeric(attitudes_binary[[v]])[mask]
        w <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
        if (!any(ok)) return(NA_real_)
        weighted.mean(vals[ok], w[ok])
      }), na.rm = TRUE))
    }

    bloc_vals_t3 <- c("Left", "Center-right or Right", "Far right", "PNR or Other")
    bloc_nms_t3  <- c("Gauche", "Centre-\\\\droit/\\\\Droite", "Extr.-\\\\droite", "Non-\\\\rép./\\\\Autre")

    # Decimal-aligned vote_agg: phantom minus for positive values
    fmt_vote_dec <- function(x) {
      if (is.na(x) || !is.finite(x)) return("")
      s <- sub("\\.", ",", sprintf("%.1f", abs(x)))
      if (x < 0) paste0("$-$", s) else paste0("\\phantom{$-$}", s)
    }

    # Profile column headers: split long names across two lines
    prof_hdr <- function(n) {
      n2 <- switch(n,
        Progressistes  = "Progres-\\\\sistes",
        Conservateurs  = "Conser-\\\\vateurs",
        n)
      sprintf("\\makecell{%s}", n2)
    }

    # 9 cols: name(l) bloc_moyen(c) 4×bloc(c) 3×profil(c)
    tex_m3 <- c(
      "\\noindent\\makebox[\\textwidth][c]{%",
      "\\begin{tabular}{lcccccccc}",
      "\\toprule",
      paste0("& & \\multicolumn{4}{c}{Soutien moyen par bloc politique (\\%)} & ",
             "\\multicolumn{3}{c}{Soutien moyen par profil (\\%)} \\\\"),
      "\\cmidrule(lr){3-6}\\cmidrule(lr){7-9}",
      paste(c("\\makecell[l]{Ensemble de mesures\\\\(nombre; \\textit{économies})}",
              "\\makecell{Bloc\\\\pol.\\\\moyen}",
              sapply(bloc_nms_t3, function(n) sprintf("\\makecell{%s}", n)),
              sapply(resp3_desc,  prof_hdr)),
            collapse = " & "),
      " \\\\",
      "\\midrule"
    )
    for (ci in seq_along(cl3_order)) {
      j   <- cl3_order[ci]
      vv  <- names(km_m_b3$cluster)[km_m_b3$cluster == j]
      amt_j   <- sum(amt_budget[intersect(vv, names(amt_budget))], na.rm = TRUE)
      amt_str <- sub("\\.", ",", sprintf("%.1f", amt_j))
      nm  <- sprintf("\\textbf{%s} (%d; \\textit{%s~Mds})", cl3_names[ci], length(vv), amt_str)
      vote_j  <- round(mean(sapply(vv, mean_vote_sup), na.rm = TRUE), 1)
      sup_bloc <- sapply(bloc_vals_t3, function(b)
        round(100 * mean(sapply(vv, support_in_bloc, bloc_str = b), na.rm = TRUE)))
      sup_prof <- sapply(resp3_j, function(p) sup_in_resp(vv, p))
      tex_m3 <- c(tex_m3,
        paste(c(nm, fmt_vote_dec(vote_j),
                as.character(sup_bloc), as.character(sup_prof)),
              collapse = " & "),
        " \\\\")
    }
    tex_m3 <- c(tex_m3, "\\bottomrule", "\\end{tabular}}")
    writeLines(tex_m3, "../tables/cluster_measures_budget3.tex")
    cat("→ ../tables/cluster_measures_budget3.tex\n")
  }

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

  # --- Figures: attitudes les plus polarisées pour chaque clustering ---
  {
    hex_col <- c(cleft    = "#F8766D", ccdroit = "#619CFF", cdroite = "#815EF8",
                 ced      = "#A020F0", cfrug   = "#009600", cdjc    = "#9BA073",
                 mgt      = "#F0509B")

    lbl_ep_f <- if (exists("labels_effect_program_fr")) labels_effect_program_fr else character(0)
    lbl_b_f  <- if (exists("labels_budget_fr"))         labels_budget_fr         else character(0)
    label_att <- function(v) {
      if (startsWith(v, "effect_program_")) {
        key <- sub("^effect_program_", "", v)
        if (length(lbl_ep_f) && key %in% names(lbl_ep_f)) unname(lbl_ep_f[key])
        else gsub("_", " ", key)
      } else {
        key <- sub("^budget_", "", v)
        if (length(lbl_b_f) && key %in% names(lbl_b_f)) unname(lbl_b_f[key])
        else gsub("_", " ", key)
      }
    }

    att_nms    <- names(attitudes_binary)
    ks_for_vs  <- list(budget = 2:4, effect_program = 2:3, both = 2:4)
    vs_lbl_fig <- c(budget = "budget", effect_program = "programme", both = "toutes")

    for (vs in names(ks_for_vs)) {
      for (k in ks_for_vs[[vs]]) {
        set.seed(42)
        cl_vec <- kmeans(mats[[vs]], centers = k, nstart = 20)$cluster

        sub_tbl   <- tbl_tex[tbl_tex$vars_set == vs & !is.na(tbl_tex$k) & tbl_tex$k == k, ]
        cl_desc   <- paste0(sub_tbl$desc, " (", sub_tbl$n_pct, "%)")
        cl_labels <- setNames(cl_desc, as.character(sub_tbl$cluster))
        cl_colors <- setNames(
          ifelse(sub_tbl$color %in% names(hex_col), hex_col[sub_tbl$color], "grey70"),
          cl_desc)

        wmean_cl <- function(v, j) {
          mask <- cl_vec == j
          vals <- as.numeric(attitudes_binary[[v]])[mask]
          w    <- e$no_weight[mask]
          ok   <- !is.na(vals) & w > 0
          if (!any(ok)) return(NA_real_)
          weighted.mean(vals[ok], w[ok])
        }

        spread <- sapply(att_nms, function(v)
          diff(range(sapply(seq_len(k), function(j) wmean_cl(v, j)), na.rm = TRUE)))
        top15 <- names(sort(spread, decreasing = TRUE))[1:min(15, sum(!is.na(spread)))]

        rows_p <- list()
        for (v in top15) for (j in seq_len(k)) {
          mask <- cl_vec == j
          vals <- as.numeric(attitudes_binary[[v]])[mask]
          w    <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
          if (!any(ok)) next
          mu <- weighted.mean(vals[ok], w[ok])
          se <- sqrt(mu * (1 - mu) / (sum(w[ok])^2 / sum(w[ok]^2)))
          rows_p[[length(rows_p) + 1]] <- data.frame(
            measure = label_att(v), cluster = cl_labels[as.character(j)],
            mean = mu, xmin = max(0, mu - 1.96*se), xmax = min(1, mu + 1.96*se),
            stringsAsFactors = FALSE)
        }
        df_p <- if (length(rows_p)) do.call(rbind, rows_p) else
          data.frame(measure=character(), cluster=character(), mean=numeric(),
                     xmin=numeric(), xmax=numeric(), stringsAsFactors=FALSE)

        # top15 is already sorted descending by spread; reverse for y-axis (most dispersed on top)
        df_p$measure <- factor(df_p$measure,
                               levels = sapply(rev(top15), label_att))
        df_p$cluster <- factor(df_p$cluster, levels = cl_desc)

        n_top    <- length(top15)
        minor_yp <- seq(0.5, n_top - 0.5, by = 1)

        dodge_w <- 0.45
        p <- ggplot(df_p, aes(y = measure, x = mean, color = cluster, group = cluster)) +
          geom_hline(yintercept = minor_yp, color = "grey85", linewidth = 0.3) +
          geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey50",
                     linewidth = 0.4) +
          geom_errorbarh(aes(xmin = xmin, xmax = xmax),
                         height = 0, linewidth = 0.35,
                         position = position_dodge(width = dodge_w)) +
          geom_point(size = 2.5, position = position_dodge(width = dodge_w)) +
          scale_color_manual(values = cl_colors, drop = FALSE) +
          scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
                             limits = c(0, 1)) +
          labs(y = NULL, x = NULL, color = NULL) +
          theme_bw(base_size = 10) +
          theme(
            legend.position      = "top",
            legend.justification = c(1, 0),
            panel.grid.major.y   = element_blank(),
            panel.grid.minor.y   = element_blank(),
            panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.3),
            axis.text            = element_text(color = "black"),
            legend.text          = element_text(color = "black"),
            plot.margin          = margin(t = 5, r = 18, b = 5, l = 5)
          ) +
          scale_y_discrete(expand = expansion(add = 0.5))

        fname <- sprintf("../figures/clusters_%s_k%d_polarises.pdf",
                         vs_lbl_fig[vs], k)
        ggsave(fname, p, width = 6, height = 5.5, device = cairo_pdf)
        cat("→", fname, "\n")
      }
    }

    # Figure pour vote_agg
    {
      sub_vote  <- tbl_tex[tbl_tex$vars_set == "vote", ]
      grp_names <- paste0(sub_vote$desc, " (", sub_vote$n_pct, "%)")
      grp_vals  <- vote_vals                     # c(-1, 0, 1, 2)
      grp_masks <- lapply(grp_vals, function(val) e$vote_agg == val)
      vote_colors <- setNames(
        ifelse(sub_vote$color %in% names(hex_col), hex_col[sub_vote$color], "grey70"),
        grp_names)
      vote_colors[grepl("Centre",    names(vote_colors))] <- "#74B9FF"
      vote_colors[grepl("Non-r",     names(vote_colors))] <- "grey60"

      wmean_v <- function(v, mask) {
        vals <- as.numeric(attitudes_binary[[v]])[mask]
        w    <- e$no_weight[mask]
        ok   <- !is.na(vals) & w > 0
        if (!any(ok)) return(NA_real_)
        weighted.mean(vals[ok], w[ok])
      }

      spread_v <- sapply(att_nms, function(v)
        diff(range(sapply(grp_masks, function(m) wmean_v(v, m)), na.rm = TRUE)))
      top15_v <- names(sort(spread_v, decreasing = TRUE))[1:min(15, sum(!is.na(spread_v)))]

      rows_v <- list()
      for (v in top15_v) for (i in seq_along(grp_names)) {
        mask <- grp_masks[[i]]
        vals <- as.numeric(attitudes_binary[[v]])[mask]
        w    <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
        if (!any(ok)) next
        mu <- weighted.mean(vals[ok], w[ok])
        se <- sqrt(mu * (1 - mu) / (sum(w[ok])^2 / sum(w[ok]^2)))
        rows_v[[length(rows_v) + 1]] <- data.frame(
          measure = label_att(v), cluster = grp_names[i],
          mean = mu, xmin = max(0, mu - 1.96*se), xmax = min(1, mu + 1.96*se),
          stringsAsFactors = FALSE)
      }
      df_v <- if (length(rows_v)) do.call(rbind, rows_v) else
        data.frame(measure=character(), cluster=character(), mean=numeric(),
                   xmin=numeric(), xmax=numeric(), stringsAsFactors=FALSE)
      df_v$measure <- factor(df_v$measure, levels = sapply(rev(top15_v), label_att))
      df_v$cluster <- factor(df_v$cluster, levels = grp_names)

      n_top_v  <- length(top15_v)
      minor_yv <- seq(0.5, n_top_v - 0.5, by = 1)

      p_v <- ggplot(df_v, aes(y = measure, x = mean, color = cluster, group = cluster)) +
        geom_hline(yintercept = minor_yv, color = "grey85", linewidth = 0.3) +
        geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey50",
                   linewidth = 0.4) +
        geom_errorbarh(aes(xmin = xmin, xmax = xmax),
                       height = 0, linewidth = 0.35,
                       position = position_dodge(width = 0.45)) +
        geom_point(size = 2.5, position = position_dodge(width = 0.45)) +
        scale_color_manual(values = vote_colors, drop = FALSE) +
        scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
                           limits = c(0, 1)) +
        labs(y = NULL, x = NULL, color = NULL) +
        theme_bw(base_size = 10) +
        theme(
          legend.position      = "top",
          legend.justification = c(1, 0),
          panel.grid.major.y   = element_blank(),
          panel.grid.minor.y   = element_blank(),
          panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.3),
          axis.text            = element_text(color = "black"),
          legend.text          = element_text(color = "black"),
          plot.margin          = margin(t = 5, r = 18, b = 5, l = 5)
        ) +
        scale_y_discrete(expand = expansion(add = 0.5))

      ggsave("../figures/clusters_vote_polarises.pdf", p_v,
             width = 6, height = 5.5, device = cairo_pdf)
      cat("→ ../figures/clusters_vote_polarises.pdf\n")
    }

    # Figure combinée: vote_agg + budget k=2/3/4, variables fixées
    {
      fixed_vars <- c(variables_effect_program[c(4, 6:8, 11, 12, 14:17)], variables_budget[c(3, 12, 19, 26, 27)])

      # Build per-grouping CI rows for a fixed variable list
      make_rows_fixed <- function(v_list, masks, grp_labels) {
        rows <- list()
        for (v in v_list) for (i in seq_along(grp_labels)) {
          mask <- masks[[i]]
          vals <- as.numeric(attitudes_binary[[v]])[mask]
          w    <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
          if (!any(ok)) next
          mu <- weighted.mean(vals[ok], w[ok])
          se <- sqrt(mu * (1 - mu) / (sum(w[ok])^2 / sum(w[ok]^2)))
          rows[[length(rows) + 1]] <- data.frame(
            measure = label_att(v), cluster = grp_labels[i],
            mean = mu, xmin = max(0, mu - 1.96*se), xmax = min(1, mu + 1.96*se),
            stringsAsFactors = FALSE)
        }
        if (length(rows)) do.call(rbind, rows) else
          data.frame(measure=character(), cluster=character(), mean=numeric(),
                     xmin=numeric(), xmax=numeric(), stringsAsFactors=FALSE)
      }

      # vote_agg grouping
      sub_vote2   <- tbl_tex[tbl_tex$vars_set == "vote", ]
      vg_names    <- paste0(sub_vote2$desc, " (", sub_vote2$n_pct, "%)")
      vg_masks    <- lapply(vote_vals, function(val) e$vote_agg == val)
      vg_colors   <- setNames(
        ifelse(sub_vote2$color %in% names(hex_col), hex_col[sub_vote2$color], "grey70"),
        vg_names)
      vg_colors[grepl("Centre", names(vg_colors))] <- "#74B9FF"
      vg_colors[grepl("Non-r",  names(vg_colors))] <- "grey60"
      df_vg <- make_rows_fixed(fixed_vars, vg_masks, vg_names)

      # budget k=2, 3, 4
      km_list <- list()
      for (k in 2:4) {
        set.seed(42)
        km_list[[as.character(k)]] <- kmeans(mats[["budget"]], centers = k, nstart = 20)
      }

      panels_df  <- list(vote = list(df = df_vg, colors = vg_colors,
                                      title = "Bloc politique"))
      for (k in 2:4) {
        cl_vec_k <- km_list[[as.character(k)]]$cluster
        sub_k    <- tbl_tex[tbl_tex$vars_set == "budget" & !is.na(tbl_tex$k) & tbl_tex$k == k, ]
        cl_desc  <- paste0(sub_k$desc, " (", sub_k$n_pct, "%)")
        cl_lbl_k <- setNames(cl_desc, as.character(sub_k$cluster))
        cl_col_k <- setNames(
          ifelse(sub_k$color %in% names(hex_col), hex_col[sub_k$color], "grey70"),
          cl_desc)
        masks_k  <- lapply(seq_len(k), function(j) cl_vec_k == j)
        labs_k   <- cl_lbl_k[as.character(seq_len(k))]
        df_k     <- make_rows_fixed(fixed_vars, masks_k, labs_k)
        panels_df[[paste0("b", k)]] <- list(df = df_k, colors = cl_col_k,
                                             title = sprintf("budget k=%d", k))
      }

      # Global spread for y-axis ordering
      spread_g <- sapply(fixed_vars, function(v) {
        all_means <- unlist(lapply(panels_df, function(pd) {
          rows_v2 <- pd$df[pd$df$measure == label_att(v), "mean"]
          if (length(rows_v2)) rows_v2 else NA_real_
        }))
        diff(range(all_means, na.rm = TRUE))
      })
      var_order <- names(sort(spread_g, decreasing = TRUE))
      lev_order <- sapply(rev(var_order), label_att)  # bottom to top

      # One ggplot per panel
      make_panel <- function(pd, show_y, dodge_w = 0.55) {
        df_i <- pd$df
        df_i$measure <- factor(df_i$measure, levels = lev_order)
        df_i$cluster <- factor(df_i$cluster, levels = names(pd$colors))
        n_rows <- length(fixed_vars)
        minor_yi <- seq(0.5, n_rows - 0.5, by = 1)
        ggplot(df_i, aes(y = measure, x = mean, color = cluster, group = cluster)) +
          geom_hline(yintercept = minor_yi, color = "grey85", linewidth = 0.3) +
          geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey50",
                     linewidth = 0.4) +
          geom_errorbarh(aes(xmin = xmin, xmax = xmax),
                         height = 0, linewidth = 0.35,
                         position = position_dodge(width = dodge_w)) +
          geom_point(size = 2.5, position = position_dodge(width = dodge_w)) +
          scale_color_manual(values = pd$colors, drop = FALSE) +
          scale_x_continuous(labels = function(x) paste0(round(x * 100), "%")) +
          guides(color = guide_legend(nrow = if (length(pd$colors) >= 3) 2 else 1)) +
          labs(y = NULL, x = NULL, color = NULL, title = pd$title) +
          theme_bw(base_size = 14) +
          theme(
            legend.position      = "top",
            legend.justification = c(if (length(pd$colors) == 3) 3 else 0.7, 0),
            legend.key.size      = unit(0.45, "cm"),
            panel.grid.major.y   = element_blank(),
            panel.grid.minor.y   = element_blank(),
            panel.grid.major.x   = element_line(color = "grey90", linewidth = 0.3),
            panel.border         = element_rect(linewidth = 0.4),
            axis.text.x          = element_text(color = "black"),
            axis.text.y          = if (show_y) element_text(color = "black")
                                   else element_blank(),
            axis.ticks.y         = if (show_y) element_line() else element_blank(),
            legend.text          = element_text(color = "black"),
            plot.title           = element_text(hjust = 0.5, size = 14),
            plot.margin          = margin(t = 3, r = 6, b = 3, l = if (show_y) 3 else 1)
          ) +
          scale_y_discrete(expand = expansion(add = 0.5))
      }

      # 2×2 layout: row 1 = k3 (left, with y labels) + vote_agg (right)
      #             row 2 = k4 (left, with y labels) + k2 (right)
      p_comb <- (make_panel(panels_df[["b3"]],   show_y = TRUE) |
                   make_panel(panels_df[["vote"]], show_y = FALSE)) /
                (make_panel(panels_df[["b4"]],   show_y = TRUE) |
                   make_panel(panels_df[["b2"]],  show_y = FALSE))

      ggsave("../figures/clusters_combined_polarises.pdf", p_comb,
             width = 10, height = 10, device = cairo_pdf)
      cat("→ ../figures/clusters_combined_polarises.pdf\n")
    }
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


##### 4a. Distances inter-individuelles (budget k=3) #####
{
  d_b    <- as.matrix(dist(mats[["budget"]]))
  cl_vec <- km_resp_b3$cluster
  cl_map <- setNames(resp3_desc, as.character(resp3_j))

  idx_up    <- which(upper.tri(d_b), arr.ind = TRUE)
  d_overall <- mean(d_b[idx_up])

  # Within-cluster mean pairwise distances
  within_vals <- setNames(sapply(resp3_j, function(j) {
    idx_j <- which(cl_vec == j)
    sub   <- d_b[idx_j, idx_j, drop = FALSE]
    if (nrow(sub) < 2) return(NA_real_)
    mean(sub[upper.tri(sub)])
  }), cl_map[as.character(resp3_j)])

  # Between-cluster mean pairwise distances
  pairs_cl    <- combn(sort(unique(cl_vec)), 2, simplify = FALSE)
  between_vals <- setNames(sapply(pairs_cl, function(pair) {
    i1 <- which(cl_vec == pair[1]); i2 <- which(cl_vec == pair[2])
    mean(d_b[i1, i2])
  }), sapply(pairs_cl, function(pair)
    paste(cl_map[as.character(pair)], collapse = "\n↔ ")))

  df_dist <- rbind(
    data.frame(type  = "Intra-profil",
               label = names(within_vals),
               value = within_vals / d_overall,
               stringsAsFactors = FALSE),
    data.frame(type  = "Inter-profil",
               label = names(between_vals),
               value = between_vals / d_overall,
               stringsAsFactors = FALSE)
  )
  df_dist$type  <- factor(df_dist$type, levels = c("Intra-profil", "Inter-profil"))
  df_dist$label <- factor(df_dist$label, levels = df_dist$label)

  p_dist <- ggplot(df_dist, aes(x = label, y = value, fill = type)) +
    geom_col(width = 0.65) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.4, size = 2.4, color = "black") +
    scale_fill_manual(values = c("Intra-profil" = "#2c6fad", "Inter-profil" = "#e07b39")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = "Distance relative à l'ensemble", fill = NULL) +
    theme_bw(base_size = 8) +
    theme(
      legend.position    = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_blank(),
      axis.line          = element_line(color = "black", linewidth = 0.3),
      axis.ticks         = element_line(color = "black", linewidth = 0.3),
      axis.text          = element_text(color = "black", size = 6.5),
      axis.text.x        = element_text(lineheight = 0.85),
      plot.margin        = margin(t = 3, r = 8, b = 3, l = 3)
    )
  ggsave("../figures/distances_profils_budget_k3.pdf", p_dist,
         width = 4.5, height = 3.2, device = cairo_pdf)
  cat("→ ../figures/distances_profils_budget_k3.pdf\n")
}


##### 4b. R² moyen et LMG des attitudes sur le profil (budget k=3) #####
{
  e$cluster_b3 <- factor(km_resp_b3$cluster)

  # R² moyen des attitudes ~ profil budget k=3
  r2_cl <- sapply(attitudes, function(v) {
    y  <- as.numeric(e[[v]])
    ok <- !is.na(y) & !is.na(e$cluster_b3)
    if (sum(ok) < 4) return(NA_real_)
    summary(lm(y[ok] ~ e$cluster_b3[ok], weights = e$no_weight[ok]))$r.squared
  })
  cat(sprintf("\nR² moyen des attitudes sur le profil budget k=3 : %.1f%%\n",
              mean(r2_cl, na.rm = TRUE) * 100))

  # LMG moyen avec profil + socio-démos
  det_labels_cl <- c("cluster_b3" = "Profil (budget k=3)", det_labels)
  det_with_cl   <- c("cluster_b3", determinants)
  det_novote_cl <- c("cluster_b3", determinants[determinants != "no.na(vote_agg)"])

  avg_lmg_for <- function(det) {
    lmg_mat <- matrix(NA_real_, nrow = length(det), ncol = length(attitudes),
                      dimnames = list(det, attitudes))
    for (v in attitudes) {
      res <- fit_decomp(v, det = det)
      if (!is.null(res)) {
        shared <- intersect(det, names(res$lmg))
        lmg_mat[shared, v] <- res$lmg[shared]
      }
    }
    rowMeans(lmg_mat, na.rm = TRUE) * 100
  }

  bar_fig_lmg <- function(avg_lmg, det_lbs, tag) {
    avg_lmg[!is.finite(avg_lmg)] <- 0
    df <- data.frame(
      label = factor(det_lbs[names(avg_lmg)],
                     levels = det_lbs[names(avg_lmg)[order(avg_lmg)]]),
      value = avg_lmg,
      stringsAsFactors = FALSE
    )
    p <- ggplot(df, aes(x = value, y = label)) +
      geom_col(fill = "#2c6fad", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", value)), hjust = -0.1, size = 2.4, color = "black") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                         labels = function(x) paste0(x, "%")) +
      labs(x = "Part moyenne de variance expliquée (%)", y = NULL) +
      theme_bw(base_size = 8) +
      theme(panel.grid.major.y  = element_blank(),
            panel.grid.minor    = element_blank(),
            panel.border        = element_blank(),
            axis.line           = element_line(color = "black", linewidth = 0.3),
            axis.ticks          = element_line(color = "black", linewidth = 0.3),
            axis.text           = element_text(color = "black"),
            axis.title          = element_text(color = "black"),
            plot.margin         = margin(t = 3, r = 12, b = 3, l = 3))
    ggsave(sprintf("../figures/%s.pdf", tag), p, width = 2.5, height = 2.2, device = cairo_pdf)
    cat("→ ../figures/", tag, ".pdf\n", sep = "")
  }

  cat("\nComputing LMG (profil + socio-démos) ...\n")
  lmg_with_cl <- avg_lmg_for(det_with_cl)
  cat(sprintf("LMG moyen Profil budget k=3 : %.1f%%\n", lmg_with_cl["cluster_b3"]))
  bar_fig_lmg(lmg_with_cl, det_labels_cl, "lmg_profil_sociodemos")

  cat("\nComputing LMG (profil + socio-démos hors vote) ...\n")
  lmg_novote_cl <- avg_lmg_for(det_novote_cl)
  cat(sprintf("LMG moyen Profil budget k=3 (hors vote) : %.1f%%\n", lmg_novote_cl["cluster_b3"]))
  bar_fig_lmg(lmg_novote_cl, det_labels_cl, "lmg_profil_sociodemos_hors_vote")
}


##### 4c. Paquets à majorité conjointe par profil (budget k=2/3/4 + ep k=2 + both k=2) #####
{
  lbl_bfr <- if (exists("labels_budget_fr")) labels_budget_fr else
    setNames(gsub("_", " ", short_b), short_b)

  # Helper commun : construit rows pour une configuration (km, vars_set, k_c, k_label, pfx)
  build_rows_pkg <- function(km_c, vs, k_c, k_label, pfx) {
    cl_vec <- km_c$cluster
    sub_k  <- tbl_tex[tbl_tex$vars_set == vs & !is.na(tbl_tex$k) & tbl_tex$k == k_c, ]
    out    <- list()
    for (ri in seq_len(nrow(sub_k))) {
      j      <- sub_k$cluster[ri]
      lbl    <- assign_label(leans_for_cl(km_c, j))
      n_pct  <- sub_k$n_pct[ri]
      wgt_cl <- ifelse(!is.na(cl_vec) & cl_vec == j, e$no_weight, 0)
      ind_sup <- sapply(seq_len(m_b), function(i) js_cl(i, wgt_cl))
      feas    <- apriori_cl(wgt_cl, label = sprintf("%s_cl%d", pfx, j))
      pkg_idx <- integer(0); js_pkg <- NA_real_; amt_pkg <- NA_real_
      if (length(feas)) {
        js_vec  <- sapply(feas, function(p) js_cl(p, wgt_cl))
        best    <- which.max(sapply(feas, function(p) sum(amt_b[p], na.rm = TRUE)))
        pkg_idx <- feas[[best]]
        js_pkg  <- js_vec[best]
        amt_pkg <- sum(amt_b[pkg_idx], na.rm = TRUE)
      }
      save.image('.RData')
      prof_lbl <- if (!is.na(js_pkg))
        sprintf("%s (%d%%)\nSCS %.0f%%, %.0f Mds€", lbl, n_pct, js_pkg * 100, amt_pkg)
      else sprintf("%s (%d%%)\n∅", lbl, n_pct)
      for (i in seq_len(m_b))
        out[[length(out) + 1]] <- data.frame(
          k_label  = k_label,
          prof_ord = paste0(pfx, "_", sprintf("%02d", ri)),
          profile  = prof_lbl,
          measure  = vars_b[i], meas_key = short_b[i],
          in_pkg   = i %in% pkg_idx, sup = ind_sup[i] * 100,
          stringsAsFactors = FALSE)
    }
    out
  }

  # ── Bloc A : profils budget k = 2/3/4 ──────────────────────────────────
  {
    rows_pkg_budget <- c(
      build_rows_pkg(km_b2, "budget", 2L, "Budget k = 2", "b2"),
      build_rows_pkg(km_b3, "budget", 3L, "Budget k = 3", "b3"),
      build_rows_pkg(km_b4, "budget", 4L, "Budget k = 4", "b4")
    )
  }

  # ── Bloc B : profils effect_program k=2 et both k=2 ────────────────────
  # Peut être relancé seul sans refaire le Bloc A.
  {
    set.seed(42); km_e2    <- kmeans(mat_e,    centers = 2, nstart = 20)
    set.seed(42); km_both2 <- kmeans(mat_both, centers = 2, nstart = 20)
    rows_pkg_new <- c(
      build_rows_pkg(km_e2,    "effect_program", 2L, "Programme k = 2", "e2"),
      build_rows_pkg(km_both2, "both",            2L, "Toutes k = 2",    "t2")
    )
  }

  # ── Figure (combine A + B) ──────────────────────────────────────────────
  {
    df_pkg <- do.call(rbind, c(rows_pkg_budget, rows_pkg_new))

    vars_in_pkg <- unique(df_pkg$measure[df_pkg$in_pkg])
    df_pkgf     <- df_pkg[df_pkg$measure %in% vars_in_pkg, ]

    df_pkgf$meas_lbl <- lbl_bfr[df_pkgf$meas_key]
    df_pkgf$meas_lbl[is.na(df_pkgf$meas_lbl)] <-
      gsub("_", " ", df_pkgf$meas_key[is.na(df_pkgf$meas_lbl)])
    amt_key <- setNames(amt_b, short_b)
    df_pkgf$meas_lbl <- paste0(df_pkgf$meas_lbl,
      ifelse(!is.na(amt_key[df_pkgf$meas_key]) & amt_key[df_pkgf$meas_key] > 0,
             paste0(" (", gsub("\\.", ",", sprintf("%g", amt_key[df_pkgf$meas_key])), " Mds)"), ""))

    # y-axis order: ascending overall SCS support (most popular at top)
    overall_sup_pkg <- setNames(
      sapply(vars_in_pkg, function(v) js_cl(which(vars_b == v), e$no_weight)),
      vars_in_pkg)
    y_ord     <- vars_in_pkg[order(overall_sup_pkg)]
    lbl_y_ord <- unique(df_pkgf$meas_lbl[match(y_ord, df_pkgf$measure)])

    # Per-profile summary: extract SCS support % and savings from profile label
    # Profile labels have format "Label (N%)\nSCS X%, Y Mds€" or "Label (N%)\n∅"
    prof_rows <- unique(df_pkgf[, c("profile", "prof_ord", "k_label")])
    prof_rows$scs_pct <- ifelse(
      grepl("SCS", prof_rows$profile),
      as.numeric(sub(".*SCS ([0-9.]+)%.*", "\\1", prof_rows$profile)),
      NA_real_)
    prof_rows$scs_amt <- ifelse(
      grepl("SCS", prof_rows$profile),
      as.numeric(sub(".*SCS [0-9.]+%, ([0-9.]+) Mds.*", "\\1", prof_rows$profile)),
      NA_real_)

    # Gradient fill for summary rows (white → dark blue), keyed by position
    blue_pal <- colorRampPalette(c("#ffffff", "#1f3a93"))(100)
    max_amt  <- max(prof_rows$scs_amt, na.rm = TRUE)
    n_prof   <- nrow(prof_rows)
    sav_keys <- paste0("sav_", seq_len(n_prof))
    sup_keys <- paste0("sup_", seq_len(n_prof))
    sav_hex  <- ifelse(is.na(prof_rows$scs_amt), "grey90",
                       blue_pal[pmin(100, pmax(1, round(prof_rows$scs_amt / max(max_amt, 1) * 99) + 1))])
    sup_hex  <- ifelse(is.na(prof_rows$scs_pct), "grey90",
                       blue_pal[pmin(100, pmax(1, round(prof_rows$scs_pct / 100 * 99) + 1))])

    df_sav_pkg <- data.frame(
      profile = prof_rows$profile, prof_ord = prof_rows$prof_ord,
      k_label = prof_rows$k_label, meas_lbl = "Économies (Mds€)",
      lbl_txt = ifelse(is.na(prof_rows$scs_amt), "∅", sprintf("%.0f", prof_rows$scs_amt)),
      fill_cat = sav_keys, txt_col = "black", stringsAsFactors = FALSE)

    df_sup_pkg <- data.frame(
      profile = prof_rows$profile, prof_ord = prof_rows$prof_ord,
      k_label = prof_rows$k_label, meas_lbl = "Soutien au paquet d'Ensemble (%)",
      lbl_txt = ifelse(is.na(prof_rows$scs_pct), "∅", sprintf("%.0f", prof_rows$scs_pct)),
      fill_cat = sup_keys,
      txt_col  = ifelse(!is.na(prof_rows$scs_pct) & prof_rows$scs_pct > 55, "white", "black"),
      stringsAsFactors = FALSE)

    # All fills through scale_fill_manual (same trick as coalition_packages_matrix)
    fill_vals <- c(in_pkg = "#2c6fad", out_pkg = "grey92",
                   setNames(sav_hex, sav_keys), setNames(sup_hex, sup_keys))

    # y-axis levels: policies (bottom, ascending) → SCS support → savings (top)
    pol_levs <- c(lbl_y_ord, "Soutien au paquet d'Ensemble (%)", "Économies (Mds€)")
    face_y   <- ifelse(pol_levs %in% c("Économies (Mds€)", "Soutien au paquet d'Ensemble (%)"), "bold", "plain")

    # Profile x-axis: strip SCS/savings line (now shown in summary rows)
    prof_levs <- unique(df_pkgf$profile[order(df_pkgf$prof_ord)])
    x_lbls    <- setNames(gsub("(\\d)%", "\\1 %", sub("\n.*", "", as.character(prof_levs))), as.character(prof_levs))

    df_pkgf$fill_cat  <- ifelse(df_pkgf$in_pkg, "in_pkg", "out_pkg")
    df_pkgf$meas_lbl  <- factor(df_pkgf$meas_lbl,  levels = pol_levs)
    df_pkgf$profile   <- factor(df_pkgf$profile,    levels = prof_levs)
    df_sav_pkg$meas_lbl <- factor(df_sav_pkg$meas_lbl, levels = pol_levs)
    df_sav_pkg$profile  <- factor(df_sav_pkg$profile,  levels = prof_levs)
    df_sup_pkg$meas_lbl <- factor(df_sup_pkg$meas_lbl, levels = pol_levs)
    df_sup_pkg$profile  <- factor(df_sup_pkg$profile,  levels = prof_levs)

    lvl_k <- c("Budget k = 2", "Budget k = 3", "Budget k = 4", "Programme k = 2", "Toutes k = 2")
    lvl_k <- lvl_k[lvl_k %in% df_pkgf$k_label]
    lvl_k_disp <- gsub(" = ", "=", lvl_k)
    df_pkgf$k_label    <- factor(gsub(" = ", "=", df_pkgf$k_label),    levels = lvl_k_disp)
    df_sav_pkg$k_label <- factor(gsub(" = ", "=", df_sav_pkg$k_label), levels = lvl_k_disp)
    df_sup_pkg$k_label <- factor(gsub(" = ", "=", df_sup_pkg$k_label), levels = lvl_k_disp)

    df_all <- rbind(df_pkgf[,    c("k_label","profile","meas_lbl","fill_cat")],
                    df_sav_pkg[, c("k_label","profile","meas_lbl","fill_cat")],
                    df_sup_pkg[, c("k_label","profile","meas_lbl","fill_cat")])
    df_txt <- rbind(df_sav_pkg[, c("k_label","profile","meas_lbl","lbl_txt","txt_col")],
                    df_sup_pkg[, c("k_label","profile","meas_lbl","lbl_txt","txt_col")])

    p_pkg <- ggplot() +
      geom_tile(data = df_all,
                aes(x = profile, y = meas_lbl, fill = fill_cat),
                color = "white", linewidth = 0.15, width = 1) +
      geom_text(data = df_txt,
                aes(x = profile, y = meas_lbl, label = lbl_txt, color = I(txt_col)),
                size = 2.3, fontface = "bold") +
      geom_hline(yintercept = length(lbl_y_ord) + 0.5, color = "grey45", linewidth = 0.5) +
      scale_fill_manual(
        values = fill_vals,
        breaks = c("in_pkg", "out_pkg"),
        labels = c(in_pkg = "Dans le paquet", out_pkg = "Hors du paquet"),
        name   = NULL) +
      scale_x_discrete(labels = x_lbls, position = "top") +
      facet_grid(. ~ k_label, scales = "free_x", space = "free_x", switch = "x") +
      labs(x = NULL, y = NULL) +
      theme_bw(base_size = 7.5) +
      theme(
        strip.background  = element_rect(fill = "grey90", color = NA),
        strip.text        = element_text(face = "bold", size = 5),
        strip.placement   = "outside",
        axis.text.x       = element_text(size = 6.5, angle = 45, hjust = 0, vjust = 0),
        axis.text.y       = element_text(size = 6.5, face = face_y),
        axis.ticks        = element_line(linewidth = 0.25),
        legend.position   = "bottom",
        legend.key.width  = unit(1.2, "cm"),
        legend.key.height = unit(0.3, "cm"),
        panel.grid        = element_blank(),
        panel.border      = element_rect(color = "grey70", linewidth = 0.3),
        panel.spacing     = unit(0, "lines"),
        plot.margin       = margin(t = 3, r = 60, b = 3, l = 5)
      )

    ggsave("../figures/paquets_profils_budget.pdf", p_pkg,
           width = 6.5, height = 5.5, device = cairo_pdf)
    cat("→ ../figures/paquets_profils_budget.pdf\n")
  }
}


##### 3old. Alternative clustering: Ward hierarchical on an ordinal+NSP distance #####
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
