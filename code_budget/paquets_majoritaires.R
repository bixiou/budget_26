### Analyses complètes — paquets majoritaires et préférences budgétaires
### (1)/(1bis)/(1ter) Paquet majoritaire avec la plus grande économie
### (2) Proportion max P soutenant conjointement un paquet ≥ 90 Mds€
### (3)/(3bis) Paquets majoritaires par groupe de votants
### (4) Paquet maximisant la somme des utilités sous contrainte ≥ 90 Mds€
### (5) Positionnement idéologique × soutien (conv+souh)
### (6) Notes moyennes par mesure et groupe de votants (effect_program et budget)
### (7) Matrice de distances entre groupes de votants

source('.Rprofile')
load('.RData')

library(dplyr)
library(ggplot2)
library(ggrepel)

if (!exists("variables_budget_policies")) variables_budget_policies <- variables_budget

## ── Constantes ──────────────────────────────────────────────────────────────
THRESHOLD     <- 0.5   # seuil majorité
THRESHOLD_LOW <- 0.1  # seuil bas pour partie (2)
AMOUNT_TARGET <- 90    # Mds€

## ── Variables et montants ───────────────────────────────────────────────────
vars    <- variables_budget_policies
short   <- sub("budget_", "", vars)
m       <- length(vars)
amounts <- budget_policies$amount[match(vars, budget_policies$variable_name)]
pkg_amount <- function(cols) sum(amounts[cols], na.rm = TRUE)

## ── Fonctions de support binaire (1=soutien, 0=rejet, NA=NSP exclus) ────────
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
joint_support <- function(cols, mat, wgt = e$weight) {
  sub   <- mat[, cols, drop = FALSE]
  zeros <- rowSums(sub == 0L, na.rm = TRUE)
  nas   <- rowSums(is.na(sub))
  joint <- ifelse(zeros > 0, 0L, ifelse(nas > 0, NA_integer_, 1L))
  weighted.mean(joint, wgt, na.rm = TRUE)
}

## ── Apriori générique ─────────────────────────────────────────────────────────
# Retourne $all_feasible : liste de vecteurs d'indices dans vars (1:m)
run_apriori <- function(mat, threshold = THRESHOLD, wgt = e$weight, label = "") {
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
report_best_economy <- function(res, mat, wgt = e$weight, label = "") {
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
  wgt_g <- ifelse(mask, e$weight, 0)
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
  wgt_g <- ifelse(mask, e$weight, 0)
  cat(sprintf("\n── Paire : %s (n=%d) ──\n", gname, sum(mask)))
  rg <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = gname)
  report_best_economy(rg, mat_SCS, wgt = wgt_g, label = gname)
}

## ═══════════════════════════════════════════════════════════════════════════
## (4) Paquet maximisant la somme des utilités sous contrainte ≥ 90 Mds€
##     Utilité d'une mesure = moyenne pondérée des valeurs (-1/0/1/2)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n\n══════════════════════════════════════════════════════════\n")
cat("(4) Paquet maximisant l'utilité totale sous contrainte ≥ 90 Mds€\n")

util_val <- function(x) case_when(
  x == "Souhaitable"  ~  2,
  x == "Convenable"   ~  1,
  x == "Supportable"  ~  0,
  x == "Inacceptable" ~ -1,
  TRUE ~ NA_real_
)
u_mean <- sapply(vars, function(v) weighted.mean(util_val(e[[v]]), e$weight, na.rm = TRUE))
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
  weighted.mean(e$vote_agg[mask] - 1, e$weight[mask])
})

# y = taux de soutien conv+souh (tous répondants, pondéré, xPNR)
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
    y     = "Taux de soutien conv+souh (%, pondéré, xPNR)",
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

wt_mean_se <- function(x, w) {
  ok <- !is.na(x) & w > 0
  xv <- x[ok]; wv <- w[ok]
  if (length(xv) < 2) return(c(mean = mean(xv, na.rm = TRUE), se = NA_real_))
  mu  <- weighted.mean(xv, wv)
  n_e <- sum(wv)^2 / sum(wv^2)    # taille effective
  wvar <- sum(wv * (xv - mu)^2) / sum(wv)
  c(mean = mu, se = sqrt(wvar / n_e))
}

group_defs <- list(
  "Overall"       = rep(TRUE, nrow(e)),
  "Left"          = !is.na(e$vote_agg) & e$vote_agg == 0,
  "Center-right"  = !is.na(e$vote_agg) & e$vote_agg == 1,
  "Far right"     = !is.na(e$vote_agg) & e$vote_agg == 2
)

compute_stats <- function(variables, score_fn) {
  rows <- lapply(variables, function(v) {
    sc <- score_fn(e[[v]])
    vname <- sub("^budget_|^effect_program_", "", v)
    lapply(names(group_defs), function(gname) {
      w_g <- e$weight * ifelse(group_defs[[gname]], 1, 0)
      ms  <- wt_mean_se(sc, w_g)
      data.frame(measure = vname, group = gname,
                 mean = ms["mean"], se = ms["se"],
                 stringsAsFactors = FALSE, row.names = NULL)
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

df_ep  <- compute_stats(variables_effect_program,  ep_score)
df_bud <- compute_stats(variables_budget_policies, bud_score)

plot_lines <- function(df, title, xlab, xlim_range) {
  df$group <- factor(df$group, levels = c("Overall","Left","Center-right","Far right"))
  ggplot(df, aes(y = measure, x = mean, group = measure, color = group)) +
    geom_line(alpha = 0.5, linewidth = 0.6) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = mean - se, xmax = mean + se),
                   height = 0.3, alpha = 0.5, linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
    coord_cartesian(xlim = xlim_range) +
    labs(y = NULL, x = xlab, title = title,
         color = "Groupe", caption = "Barres = ±1 SE (erreur standard de la moyenne pondérée)") +
    theme_bw(base_size = 10) +
    theme(legend.position = "top")
}

p6_ep  <- plot_lines(df_ep,  "effect_program : notes moyennes par groupe de votants",
                     "Note moyenne (−2 à +2)", c(-2, 2))
p6_bud <- plot_lines(df_bud, "budget : notes moyennes par groupe de votants",
                     "Note moyenne (−1 à +2)", c(-1, 2))

ggsave("../figures/notes_groupes_effect_program.pdf", p6_ep,  width = 12, height = 8)
ggsave("../figures/notes_groupes_budget.pdf",         p6_bud, width = 12, height = 10)
cat("  → ../figures/notes_groupes_effect_program.pdf\n")
cat("  → ../figures/notes_groupes_budget.pdf\n")

## ═══════════════════════════════════════════════════════════════════════════
## (7) Matrice de distances entre groupes de votants
##     Distance = somme des |écarts| de notes moyennes sur toutes les mesures
##     (effect_program + budget, scales naturelles : −2/+2 et −1/+2)
## ═══════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════\n")
cat("(7) Matrice de distances entre groupes de votants\n")

dist_groups <- list(
  "Overall"      = rep(TRUE, nrow(e)),
  "Left"         = !is.na(e$vote_agg) & e$vote_agg == 0,
  "Center-right" = !is.na(e$vote_agg) & e$vote_agg == 1,
  "Far right"    = !is.na(e$vote_agg) & e$vote_agg == 2
)

group_mean_vec <- function(variables, score_fn) {
  # Retourne matrice : mesures × groupes
  sapply(names(dist_groups), function(gname) {
    w_g <- e$weight * ifelse(dist_groups[[gname]], 1, 0)
    sapply(variables, function(v) weighted.mean(score_fn(e[[v]]), w_g, na.rm = TRUE))
  })
}

means_ep  <- group_mean_vec(variables_effect_program,  ep_score)   # (n_ep)  × 4
means_bud <- group_mean_vec(variables_budget_policies, bud_score)  # (n_bud) × 4
means_all <- rbind(means_ep, means_bud)                            # all measures × 4

gnames  <- names(dist_groups)
ng      <- length(gnames)
dist_mat <- matrix(0, ng, ng, dimnames = list(gnames, gnames))
for (i in seq_len(ng))
  for (j in seq_len(ng))
    if (i != j) dist_mat[i, j] <- sum(abs(means_all[, i] - means_all[, j]), na.rm = TRUE)

cat("\nMatrice de distances (∑|Δ note| sur toutes les mesures effect_program + budget) :\n")
print(round(dist_mat, 3))

cat("\nTerminé.\n")
