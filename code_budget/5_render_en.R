##### 5_render_en.R — English versions of all figures in budget_en.tex #####
# Run from code_budget/ (setwd if needed).
# Outputs: ../figures/*_en.pdf (12 figures)

source('.Rprofile')
load('.RData')
e$no_weight <- 1

library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(corrplot)
library(Hmisc)

## ── English label dictionaries ──────────────────────────────────────────────

labels_budget_en <- c(
  aligner_tva_restauration                        = "Align restaurant VAT with standard rate",
  augmenter_age_retraite_65                       = "Raise statutory retirement age to 65",
  augmenter_cotisations_salaires_moyens           = "Raise payroll contributions on middle wages",
  augmenter_csg_1pt                               = "Raise CSG social contributions (+1 pp)",
  augmenter_duree_travail_droit_chomage           = "Extend work period for unemployment benefits",
  augmenter_impot_heritages_eleves                = "Tax large inheritances more heavily",
  augmenter_impot_revenu_aises                    = "Raise income tax on high earners",
  augmenter_impot_revenu_tous                     = "Raise income tax (all households)",
  augmenter_impot_societes                        = "Raise corporate tax",
  augmenter_taxe_revenus_capital                  = "Raise capital income tax",
  augmenter_tva_1pt                               = "Raise VAT (+1 pp)",
  diminuer_credit_impot_recherche                 = "Reduce R&D tax credit",
  diminuer_subventions_ecole_privee               = "Reduce private school subsidies",
  eliminer_doublons_territoriaux                  = "Eliminate territorial overlap in local governance",
  geler_aides_sociales                            = "Freeze welfare benefits",
  geler_depenses_etat_collectivites               = "Freeze public spending (state & local)",
  reduire_aides_apprentissage                     = "Reduce apprenticeship subsidies",
  reduire_depenses_educatives_demographie         = "Reduce education spending (demographics)",
  reduire_depenses_militaires                     = "Reduce military spending",
  reduire_pensions_retraite                       = "Reduce pension payments",
  reduire_remboursement_soins                     = "Reduce healthcare reimbursements",
  restaurer_taxe_habitation_aises                 = "Restore housing tax for high earners",
  retablir_isf                                    = "Restore the ISF (wealth tax)",
  retirer_aides_sociales_etrangers                = "Withdraw welfare benefits for non-citizens",
  soumettre_livret_a_impot                        = "Subject Livret A (savings) interest to income tax",
  supprimer_abattement_ir_retraites               = "Remove income tax allowance for retirees",
  supprimer_ame                                   = "Abolish State Medical Aid for foreigners (AME)",
  supprimer_avantages_fiscaux_complements_salaire = "Tax wage supplements (end fiscal exemptions)",
  supprimer_exonerations_taxes_carburants         = "End sectoral fuel tax exemptions",
  tva_luxe                                        = "Apply higher VAT to luxury goods"
)

labels_effect_program_en <- c(
  reduire_aide_developpement      = "Reduce development aid",
  taxe_millionaires_onu           = "UN tax on millionaires",
  fin_dutreil                     = "End Dutreil inheritance tax break",
  education_sante                 = "Increase education & health budgets",
  augmenter_allocs_familiales     = "Increase family allowances",
  reduire_deficit                 = "Reduce the public deficit",
  reduire_depenses_fonctionnement = "Reduce public operating expenditures",
  restreindre_aides_etrangers     = "Restrict welfare for non-citizens",
  appliquer_oqtf                  = "Enforce deportation orders (OQTFs)",
  regulariser_sans_papiers        = "Regularize undocumented migrants",
  peines_planchers_recidive       = "Mandatory minimum sentences",
  retraite_65_ans                 = "Raise statutory retirement age to 65",
  retraite_62_ans                 = "Lower retirement age to 62",
  augmenter_smic                  = "Raise minimum wage (SMIC)",
  ric                             = "Citizens' initiative referendum (RIC)",
  proportionnelle                 = "Proportional representation",
  maintenir_green_deal            = "Maintain the Green Deal"
)

det_labels_en <- c(
  "no.na(vote_agg)"          = "Political bloc",
  "man"                      = "Gender",
  "age_factor"               = "Age",
  "income_factor"            = "Income level",
  "urbanity_factor"          = "Urbanity",
  "as.factor(region)"        = "Region",
  "as.factor(education)"     = "Education",
  "no.na(wealth_quartile_5)" = "Wealth",
  "employment_agg"           = "Employment status",
  "Nb_children__14"          = "No. children < 14",
  "hh_size"                  = "Household size",
  "voted"                    = "Voted (EU elections)"
)

group_labels_en <- c(
  "Overall"                  = "Overall",
  "Left"                     = "Left",
  "Center-right"             = "Centre + LR",
  "Far right"                = "Far right",
  "Left + Far right"         = "Left + Far right",
  "Center-right + Far right" = "Centre + LR + Far right",
  "Center-right + Left"      = "Left + Centre + LR",
  "LFI"                      = "LFI",
  "LFI_EELV_PCF"             = "LFI + LÉ + PCF",
  "EELV"                     = "LÉ",
  "EELV_PS_centre"           = "LÉ + PS + C",
  "centre"                   = "C",
  "PS_centre"                = "PS + C",
  "PS"                       = "PS",
  "PS_centre_LR"             = "PS + C + LR",
  "EELV_PS_centre_LR"        = "LÉ + PS + C + LR",
  "LR"                       = "LR",
  "LR_RN_Reconquete"         = "LR + RN + Reconq.",
  "centre_LR_RN_Reconquete"  = "C + LR + Far right"
)

group_labels_short_en <- c(
  "Overall"                  = "All",
  "Left"                     = "L",
  "Center-right"             = "C+LR",
  "Far right"                = "FR",
  "Left + Far right"         = "L+FR",
  "Center-right + Far right" = "C+LR+FR",
  "Center-right + Left"      = "L+C+LR",
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
  "LR_RN_Reconquete"         = "LR+RN+Rec.",
  "centre_LR_RN_Reconquete"  = "C+LR+FR"
)

# French → English cluster name mapping (for clusters_combined figure)
fr_to_en_clusters <- c(
  "Progressistes"       = "Progressives",
  "Conservateurs"       = "Conservatives",
  "Sociaux-nativistes"  = "Social-nativists",
  "Frugaux"             = "Deficit hawks",
  "Libéraux-nativistes" = "Liberal-nativists",
  "Sociaux"             = "Social",
  "Libéraux-frugaux"    = "Liberal-frugal",
  "Nativistes"          = "Nativists",
  "Centre"              = "Centre",
  "Ensemble"            = "Overall"
)
fr_to_en_vote <- c(
  "Gauche"               = "Left",
  "Centre-droit/Droite"  = "Centre-right/Right",
  "Extrême-droite"       = "Far right",
  "Non-réponse/Autre"    = "Non-response/Other"
)

## ── Load 3_paquets_majoritaires.R setup (scoring fns, matrices, run_apriori, coalition_defs) ──
{
  l3         <- readLines('3_paquets_majoritaires.R')
  setup_end3 <- which(grepl("^## \\(1\\)  supp", l3))[1] - 1
  eval(parse(text = paste(l3[1:setup_end3], collapse = '\n')))
  cat("3_paquets setup done.\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 1. Bar charts: effect_program_en.pdf and budget_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  short_ep  <- sub("effect_program_", "", variables_effect_program)
  short_bud <- sub("budget_", "", variables_budget)

  ep_labels <- break_strings(unname(labels_effect_program_en[short_ep]), 57)
  bd_ep_en <- list(
    "effect_program" = list(
      name       = "effect_program",
      vars       = variables_effect_program,
      labels     = ep_labels,
      legend     = c("Much less favorable", "Less favorable", "No change", "More favorable", "Much more favorable"),
      miss       = FALSE,
      sort       = TRUE,
      rev        = FALSE,
      rev_color  = TRUE,
      fr         = FALSE,
      title      = "",
      showLegend = TRUE,
      thin       = TRUE,
      width      = 980,
      height     = fig_height(nb_bars = length(ep_labels), large = any(grepl("<br>", ep_labels)))
    )
  )
  barres_multiple(bd_ep_en, append_name = "_en")
  cat("→ ../figures/effect_program_en.pdf\n")

  bud_labels <- break_strings(
    paste0(labels_budget_en[short_bud], ": ",
           as.character(budget_policies_amounts[variables_budget]), " €bn"), 64)
  bd_bud_en <- list(
    "budget" = list(
      name       = "budget",
      vars       = variables_budget,
      labels     = bud_labels,
      legend     = c("Unacceptable", "Tolerable", "Appropriate", "Desirable", "Don't know"),
      miss       = TRUE,
      sort       = TRUE,
      rev        = FALSE,
      rev_color  = TRUE,
      fr         = FALSE,
      title      = "",
      showLegend = TRUE,
      thin       = TRUE,
      width      = 1100,
      height     = 1500
    )
  )
  barres_multiple(bd_bud_en, weights = FALSE, append_name = "_en")
  cat("→ ../figures/budget_en.pdf\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 2. Notes by group: notes_groupes_budget_en.pdf + notes_groupes_effect_program_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  # ep_score and bud_score already defined by 3_paquets setup above

  wt_mean_mad_asym <- function(x, w) {
    ok <- !is.na(x) & w > 0
    xv <- x[ok]; wv <- w[ok]
    if (length(xv) < 2) return(c(mean = mean(xv, na.rm = TRUE),
                                  mad_lo = NA_real_, mad_hi = NA_real_))
    mu     <- weighted.mean(xv, wv)
    below  <- xv < mu; above <- xv > mu
    mad_lo <- if (any(below)) sum(wv[below] * (mu - xv[below])) / sum(wv[below]) else 0
    mad_hi <- if (any(above)) sum(wv[above] * (xv[above] - mu)) / sum(wv[above]) else 0
    c(mean = mu, mad_lo = mad_lo, mad_hi = mad_hi)
  }

  group_defs <- list(
    "Overall"      = rep(TRUE, nrow(e)),
    "Left"         = !is.na(e$vote_agg) & e$vote_agg == 0,
    "Center-right" = !is.na(e$vote_agg) & e$vote_agg == 1,
    "Far right"    = !is.na(e$vote_agg) & e$vote_agg == 2
  )

  colors_en <- c(
    "Overall"      = "black",
    "Left"         = "#F8766D",
    "Centre-right" = "#619CFF",
    "Far right"    = "#A020F0"
  )

  compute_stats_en <- function(variables, score_fn, label_map) {
    rows <- lapply(variables, function(v) {
      sc   <- score_fn(e[[v]])
      key  <- sub("^budget_|^effect_program_", "", v)
      vlbl <- if (key %in% names(label_map)) unname(label_map[key]) else gsub("_", " ", key)
      lapply(names(group_defs), function(gname) {
        w_g <- e$no_weight * ifelse(group_defs[[gname]], 1, 0)
        ms  <- wt_mean_mad_asym(sc, w_g)
        data.frame(measure = vlbl, group = gname,
                   mean = ms["mean"], mad_lo = ms["mad_lo"], mad_hi = ms["mad_hi"],
                   stringsAsFactors = FALSE, row.names = NULL)
      })
    })
    do.call(rbind, unlist(rows, recursive = FALSE))
  }

  df_ep_en  <- compute_stats_en(variables_effect_program, ep_score,  labels_effect_program_en)
  df_bud_en <- compute_stats_en(variables_budget,         bud_score, labels_budget_en)

  plot_lines_en <- function(df, show_vline = TRUE, x_breaks = waiver(), x_labels = waiver()) {
    order_df   <- df[df$group == "Overall", ]
    order_df   <- order_df[order(order_df$mean), ]
    df$measure <- factor(df$measure, levels = order_df$measure)
    # Map internal group names to display labels
    lvl_map <- c("Overall" = "Overall", "Left" = "Left",
                 "Center-right" = "Centre-right", "Far right" = "Far right")
    df$group_en <- factor(lvl_map[df$group], levels = unname(lvl_map))
    dodge    <- position_dodge(width = 0.7)
    n_items  <- nlevels(df$measure)
    minor_y  <- seq(0.5, n_items - 0.5, by = 1)
    xmin     <- min(df$mean - df$mad_lo, na.rm = TRUE)
    xmax     <- max(df$mean + df$mad_hi, na.rm = TRUE)
    pad      <- (xmax - xmin) * 0.03
    xlim_r   <- c(xmin - pad, xmax + pad)
    p <- ggplot(df, aes(y = measure, x = mean, color = group_en, group = group_en)) +
      geom_hline(yintercept = minor_y, color = "grey85", linewidth = 0.3) +
      geom_errorbarh(aes(xmin = mean - mad_lo, xmax = mean + mad_hi),
                     height = 0, alpha = 0.7, linewidth = 0.25, position = dodge) +
      geom_point(size = 2.1, position = dodge) +
      scale_color_manual(values = colors_en, drop = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels) +
      coord_cartesian(xlim = xlim_r) +
      labs(y = NULL, x = NULL, color = "Group") +
      theme_bw(base_size = 10) +
      theme(
        legend.position     = "top",
        legend.justification = c(1, 0),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x  = element_line(color = "grey90", linewidth = 0.3),
        axis.text           = element_text(color = "black"),
        axis.title          = element_text(color = "black"),
        legend.text         = element_text(color = "black"),
        legend.title        = element_text(color = "black"),
        plot.margin         = margin(t = 5, r = 18, b = 5, l = 5)
      ) +
      scale_y_discrete(expand = expansion(add = 0.5))
    if (show_vline && xmin < 0 && xmax > 0)
      p <- p + geom_vline(xintercept = 0, linetype = "dotted", color = "grey40")
    p
  }

  p_ep_en <- plot_lines_en(
    df_ep_en, show_vline = TRUE,
    x_breaks = c(-2, -1, 0, 1, 2),
    x_labels = c("Much less\nfavorable", "Less\nfavorable", "No\nchange",
                 "More\nfavorable", "Much more\nfavorable"))
  p_bud_en <- plot_lines_en(
    df_bud_en, show_vline = FALSE,
    x_breaks = c(-1, 0, 1, 2),
    x_labels = c("Unacceptable", "Tolerable", "Appropriate", "Desirable"))

  ggsave("../figures/notes_groupes_effect_program_en.pdf", p_ep_en,  width = 5.5, height = 5)
  ggsave("../figures/notes_groupes_budget_en.pdf",         p_bud_en, width = 5.5, height = 7)
  cat("→ ../figures/notes_groupes_effect_program_en.pdf\n")
  cat("→ ../figures/notes_groupes_budget_en.pdf\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 3. LMG variance decomposition: lmg_attitudes_en.pdf + r2_iso_attitudes_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  library(relaimpo)

  # budget_accept and ep_score (binary versions for regression)
  budget_accept <- as.data.frame(sapply(variables_budget, function(v)
    ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1L,
           ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0L, NA_integer_))))

  ep_num <- function(x) case_when(
    x == "Beaucoup plus favorable"  ~  2,
    x == "Plus favorable"           ~  1,
    x == "Ne changerait rien"       ~  0,
    x == "Moins favorable"          ~ -1,
    x == "Beaucoup moins favorable" ~ -2,
    TRUE ~ NA_real_)

  program_favorable <- as.data.frame(sapply(
    unname(variables_effect_program), function(v) e[[v]] > 0))

  determinants <- c(
    "no.na(vote_agg)", "man", "age_factor", "income_factor",
    "urbanity_factor", "as.factor(region)", "as.factor(education)",
    "no.na(wealth_quartile_5)", "employment_agg", "Nb_children__14",
    "hh_size", "voted")

  fit_decomp <- function(y, df = e, det = determinants) {
    mod <- tryCatch(
      lm(as.formula(paste("as.numeric(", y, ") ~", paste(det, collapse = '+'))),
         data = df, weights = no_weight),
      error = function(e) NULL)
    if (is.null(mod)) return(NULL)
    s   <- summary(mod)$coefficients
    sig <- s[, 4] < 0.05
    sig_by_var <- setNames(sapply(det, function(d) {
      m <- startsWith(rownames(s), d)
      if (!any(m)) FALSE else any(sig[m], na.rm = TRUE)
    }), det)
    lmg <- if (length(det) == 1) {
      setNames(summary(mod)$r.squared, det)
    } else {
      tryCatch(calc.relimp(mod, type = "lmg", rela = FALSE, rank = FALSE)@lmg,
               error = function(e) setNames(rep(NA_real_, length(det)), det))
    }
    list(sig = sig, sig_by_var = sig_by_var, lmg = lmg, R2 = summary(mod)$r.squared)
  }

  collect_lmg_iso <- function(vars_set) {
    lmg_mat <- matrix(NA_real_, nrow = length(determinants), ncol = length(vars_set),
                      dimnames = list(determinants, vars_set))
    iso_mat  <- lmg_mat
    for (v in vars_set) {
      res_full <- fit_decomp(v, det = determinants)
      if (!is.null(res_full)) {
        shared <- intersect(determinants, names(res_full$lmg))
        lmg_mat[shared, v] <- res_full$lmg[shared]
      }
      for (d in determinants) {
        res_iso <- fit_decomp(v, det = d)
        if (!is.null(res_iso)) iso_mat[d, v] <- res_iso$R2
      }
    }
    list(avg_lmg = rowMeans(lmg_mat, na.rm = TRUE) * 100,
         avg_iso = rowMeans(iso_mat,  na.rm = TRUE) * 100)
  }

  bar_fig_en <- function(df_vals, tag, xlab) {
    df_vals[!is.finite(df_vals)] <- 0
    df <- data.frame(
      label = factor(det_labels_en[determinants],
                     levels = det_labels_en[determinants[order(df_vals)]]),
      value = df_vals, stringsAsFactors = FALSE)
    p <- ggplot(df, aes(x = value, y = label)) +
      geom_col(fill = "#2c6fad", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", value)),
                hjust = -0.1, size = 2.4, color = "black") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                         labels = function(x) paste0(x, "%")) +
      labs(x = xlab, y = NULL) +
      theme_bw(base_size = 8) +
      theme(panel.grid.major.y  = element_blank(),
            panel.grid.minor    = element_blank(),
            panel.border        = element_blank(),
            axis.line           = element_line(color = "black", linewidth = 0.3),
            axis.ticks          = element_line(color = "black", linewidth = 0.3),
            axis.text           = element_text(color = "black"),
            axis.title          = element_text(color = "black"),
            plot.margin         = margin(t = 3, r = 12, b = 3, l = 3))
    ggsave(sprintf("../figures/%s.pdf", tag), p,
           width = 2.5, height = 1.9, device = cairo_pdf)
    cat("→ ../figures/", tag, ".pdf\n", sep = "")
  }

  cat("Computing LMG/R² (attitudes)...\n")
  attitudes <- c(variables_budget, variables_effect_program, "sum_convenable", "sum_souhaitable")
  res_att   <- collect_lmg_iso(attitudes)
  bar_fig_en(res_att$avg_lmg, "lmg_attitudes_en",
             "Average share of variance explained (%)")
  bar_fig_en(res_att$avg_iso, "r2_iso_attitudes_en",
             "Average R² (%)")
}

## ══════════════════════════════════════════════════════════════════════════════
## 4. Distance matrices: distance_matrix_pairwise_en.pdf + distance_matrix_means_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  # dist_groups and coalition_defs are already defined by 3_paquets setup
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
    lapply(coalition_defs, function(parties)
      !is.na(e$vote_original) & e$vote_original %in% parties)
  )

  gnames <- names(dist_groups)
  ng     <- length(gnames)

  # Compute group mean vectors
  group_mean_vec <- function(variables, score_fn) {
    sapply(names(dist_groups), function(gname) {
      w_g <- e$no_weight * ifelse(dist_groups[[gname]], 1, 0)
      sapply(variables, function(v) weighted.mean(score_fn(e[[v]]), w_g, na.rm = TRUE))
    })
  }

  means_ep  <- group_mean_vec(variables_effect_program, ep_score)
  means_bud <- group_mean_vec(variables_budget, bud_score)
  means_all <- rbind(means_ep, means_bud)

  dist_mat <- matrix(0, ng, ng, dimnames = list(gnames, gnames))
  for (i in seq_len(ng))
    for (j in seq_len(ng))
      if (i != j) dist_mat[i, j] <- sum(abs(means_all[, i] - means_all[, j]), na.rm = TRUE)

  # Pairwise inter-individual distances
  score_list <- c(
    lapply(variables_effect_program, function(v) ep_score(e[[v]])),
    lapply(variables_budget,         function(v) bud_score(e[[v]]))
  )

  compute_pairwise_dist <- function(scores) {
    mean_dist_v <- sapply(scores, function(x) {
      ok <- !is.na(x) & e$no_weight > 0
      if (sum(ok) < 2) return(0)
      xv <- x[ok]; wv <- e$no_weight[ok]
      tab <- tapply(wv, xv, sum)
      vs  <- as.numeric(names(tab)); p <- as.numeric(tab) / sum(tab)
      sum(outer(vs, vs, function(a, b) abs(a - b)) * outer(p, p))
    })
    gd <- lapply(dist_groups, function(mask) {
      lapply(scores, function(x) {
        ok     <- mask & e$no_weight > 0
        tot_w  <- sum(e$no_weight[ok])
        if (tot_w == 0) return(list(v = numeric(0), p = numeric(0), p_nsp = 0))
        nsp    <- ok & is.na(x); non <- ok & !is.na(x)
        p_nsp  <- sum(e$no_weight[nsp]) / tot_w
        if (!any(non)) return(list(v = numeric(0), p = numeric(0), p_nsp = p_nsp))
        xv <- x[non]; wv <- e$no_weight[non]
        tab <- tapply(wv, xv, sum)
        list(v = as.numeric(names(tab)), p = as.numeric(tab) / tot_w, p_nsp = p_nsp)
      })
    })
    m <- matrix(0, ng, ng, dimnames = list(gnames, gnames))
    for (i in seq_len(ng)) for (j in seq(i, ng)) {
      d <- 0
      for (k in seq_along(scores)) {
        dA    <- gd[[i]][[k]]; dB <- gd[[j]][[k]]
        cross <- if (length(dA$v) > 0 && length(dB$v) > 0)
          sum(outer(dA$v, dB$v, function(a, b) abs(a - b)) * outer(dA$p, dB$p)) else 0
        nsp_share <- dA$p_nsp + dB$p_nsp - dA$p_nsp * dB$p_nsp
        d <- d + cross + nsp_share * mean_dist_v[k]
      }
      m[i, j] <- d; m[j, i] <- d
    }
    m
  }

  dist_mat_indiv <- compute_pairwise_dist(score_list)
  ref_dist       <- dist_mat_indiv["Overall", "Overall"]
  dist_mat_norm       <- (dist_mat       / ref_dist - 1) * 100
  dist_mat_indiv_norm <- (dist_mat_indiv / ref_dist - 1) * 100

  display_order <- c(
    "Overall",
    "Left", "Center-right", "Far right",
    "Center-right + Left", "Center-right + Far right", "Left + Far right",
    "LFI", "LFI_EELV_PCF", "EELV", "PS",
    "EELV_PS_centre", "PS_centre", "centre",
    "PS_centre_LR", "EELV_PS_centre_LR",
    "LR", "LR_RN_Reconquete"
  )

  plot_dist_heatmap_en <- function(mat, outfile, normalized = TRUE) {
    fill_name <- if (normalized) "Deviation vs\noverall" else "Distance"
    ord       <- display_order[display_order %in% rownames(mat)]
    mat       <- mat[ord, ord, drop = FALSE]
    rn_en     <- group_labels_en[rownames(mat)]
    cn_short  <- group_labels_short_en[colnames(mat)]
    df        <- as.data.frame(as.table(mat))
    names(df) <- c("A", "B", "dist")
    df$A      <- factor(group_labels_en[as.character(df$A)],    levels = rev(rn_en))
    df$B      <- factor(group_labels_short_en[as.character(df$B)], levels = cn_short)
    nr        <- length(levels(df$A)); nc <- length(levels(df$B))
    text_thresh <- if (normalized) max(abs(range(df$dist, na.rm = TRUE))) * 0.55
                   else max(df$dist, na.rm = TRUE) * 0.6
    fill_dark   <- if (normalized) abs(df$dist) > text_thresh else df$dist > text_thresh
    legend_fmt  <- if (normalized) function(x) sprintf("%+.0f%%", x) else function(x) sprintf("%.1f", x)
    p <- ggplot(df, aes(x = B, y = A, fill = dist)) +
      geom_tile(color = "white", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.1f", dist), color = fill_dark),
                size = 1.9, show.legend = FALSE) +
      scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"))
    if (normalized) {
      p <- p + scale_fill_gradient2(low = "#2b6cb0", mid = "white", high = "#c53030",
                                    midpoint = 0, name = fill_name, labels = legend_fmt)
    } else {
      p <- p + scale_fill_gradient(low = "white", high = "#c53030",
                                   name = fill_name, labels = legend_fmt)
    }
    p <- p +
      coord_fixed(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 8) +
      theme(
        axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1,
                                          size = 6.5, color = "black"),
        axis.text.y        = element_text(size = 6.5, color = "black"),
        panel.grid         = element_blank(),
        legend.position    = "right",
        legend.key.height  = grid::unit(0.8, "cm"),
        legend.text        = element_text(color = "black"),
        legend.title       = element_text(color = "black"),
        plot.margin        = margin(t = 25, r = 5, b = 5, l = 5)
      ) +
      ggplot2::annotate("text",
               x = seq_len(nc), y = nr + 0.85,
               label = levels(df$B),
               angle = 45, hjust = 0, vjust = 0.5, size = 2.1, color = "black")
    ggsave(outfile, p, width = 6.5, height = 5.5)
    cat("→", outfile, "\n")
  }

  plot_dist_heatmap_en(round(dist_mat_norm),       "../figures/distance_matrix_means_en.pdf")
  plot_dist_heatmap_en(dist_mat_indiv_norm,         "../figures/distance_matrix_pairwise_en.pdf")
}

## ══════════════════════════════════════════════════════════════════════════════
## 5. Coalition packages matrix: coalition_packages_matrix_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  vote_bloc_masks <- list(
    "Overall"      = rep(TRUE, nrow(e)),
    "Left"         = !is.na(e$vote_agg) & e$vote_agg == 0,
    "Center-right" = !is.na(e$vote_agg) & e$vote_agg == 1,
    "Far right"    = !is.na(e$vote_agg) & e$vote_agg == 2
  )
  party_coal_keys <- c("EELV_PS_centre", "PS_centre", "PS_centre_LR",
                       "centre_LR_RN_Reconquete", "LR_RN_Reconquete",
                       "LFI", "EELV", "PS", "centre", "LR")
  coal_masks <- c(
    vote_bloc_masks,
    setNames(lapply(party_coal_keys, function(cn)
               !is.na(e$vote_original) & e$vote_original %in% coalition_defs[[cn]]),
             party_coal_keys)
  )

  best_by_savings <- function(mask) {
    wgt_g <- ifelse(mask, e$no_weight, 0)
    capture.output(feas <- run_apriori(mat_SCS, THRESHOLD, wgt = wgt_g, label = "")$all_feasible)
    if (!length(feas)) return(integer(0))
    feas[[which.max(sapply(feas, function(p) sum(amounts[p], na.rm = TRUE)))]]
  }

  cat("\n=== Coalition packages (English figure) ===\n")
  pkg_res <- setNames(lapply(names(coal_masks), function(cn) {
    bp  <- best_by_savings(coal_masks[[cn]])
    amt <- sum(amounts[bp], na.rm = TRUE)
    cat(sprintf("  %-22s: %d measures | %.1f €bn\n", cn, length(bp), amt))
    list(vnames = vars[bp], savings = amt)
  }), names(coal_masks))

  vars_f   <- intersect(vars, unique(unlist(lapply(pkg_res, `[[`, "vnames"))))
  short_f  <- sub("budget_", "", vars_f)
  pol_amt  <- amounts[match(vars_f, vars)]

  pkg_count <- sapply(vars_f, function(v)
    sum(sapply(pkg_res, function(p) v %in% p$vnames)))

  pol_lbl_en <- setNames(
    paste0(labels_budget_en[short_f], " (",
           gsub("\\.", ",", sprintf("%.1f", pol_amt)), " €bn)"),
    vars_f
  )
  pol_levs_en <- c(pol_lbl_en[vars_f[order(pkg_count)]],
                   "Support for Overall package (%)", "Savings (€ bn)")

  col_levs    <- names(coal_masks)
  col_disp_en <- group_labels_en[col_levs]
  col_disp_en["LR_RN_Reconquete"]        <- "LR + Far right"
  col_disp_en["centre_LR_RN_Reconquete"] <- "C + LR + Far right"
  savings_vec <- sapply(pkg_res, `[[`, "savings")

  overall_pkg_idx <- match(pkg_res$Overall$vnames, vars)
  support_ens_pct <- sapply(names(coal_masks), function(cn) {
    mask  <- coal_masks[[cn]]
    wgt_g <- ifelse(mask, e$no_weight, 0)
    joint_support(overall_pkg_idx, mat_SCS, wgt_g) * 100
  })

  blue_pal <- colorRampPalette(c("#ffffff", "#1f3a93"))
  sav_keys <- paste0("sav_", seq_along(savings_vec))
  sup_keys <- paste0("sup_", seq_along(support_ens_pct))
  sav_hex  <- blue_pal(100)[pmin(100, pmax(1, round(savings_vec / 120 * 99) + 1))]
  sup_hex  <- blue_pal(100)[pmin(100, pmax(1, round(support_ens_pct / 100 * 99) + 1))]

  df_tile <- expand.grid(policy = vars_f, coalition = col_levs, stringsAsFactors = FALSE)
  df_tile$in_pkg   <- mapply(function(p, c) p %in% pkg_res[[c]]$vnames,
                              df_tile$policy, df_tile$coalition)
  df_tile$pol_lbl  <- factor(pol_lbl_en[df_tile$policy],          levels = pol_levs_en)
  df_tile$col_disp <- factor(col_disp_en[df_tile$coalition],      levels = col_disp_en)
  df_tile$fill_cat <- ifelse(df_tile$in_pkg, "in_pkg", "out_pkg")

  df_sav <- data.frame(
    col_disp = factor(col_disp_en, levels = col_disp_en),
    pol_lbl  = factor("Savings (€ bn)", levels = pol_levs_en),
    lbl_txt  = sprintf("%.1f", savings_vec),
    fill_cat = sav_keys, txt_col = "black", stringsAsFactors = FALSE)

  df_sup <- data.frame(
    col_disp = factor(col_disp_en, levels = col_disp_en),
    pol_lbl  = factor("Support for Overall package (%)", levels = pol_levs_en),
    lbl_txt  = sprintf("%.0f", support_ens_pct),
    fill_cat = sup_keys,
    txt_col  = ifelse(support_ens_pct > 50, "white", "black"),
    stringsAsFactors = FALSE)

  fill_vals <- c(in_pkg  = "#2c6fad", out_pkg = "grey92",
                 setNames(sav_hex, sav_keys), setNames(sup_hex, sup_keys))

  face_x_en <- ifelse(col_disp_en == col_disp_en["Overall"], "bold", "plain")
  face_y_en <- ifelse(pol_levs_en %in% c("Savings (€ bn)", "Support for Overall package (%)"),
                      "bold", "plain")

  p_coal_en <- ggplot() +
    geom_tile(data = rbind(df_tile[, c("col_disp","pol_lbl","fill_cat")],
                           df_sav[,  c("col_disp","pol_lbl","fill_cat")],
                           df_sup[,  c("col_disp","pol_lbl","fill_cat")]),
              aes(x = col_disp, y = pol_lbl, fill = fill_cat),
              color = "white", linewidth = 0.35, width = 0.92) +
    geom_text(data = rbind(df_sav, df_sup),
              aes(x = col_disp, y = pol_lbl, label = lbl_txt, color = I(txt_col)),
              size = 2.3, fontface = "bold") +
    geom_hline(yintercept = length(pol_levs_en) - 1.5,
               color = "grey45", linewidth = 0.5) +
    scale_fill_manual(
      values = fill_vals,
      breaks = c("in_pkg", "out_pkg"),
      labels = c(in_pkg = "In the package", out_pkg = "Not in the package"),
      name   = NULL) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 8.5) +
    theme(
      axis.text.x         = element_text(angle = 35, hjust = 0, size = 7.5,
                                         face = face_x_en, color = "black"),
      axis.text.y         = element_text(size = 7.5, face = face_y_en, color = "black"),
      legend.position     = "bottom",
      legend.text         = element_text(size = 8, color = "black"),
      panel.grid          = element_blank(),
      plot.margin         = margin(t = 5, r = 60, b = 5, l = 5))

  ggsave("../figures/coalition_packages_matrix_en.pdf", p_coal_en,
         width = 6.5, height = 6.5, device = cairo_pdf)
  cat("→ ../figures/coalition_packages_matrix_en.pdf\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 6. Coalition support heatmap: coalition_support_heatmap_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  h_defs_en <- list(
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
  h_lbl_en <- c(
    "Overall"                 = "Overall",
    "Left"                    = "Left",
    "Center-right"            = "Centre + LR",
    "Far right"               = "Far right",
    "EELV_PS_centre"          = "LÉ + PS + C",
    "PS_centre"               = "PS + C",
    "PS_centre_LR"            = "PS + C + LR",
    "centre_LR_RN_Reconquete" = "C + LR + Far right",
    "LR_RN_Reconquete"        = "LR + Far right",
    "LFI"                     = "LFI",
    "EELV"                    = "LÉ",
    "PS"                      = "PS",
    "centre"                  = "C",
    "LR"                      = "LR"
  )
  h_masks_en <- lapply(names(h_defs_en), function(cn) {
    if      (cn == "Overall")           rep(TRUE, nrow(e))
    else if (cn == "Left")              !is.na(e$vote_agg) & e$vote_agg == 0
    else if (cn == "Center-right")      !is.na(e$vote_agg) & e$vote_agg == 1
    else if (cn == "Far right")         !is.na(e$vote_agg) & e$vote_agg == 2
    else                                e[[cn]] == 1L & !is.na(e[[cn]])
  })
  names(h_masks_en) <- names(h_defs_en)

  ba_h <- as.data.frame(sapply(variables_budget, function(v)
    ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1L,
           ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0L, NA_integer_))))

  h_amt   <- setNames(budget_policies$amount[match(variables_budget, budget_policies$variable_name)],
                      variables_budget)
  h_short <- sub("budget_", "", variables_budget)

  cs_mat_en <- sapply(names(h_masks_en), function(cn) {
    wg <- ifelse(h_masks_en[[cn]], e$no_weight, 0)
    sapply(variables_budget, function(v) {
      y <- ba_h[[v]]; ok <- !is.na(y) & wg > 0
      if (!any(ok)) return(NA_real_)
      sum(y[ok] * wg[ok]) / sum(wg[ok])
    })
  })
  rownames(cs_mat_en) <- variables_budget

  row_ord_en  <- order(cs_mat_en[, "Overall"])
  pol_lbl_h   <- paste0(labels_budget_en[h_short], " (", sprintf("%.1f", h_amt), " €bn)")
  names(pol_lbl_h) <- variables_budget

  med_metrics_en <- c(
    "Median Desirable (€bn)"   = "sum_souhaitable",
    "Median Appropriate (€bn)" = "sum_convenable",
    "Median Acceptable (€bn)"  = "sum_supportable")
  m_mat_en <- sapply(names(h_masks_en), function(cn) {
    sapply(med_metrics_en, function(v) {
      ok <- h_masks_en[[cn]] & !is.na(e[[v]]) & !is.na(e$no_weight) & e$no_weight > 0
      if (!any(ok)) return(NA_real_)
      as.numeric(Hmisc::wtd.quantile(e[[v]][ok], e$no_weight[ok], probs = 0.5, na.rm = TRUE))
    })
  })
  rownames(m_mat_en) <- names(med_metrics_en)

  med_levs_en  <- c("Median Acceptable (€bn)", "Median Appropriate (€bn)", "Median Desirable (€bn)")
  pol_levs_h   <- c(pol_lbl_h[variables_budget[row_ord_en]], med_levs_en)

  blue_pal100 <- colorRampPalette(c("#ffffff", "#1f3a93"))(100)
  hex_from    <- function(x) ifelse(is.na(x), "grey90",
                                    blue_pal100[pmin(100, pmax(1, round(x * 99) + 1))])
  m_max_en    <- max(m_mat_en, na.rm = TRUE)

  df_h_en <- expand.grid(measure = variables_budget, coalition = names(h_masks_en),
                         stringsAsFactors = FALSE)
  df_h_en$rate     <- mapply(function(m, c) cs_mat_en[m, c], df_h_en$measure, df_h_en$coalition)
  df_h_en$pol_lbl  <- factor(pol_lbl_h[df_h_en$measure], levels = pol_levs_h)
  df_h_en$col_lbl  <- factor(h_lbl_en[df_h_en$coalition], levels = h_lbl_en)
  df_h_en$fill_hex <- hex_from(df_h_en$rate)
  df_h_en$lbl_txt  <- ifelse(is.na(df_h_en$rate), "", sprintf("%.0f", df_h_en$rate * 100))
  df_h_en$txt_col  <- ifelse(is.na(df_h_en$rate) | df_h_en$rate < 0.55, "black", "white")

  df_m_en <- expand.grid(metric = names(med_metrics_en), coalition = names(h_masks_en),
                         stringsAsFactors = FALSE)
  df_m_en$value    <- mapply(function(m, c) m_mat_en[m, c], df_m_en$metric, df_m_en$coalition)
  df_m_en$pol_lbl  <- factor(df_m_en$metric, levels = pol_levs_h)
  df_m_en$col_lbl  <- factor(h_lbl_en[df_m_en$coalition], levels = h_lbl_en)
  df_m_en$fill_hex <- hex_from(df_m_en$value / m_max_en)
  df_m_en$lbl_txt  <- ifelse(is.na(df_m_en$value), "", sprintf("%.0f", df_m_en$value))
  df_m_en$txt_col  <- ifelse(is.na(df_m_en$value) | df_m_en$value / m_max_en < 0.55,
                             "black", "white")

  face_x_h_en <- ifelse(h_lbl_en == h_lbl_en["Overall"], "bold", "plain")
  face_y_h_en <- ifelse(pol_levs_h %in% med_levs_en, "bold", "plain")

  p_hs_en <- ggplot() +
    geom_tile(data = df_h_en, aes(x = col_lbl, y = pol_lbl, fill = fill_hex),
              color = "white", linewidth = 0.3, width = 0.92) +
    geom_text(data = df_h_en, aes(x = col_lbl, y = pol_lbl,
                                  label = lbl_txt, color = I(txt_col)), size = 2.1) +
    geom_tile(data = df_m_en, aes(x = col_lbl, y = pol_lbl, fill = fill_hex),
              color = "white", linewidth = 0.3, width = 0.92) +
    geom_text(data = df_m_en, aes(x = col_lbl, y = pol_lbl,
                                  label = lbl_txt, color = I(txt_col)),
              size = 2.1, fontface = "bold") +
    geom_hline(yintercept = length(variables_budget) + 0.5,
               color = "grey45", linewidth = 0.5) +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 8.5) +
    theme(
      axis.text.x         = element_text(angle = 35, hjust = 0, size = 7.5,
                                         face = face_x_h_en, color = "black"),
      axis.text.y         = element_text(size = 7.5, face = face_y_h_en, color = "black"),
      legend.position     = "none",
      panel.grid          = element_blank(),
      plot.margin         = margin(t = 5, r = 60, b = 5, l = 5))

  ggsave("../figures/coalition_support_heatmap_en.pdf", p_hs_en,
         width = 7, height = 9, device = cairo_pdf)
  cat("→ ../figures/coalition_support_heatmap_en.pdf\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 7. Budget correlations: budget_correlations_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  bud_num_mat <- sapply(variables_budget, function(v) {
    x <- as.character(e[[v]])
    ifelse(x == "Souhaitable", 2, ifelse(x == "Convenable", 1,
    ifelse(x == "Supportable", 0, ifelse(x == "Inacceptable", -1, NA_real_))))
  })
  cor_mat_en        <- cor(bud_num_mat, use = "pairwise.complete.obs")
  short_en          <- sub("budget_", "", rownames(cor_mat_en))
  rownames(cor_mat_en) <- labels_budget_en[short_en]
  colnames(cor_mat_en) <- labels_budget_en[short_en]

  pdf("../figures/budget_correlations_en.pdf", width = 14, height = 14)
  corrplot(cor_mat_en, method = "color", type = "upper", order = "hclust",
           tl.cex = 0.55, tl.col = "black", addCoef.col = "black", number.cex = 0.45,
           diag = FALSE, col = colorRampPalette(c("#c0392b", "white", "#2c3e50"))(200))
  dev.off()
  cat("→ ../figures/budget_correlations_en.pdf\n")
}

## ══════════════════════════════════════════════════════════════════════════════
## 8. Clusters combined: clusters_combined_polarises_en.pdf
## ══════════════════════════════════════════════════════════════════════════════
{
  library(cluster)

  # ── Recompute objects needed for clustering (from 4_analyse.R sections 2a & 3) ──

  budget_accept <- as.data.frame(sapply(variables_budget, function(v)
    ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1L,
           ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0L, NA_integer_))))

  ep_num_fn <- function(x) case_when(
    x == "Beaucoup plus favorable"  ~  2,  x == "Plus favorable"           ~  1,
    x == "Ne changerait rien"       ~  0,  x == "Moins favorable"          ~ -1,
    x == "Beaucoup moins favorable" ~ -2,  TRUE ~ NA_real_)
  bud_num_fn <- function(x) case_when(
    x == "Souhaitable"  ~  2,  x == "Convenable"   ~  1,
    x == "Supportable"  ~  0,  x == "Inacceptable" ~ -1,
    TRUE ~ NA_real_)

  program_favorable <- as.data.frame(sapply(
    unname(variables_effect_program), function(v) e[[v]] > 0))
  attitudes_binary  <- c(budget_accept, program_favorable)

  impute_col_means <- function(df) {
    for (j in seq_len(ncol(df))) {
      m <- mean(df[[j]], na.rm = TRUE)
      df[[j]][is.na(df[[j]])] <- m
    }
    df
  }

  mat_b    <- impute_col_means(as.data.frame(sapply(variables_budget,         function(v) bud_num_fn(e[[v]]))))
  mat_e    <- impute_col_means(as.data.frame(sapply(variables_effect_program, function(v) ep_num_fn(e[[v]]))))
  mat_both <- cbind(mat_b, mat_e)
  mats     <- list(budget = mat_b, effect_program = mat_e, both = mat_both)

  leaning_b <- setNames(
    budget_policies$leaning[match(variables_budget, budget_policies$variable_name)],
    variables_budget)
  lean_vals   <- sort(unique(leaning_b[!is.na(leaning_b)]))
  lean_nms    <- paste0("lean", lean_vals)
  lean_nms_df <- make.names(lean_nms)

  sil_scores <- list()
  rows       <- list()
  for (vs in names(mats)) {
    for (k in 2:4) {
      set.seed(42)
      km_tmp <- kmeans(mats[[vs]], centers = k, nstart = 20)
      cl_vec <- km_tmp$cluster; n_tot <- length(cl_vec)
      sil_scores[[paste0(vs, "_", k)]] <-
        mean(cluster::silhouette(cl_vec, dist(mats[[vs]]))[, 3])
      for (j in seq_len(k)) {
        mask      <- cl_vec == j
        lean_means <- setNames(vapply(lean_vals, function(lv) {
          vv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
          if (!length(vv)) return(NA_real_)
          round(100*mean(rowMeans(budget_accept[mask, vv, drop = FALSE], na.rm = TRUE), na.rm = TRUE))
        }, numeric(1)), lean_nms)
        rows[[length(rows) + 1]] <- cbind(
          data.frame(vars_set = vs, k = k, cluster = j,
                     n_pct    = round(sum(mask) / n_tot * 100, 0),
                     vote_agg = round(mean(as.numeric(e$vote_agg)[mask], na.rm = TRUE), 2) - 1,
                     sum_conv = round(mean(e$sum_convenable[mask], na.rm = TRUE), 0),
                     stringsAsFactors = FALSE),
          as.data.frame(as.list(round(lean_means, 2))))
      }
    }
  }
  tbl_long <- do.call(rbind, rows); rownames(tbl_long) <- NULL
  lean_ord <- lean_nms_df[lean_vals != 0.5]
  lean_ord <- c(lean_ord[lean_ord != "lean0"], "lean0")
  tbl_tex  <- tbl_long[order(tbl_long$vars_set, tbl_long$k, tbl_long$vote_agg), ]

  # ── English cluster label assignment ──────────────────────────────────────
  col_map_en <- c(
    Progressives      = "cleft",  Social             = "mgt",
    "Liberal-frugal"  = "cdjc",   Centre             = "cdjc",
    "Liberal-nativist"= "ccdroit", Conservatives     = "cdroite",
    Nativists         = "cdroite", "Social-nativist" = "ced",
    "Deficit hawks"   = "cfrug"
  )
  b_fn <- function(x) isTRUE(x > 50)
  assign_label_en <- function(r) {
    l1 <- r[["lean-1"]]; ls <- r[["lean1"]]; ln <- r[["lean2"]]; l0 <- r[["lean0"]]
    sc <- r[["sum_conv"]]
    if      ( b_fn(l1) &&  b_fn(ls) &&  b_fn(ln) &&  b_fn(l0))                       "Deficit hawks"
    else if ( b_fn(l1) &&  b_fn(ls) &&  b_fn(ln) && !b_fn(l0) && isTRUE(sc > 100))   "Liberal-frugal"
    else if ( b_fn(l1) &&  b_fn(ls) &&  b_fn(ln) && !b_fn(l0))                       "Nativists"
    else if ( b_fn(l1) && !b_fn(ls) && !b_fn(ln) && !b_fn(l0))                       "Progressives"
    else if ( b_fn(l1) && !b_fn(ls) &&  b_fn(ln) && !b_fn(l0) && isTRUE(ln < 60))   "Social"
    else if ( b_fn(l1) && !b_fn(ls) &&  b_fn(ln) && !b_fn(l0))                       "Social-nativist"
    else if (!b_fn(l1) && isTRUE(l1 > 30) &&  b_fn(ln) && !b_fn(l0) && isTRUE(sc < 100)) "Conservatives"
    else if (!b_fn(l1) &&               b_fn(ln) && !b_fn(l0))                       "Liberal-nativist"
    else "Centre"
  }

  lean_ens <- setNames(vapply(lean_vals, function(lv) {
    vv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
    if (!length(vv)) return(NA_real_)
    round(100 * mean(rowMeans(budget_accept[, vv, drop = FALSE], na.rm = TRUE), na.rm = TRUE))
  }, numeric(1)), lean_nms)
  row_ens <- cbind(
    data.frame(vars_set = "all", k = NA_integer_, cluster = 0L,
               n_pct = 100,
               vote_agg = round(mean(as.numeric(e$vote_agg), na.rm = TRUE), 2) - 1,
               sum_conv = round(mean(e$sum_convenable, na.rm = TRUE), 0),
               stringsAsFactors = FALSE),
    as.data.frame(as.list(round(lean_ens, 2))))
  tbl_tex <- rbind(row_ens, tbl_tex[, names(row_ens)])
  rownames(tbl_tex) <- NULL

  tbl_tex$desc  <- ""
  tbl_tex$color <- ""
  tbl_tex$desc[tbl_tex$vars_set == "all"] <- "Overall"
  non_ens <- which(tbl_tex$vars_set != "all")
  for (i in non_ens) {
    lbl <- assign_label_en(tbl_tex[i, ])
    tbl_tex$desc[i]  <- lbl
    tbl_tex$color[i] <- unname(col_map_en[lbl])
  }

  # Insert Social-nativist after Liberal-nativist within each group
  ens_row  <- tbl_tex[tbl_tex$vars_set == "all",  , drop = FALSE]
  body     <- tbl_tex[tbl_tex$vars_set != "all",  , drop = FALSE]
  grp_keys <- unique(body[, c("vars_set", "k")])
  new_body <- body[0, ]
  for (gi in seq_len(nrow(grp_keys))) {
    sub <- body[body$vars_set == grp_keys$vars_set[gi] & body$k == grp_keys$k[gi], ]
    sn  <- which(sub$desc == "Social-nativist")
    ln  <- which(sub$desc == "Liberal-nativist")
    if (length(sn) == 1 && length(ln) == 1 && sn < ln) {
      rest <- setdiff(seq_len(nrow(sub)), sn)
      sub  <- sub[c(rest[seq_len(ln - 1)], sn, rest[seq(ln, length(rest))]), ]
    }
    new_body <- rbind(new_body, sub)
  }
  tbl_tex <- rbind(ens_row, new_body); rownames(tbl_tex) <- NULL

  # Vote rows with English names
  vote_vals_en  <- c("Left", "Center-right or Right", "Far right", "PNR or Other")
  vote_names_en <- c("Left", "Centre-right/Right", "Far right", "Non-response/Other")
  rows_vote_en  <- lapply(seq_along(vote_vals_en), function(i) {
    val   <- vote_vals_en[i]
    mask  <- (e$vote_agg) == val
    lmeans <- setNames(vapply(lean_vals, function(lv) {
      vv <- names(leaning_b)[!is.na(leaning_b) & leaning_b == lv]
      if (!length(vv)) return(NA_real_)
      round(100*mean(rowMeans(budget_accept[mask, vv, drop = FALSE], na.rm = TRUE), na.rm = TRUE))
    }, numeric(1)), lean_nms)
    r_lst <- c(list(vars_set = "vote", k = NA_integer_, cluster = NA_integer_,
                    n_pct    = round(sum(mask) / nrow(e) * 100, 0),
                    vote_agg = round(mean(as.numeric(e$vote_agg)[mask], na.rm = TRUE), 2) - 1,
                    sum_conv = round(mean(e$sum_convenable[mask], na.rm = TRUE), 0)),
               as.list(lmeans))
    lbl <- assign_label_en(r_lst)
    data.frame(vars_set = "vote", k = NA_integer_, cluster = NA_integer_,
               n_pct = r_lst$n_pct, vote_agg = r_lst$vote_agg, sum_conv = r_lst$sum_conv,
               as.data.frame(as.list(lmeans), stringsAsFactors = FALSE),
               desc = vote_names_en[i], color = unname(col_map_en[lbl]),
               stringsAsFactors = FALSE)
  })
  tbl_vote_en <- do.call(rbind, rows_vote_en)
  tbl_tex     <- rbind(tbl_tex, tbl_vote_en); rownames(tbl_tex) <- NULL

  # ── Build clusters_combined figure (English labels) ──────────────────────
  hex_col <- c(cleft    = "#F8766D", ccdroit = "#619CFF", cdroite = "#815EF8",
               ced      = "#A020F0", cfrug   = "#009600", cdjc    = "#9BA073",
               mgt      = "#F0509B")

  label_att_en <- function(v) {
    if (startsWith(v, "effect_program_")) {
      key <- sub("^effect_program_", "", v)
      if (key %in% names(labels_effect_program_en)) unname(labels_effect_program_en[key])
      else gsub("_", " ", key)
    } else {
      key <- sub("^budget_", "", v)
      if (key %in% names(labels_budget_en)) unname(labels_budget_en[key])
      else gsub("_", " ", key)
    }
  }

  fixed_vars <- c(variables_effect_program[c(4, 6:8, 11, 12, 14:17)],
                  variables_budget[c(3, 12, 19, 26, 27)])

  make_rows_fixed_en <- function(v_list, masks, grp_labels) {
    rows_f <- list()
    for (v in v_list) for (i in seq_along(grp_labels)) {
      mask <- masks[[i]]
      vals <- as.numeric(attitudes_binary[[v]])[mask]
      w    <- e$no_weight[mask]; ok <- !is.na(vals) & w > 0
      if (!any(ok)) next
      mu <- weighted.mean(vals[ok], w[ok])
      se <- sqrt(mu * (1 - mu) / (sum(w[ok])^2 / sum(w[ok]^2)))
      rows_f[[length(rows_f) + 1]] <- data.frame(
        measure = label_att_en(v), cluster = grp_labels[i],
        mean = mu, xmin = max(0, mu - 1.96*se), xmax = min(1, mu + 1.96*se),
        stringsAsFactors = FALSE)
    }
    if (length(rows_f)) do.call(rbind, rows_f) else
      data.frame(measure = character(), cluster = character(),
                 mean = numeric(), xmin = numeric(), xmax = numeric(),
                 stringsAsFactors = FALSE)
  }

  # Vote-agg panel
  sub_vote_en <- tbl_tex[tbl_tex$vars_set == "vote", ]
  vg_names_en <- paste0(sub_vote_en$desc, " (", sub_vote_en$n_pct, "%)")
  vg_masks_en <- lapply(vote_vals_en, function(val) e$vote_agg == val)
  vg_colors_en <- setNames(
    ifelse(sub_vote_en$color %in% names(hex_col), hex_col[sub_vote_en$color], "grey70"),
    vg_names_en)
  vg_colors_en[grepl("Centre", names(vg_colors_en))] <- "#74B9FF"
  vg_colors_en[grepl("Non-r",  names(vg_colors_en))] <- "grey60"
  df_vg_en <- make_rows_fixed_en(fixed_vars, vg_masks_en, vg_names_en)

  # Budget k=2, 3, 4 panels
  km_list_en <- list()
  for (k in 2:4) { set.seed(42); km_list_en[[as.character(k)]] <- kmeans(mats[["budget"]], k, nstart = 20) }

  panels_en <- list(vote = list(df = df_vg_en, colors = vg_colors_en, title = "Political bloc"))
  for (k in 2:4) {
    cl_vec_k <- km_list_en[[as.character(k)]]$cluster
    sub_k    <- tbl_tex[tbl_tex$vars_set == "budget" & !is.na(tbl_tex$k) & tbl_tex$k == k, ]
    cl_desc  <- paste0(sub_k$desc, " (", sub_k$n_pct, "%)")
    cl_lbl_k <- setNames(cl_desc, as.character(sub_k$cluster))
    cl_col_k <- setNames(
      ifelse(sub_k$color %in% names(hex_col), hex_col[sub_k$color], "grey70"),
      cl_desc)
    masks_k  <- lapply(seq_len(k), function(j) cl_vec_k == j)
    labs_k   <- cl_lbl_k[as.character(seq_len(k))]
    df_k     <- make_rows_fixed_en(fixed_vars, masks_k, labs_k)
    panels_en[[paste0("b", k)]] <- list(df = df_k, colors = cl_col_k,
                                         title = sprintf("budget k=%d", k))
  }

  spread_g <- sapply(fixed_vars, function(v) {
    all_means <- unlist(lapply(panels_en, function(pd) {
      rows_v <- pd$df[pd$df$measure == label_att_en(v), "mean"]
      if (length(rows_v)) rows_v else NA_real_
    }))
    diff(range(all_means, na.rm = TRUE))
  })
  var_order <- names(sort(spread_g, decreasing = TRUE))
  lev_order <- sapply(rev(var_order), label_att_en)

  make_panel_en <- function(pd, show_y, dodge_w = 0.55) {
    df_i         <- pd$df
    df_i$measure <- factor(df_i$measure, levels = lev_order)
    df_i$cluster <- factor(df_i$cluster, levels = names(pd$colors))
    n_rows       <- length(fixed_vars)
    minor_yi     <- seq(0.5, n_rows - 0.5, by = 1)
    ggplot(df_i, aes(y = measure, x = mean, color = cluster, group = cluster)) +
      geom_hline(yintercept = minor_yi, color = "grey85", linewidth = 0.3) +
      geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
      geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0, linewidth = 0.35,
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
        axis.text.y          = if (show_y) element_text(color = "black") else element_blank(),
        axis.ticks.y         = if (show_y) element_line() else element_blank(),
        legend.text          = element_text(color = "black"),
        plot.title           = element_text(hjust = 0.5, size = 14),
        plot.margin          = margin(t = 3, r = 6, b = 3, l = if (show_y) 3 else 1)
      ) +
      scale_y_discrete(expand = expansion(add = 0.5))
  }

  p_comb_en <- (make_panel_en(panels_en[["b3"]],   show_y = TRUE) |
                make_panel_en(panels_en[["vote"]], show_y = FALSE)) /
               (make_panel_en(panels_en[["b4"]],   show_y = TRUE) |
                make_panel_en(panels_en[["b2"]],   show_y = FALSE))

  ggsave("../figures/clusters_combined_polarises_en.pdf", p_comb_en,
         width = 10, height = 10, device = cairo_pdf)
  cat("→ ../figures/clusters_combined_polarises_en.pdf\n")
}

cat("\nAll English figures done.\n")
