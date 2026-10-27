### Re-export in black and white (options(fig_bw = TRUE)) the figures of papers/budget_grey.tex, to ../figures/grey/
### Run with: Rscript --no-init-file gen_figures_grey.R
### The scripts' other outputs (figures, tables) are redirected to a temporary folder and discarded.
### Figures in a single shade of blue (lmg, r2_iso, coalition matrices, paquets_profils) are already readable in black and white.

options(fig_bw = TRUE)
tmp_dir <- paste0(gsub("\\\\", "/", tempdir()), "/fig_bw/")
dir.create(tmp_dir, showWarnings = FALSE)
dir.create("../figures/grey", showWarnings = FALSE)

#' Evaluate lines of a script in the global environment, with outputs redirected to tmp_dir
#' @param lines Character vector of code lines.
run_lines <- function(lines) {
  lines <- gsub("../figures/", tmp_dir, lines, fixed = TRUE)
  lines <- gsub("../tables/", tmp_dir, lines, fixed = TRUE)
  tryCatch(eval(parse(text = paste(lines, collapse = "\n")), envir = .GlobalEnv),
           error = function(e) cat("Error:", conditionMessage(e), "\n")) # so that figures already produced are still copied
}

## Stacked bars (2_render.R): effect_program and budget
load(".RData")
source(".Rprofile") # after .RData, so that barres() handles fig_bw
l2 <- readLines("2_render.R")
run_lines(l2[1:(which(grepl("^##### Export PDFs", l2))[1] - 1)])
barres_multiple(barres_defs["effect_program"], folder = tmp_dir, export_xls = FALSE)
barres_defs[["budget"]]$legend[5] <- "Ne sais pas"
barres_defs[["budget"]]$labels <- break_strings(paste0(labels_vars[variables_budget], ": ", sub(".", ",", as.character(budget_policies_amounts), fixed = T), " Mds"), 64)
barres_multiple(barres_defs["budget"], folder = tmp_dir, export_xls = FALSE, weights = F)
cat("Stacked bars done.\n")

## notes_groupes and distance matrices (3_paquets_majoritaires.R, sections 6 and 7)
l3 <- readLines("3_paquets_majoritaires.R")
setup_end  <- which(grepl("^## \\(1\\)  supp", l3))[1] - 1
sec6_start <- which(grepl("^## \\(6\\) Notes", l3))[1]
sec7_end   <- which(grepl("^cat\\(\"\\\\nTermin", l3))[1] - 1
run_lines(l3[1:setup_end])
run_lines(l3[sec6_start:sec7_end])
cat("notes_groupes and distance matrices done.\n")

## budget_correlations and clusters_combined_polarises (4_analyse.R, up to section 4a)
## Skipped sections: Representativeness (writes to ../tables/ through a default argument, not redirected)
## and 3d (its exhaustive enumeration of packages takes hours and is not used afterwards).
l4 <- readLines("4_analyse.R")
sec_lines <- function(from, to) which(grepl(from, l4))[1]:(which(grepl(to, l4))[1] - 1)
skipped <- c(sec_lines("^##### Representativeness", "^##### Budget; Claude"), sec_lines("^##### 3d\\.", "^##### 3e\\."))
run_lines(l4[setdiff(1:(which(grepl("^##### 4a\\.", l4))[1] - 1), skipped)])
cat("Correlations and clusters done.\n")

figs_bw <- c("effect_program", "budget", "notes_groupes_effect_program", "notes_groupes_budget", "distance_matrix_pairwise",
             "distance_matrix_means", "budget_correlations", "clusters_combined_polarises")
copied <- file.copy(paste0(tmp_dir, figs_bw, ".pdf"), "../figures/grey/", overwrite = TRUE)
cat("\nBlack and white figures exported to ../figures/grey/:", figs_bw[copied], "\n")
if (!all(copied)) cat("Missing:", figs_bw[!copied], "\n")
