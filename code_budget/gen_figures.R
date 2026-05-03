### Re-export 5 figures (no title, no_weight)
### Run with: Rscript --no-init-file gen_figures.R

load('.RData')
e$no_weight <- 1

l3 <- readLines('3_paquets_majoritaires.R')
l4 <- readLines('4_analyse.R')

# Detect section boundaries
setup_end   <- which(grepl("^## \\(1\\)  supp", l3))[1] - 1
sec6_start  <- which(grepl("^## \\(6\\) Notes", l3))[1]
sec7_start  <- which(grepl("^## \\(7\\) Matrice", l3))[1]
sec7_end    <- which(grepl("^cat\\(\"\\\\nTermin", l3))[1] - 1
sec6_start4 <- which(grepl("^##### 6\\. Coalition packages", l4))[1]
sec6b_end4  <- which(grepl("Analyses complete", l4))[1] - 1

cat(sprintf("Setup 1:%d | sec6 %d:%d | sec7 %d:%d | 4_analyse %d:%d\n",
            setup_end, sec6_start, sec7_start-1, sec7_start, sec7_end,
            sec6_start4, sec6b_end4))

eval(parse(text = paste(l3[1:setup_end],          collapse = '\n')), envir = .GlobalEnv)
cat("Setup done.\n")
eval(parse(text = paste(l3[sec6_start:(sec7_start-1)], collapse = '\n')), envir = .GlobalEnv)
cat("Section 6 done (notes_groupes).\n")
eval(parse(text = paste(l3[sec7_start:sec7_end],   collapse = '\n')), envir = .GlobalEnv)
cat("Section 7 done (distances).\n")
eval(parse(text = paste(l4[sec6_start4:sec6b_end4], collapse = '\n')), envir = .GlobalEnv)
cat("Coalition figures done.\n")
cat("\nAll 5 figures exported.\n")
