# Rejoue la section (7) de 3_paquets_majoritaires.R (matrices de distances et
# robustesses, dont les regroupements en 2 catégories) sans relancer l'Apriori.
# Usage : Rscript --no-init-file run_robustesse_binaire.R  (depuis code_budget/)
source(".Rprofile")
load(".RData")
e$no_weight <- 1

src <- readLines("3_paquets_majoritaires.R")
chunk <- function(from, to) eval(parse(text = paste(src[from:to], collapse = "\n")),
                                envir = globalenv())
i_coal  <- grep("^## ── Coalitions ", src)
i_score <- grep("^ep_score  <- function", src)
i_dist  <- grep("^## ═+$", src)[which(grepl("Matrice de distances entre groupes",
                                            src[grep("^## ═+$", src) + 1]))]

i_coal_end <- grep("^## ── Fonctions de support binaire", src) - 1

chunk(i_coal, i_coal_end)    # coalition_defs (sans l'Apriori)
chunk(i_score, i_score + 7)  # ep_score + bud_score
chunk(i_dist, length(src))   # section (7) : distances, robustesses, figures
