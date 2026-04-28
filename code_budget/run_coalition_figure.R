setwd("C:/Users/fabre/Documents/www/budget_26/code_budget")
load(".RData")
paq <- readLines("paquets_majoritaires.R")
# Lines 14-204: libraries + constants + labels + coalition_defs + support matrices + run_apriori
eval(parse(text = paste(paq[14:204], collapse = "\n")))
# Lines 596-636: group_labels_fr + group_labels_short (defined later in section 7)
eval(parse(text = paste(paq[596:636], collapse = "\n")))
# Run section 6 of 3_analyse.R
a3 <- readLines("3_analyse.R")
sec6 <- which(grepl("^##### 6\\.", a3))
eval(parse(text = paste(a3[sec6:(length(a3) - 1L)], collapse = "\n")))
