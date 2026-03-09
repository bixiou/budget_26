##### Budget survey: prepare, convert, stats_exclude #####
if (file.exists(".Rprofile")) source(".Rprofile")
source("1_rename.R")
library(readr)
if (!exists("decrit")) decrit <- function(x, ...) print(summary(x))

# Adapted from former_2_prepare.R: guards for columns that may be absent in Budget.
stats_exclude <- function(data_name, all = FALSE, old_names = FALSE) {
  cat("\n\nSURVEY:", data_name, "\n\n")
  e <- read_csv(paste0("../data_raw/", data_name, ".csv"), guess_max = Inf)
  if (!old_names) {
    if ("finished" %in% names(e)) e$Finished <- e$finished
    if ("progress" %in% names(e)) e$Progress <- e$progress
  }
  if ("Finished...7" %in% names(e)) e$Finished <- e$Finished...7
  socio_demo_cond <- if ("income" %in% names(e) && "urbanity" %in% names(e) && "region" %in% names(e) && "age_exact" %in% names(e)) (e$income == "Prefer not to say" | e$urbanity %in% 0 | e$region %in% 0 | e$age_exact %in% "Below 18") else rep(FALSE, nrow(e))
  desc_num <- function(x) print(summary(x))
  cat(paste0(nrow(e), " total obs.\n"))
  if ("Q_TerminateFlag" %in% names(e)) {
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "QuotaMet") / nrow(e), 1), "% quota met (incl. socio-demo screened)\n"))
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "QuotaMet" & socio_demo_cond) / nrow(e), 1), "% socio-demo screened\n"))
    if (all) cat(paste0(sum((!e$Q_TerminateFlag %in% "QuotaMet") & socio_demo_cond), " socio-demo screened not flagged as Quota Met\n"))
  }
  if ("income" %in% names(e)) cat(paste0(round(100 * sum(e$income %in% "Prefer not to say") / nrow(e), 1), "% socio-demo screened due to PNR income\n"))
  if ("urbanity" %in% names(e) || "region" %in% names(e)) { urb_or_reg <- (if ("urbanity" %in% names(e)) e$urbanity %in% 0 else FALSE) | (if ("region" %in% names(e)) e$region %in% 0 else FALSE); cat(paste0(round(100 * sum(urb_or_reg) / nrow(e), 1), "% socio-demo screened due to unrecognized zipcode\n")) }
  if (all && "age_exact" %in% names(e)) cat(paste0(round(100 * sum(e$age_exact %in% "Below 18") / nrow(e), 1), "% socio-demo screened due to age < 18\n"))
  if ("Q_TerminateFlag" %in% names(e)) {
    n_valid <- sum(!e$Q_TerminateFlag %in% "QuotaMet"); n_legit <- sum(is.na(e$Q_TerminateFlag))
    cat(paste0(n_valid, " valid: not quota met or socio-demo screened\n"), paste0(sum(e$Q_TerminateFlag %in% "Screened"), " screened after socio-demo\n"))
    if (all) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened") / nrow(e), 1), "% screened in total\n"))
    cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened") / n_valid, 1), "% screened among valid\n"))
    if ("attention_test" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little")) / n_valid, 1), "% screened among valid due to failed attention_test\n"))
    if ("Q_TotalDuration" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & e$Q_TotalDuration < 360) / n_valid, 1), "% screened among valid due to duration < 360\n"))
    if ("attention_test" %in% names(e) && "Q_TotalDuration" %in% names(e)) cat(paste0(round(100 * sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little") & e$Q_TotalDuration < 360) / n_valid, 1), "% screened among valid due to both reasons\n"))
    if (all) cat(paste0(n_legit, " legit: not quota met nor screened out\n"))
    if (n_legit > 0) { cat(paste0(round(100 * sum(!e$Finished %in% c(TRUE, "TRUE", 1)) / n_legit, 1), "% dropout among legit\n")); if (all) cat(paste0(round(100 * sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag)) / n_legit, 1), "% finished among legit\n")) }
    cat(paste0(sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag)), " final: finished, not quota met nor screened out\n"))
  }
  if (all && "Progress" %in% names(e) && "Q_TerminateFlag" %in% names(e)) { cat("Progress among legit\n"); desc_num(e$Progress[is.na(e$Q_TerminateFlag)]) }
  if ("Q_TerminateFlag" %in% names(e) && "Q_TotalDuration" %in% names(e) && "Finished" %in% names(e)) {
    final_idx <- is.na(e$Q_TerminateFlag) & e$Finished %in% c(TRUE, "TRUE", 1)
    cat("Duration in final sample\n"); if (sum(final_idx) > 0) desc_num(e$Q_TotalDuration[final_idx] / 60)
    if (all && "long" %in% names(e) && sum(final_idx) > 0) print(summary(lm(Q_TotalDuration/60 ~ (long > .42), data = e, subset = final_idx)))
    if (all && "cut" %in% names(e) && "long" %in% names(e)) {
      for (lab in c("cut == 1", "cut == 0", "long > .42", "long < .42")) {
        idx <- if (lab == "cut == 1") e$cut == 1 & final_idx else if (lab == "cut == 0") e$cut == 0 & final_idx else if (lab == "long > .42") e$long > .42 & final_idx else e$long < .42 & final_idx
        cat("Duration in final sample for", lab, "\n"); if (sum(idx, na.rm = TRUE) > 0) desc_num(e$Q_TotalDuration[idx] / 60)
      }
      if (sum(final_idx) > 0) print(summary(lm(Q_TotalDuration/60 ~ (long > .42) * cut, data = e, subset = final_idx)))
    }
  }
  cat("___________________________________________\n")
}
