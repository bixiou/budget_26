### Analyses complémentaires pour budget_26
### Step 2 of TODO.md

source('.Rprofile')
load('.RData')

library(dplyr)

# Helper: get numeric support score (1=Souhaitable, 0.67=Convenable, 0.33=Supportable, 0=Inacceptable, NA=NSP)
support_score <- function(x) {
  case_when(
    x == "Souhaitable" ~ 1,
    x == "Convenable" ~ 2/3,
    x == "Supportable" ~ 1/3,
    x == "Inacceptable" ~ 0,
    TRUE ~ NA_real_
  )
}

# Create numeric matrix of budget support
budget_mat <- sapply(variables_budget_policies, function(v) support_score(e[[v]]))
budget_mat <- as.data.frame(budget_mat)

# Also create binary: accept = Souhaitable or Convenable
budget_accept <- sapply(variables_budget_policies, function(v) {
  ifelse(e[[v]] %in% c("Souhaitable", "Convenable"), 1,
         ifelse(e[[v]] %in% c("Supportable", "Inacceptable"), 0, NA))
})
budget_accept <- as.data.frame(budget_accept)

### 1. Weighted means of budget support by sociodem
cat("\n=== Weighted mean support (conv+souh) by vote_factor ===\n")
for (v in variables_budget_policies) {
  means <- tapply(budget_accept[[v]] * e$weight, e$vote_factor, function(x) sum(x, na.rm=TRUE)) /
           tapply(!is.na(budget_accept[[v]]) * e$weight, e$vote_factor, function(x) sum(x, na.rm=TRUE))
  cat(v, ":", round(means, 2), "\n")
}

cat("\n=== Regressions: support ~ vote + income + age + gender + education ===\n")
results <- list()
for (v in variables_budget_policies) {
  df_reg <- data.frame(
    y = budget_accept[[v]],
    vote = e$vote_factor,
    income = e$income_quartile,
    age = e$age_factor,
    gender = e$man,
    education = e$education,
    weight = e$weight
  )
  df_reg <- df_reg[!is.na(df_reg$y), ]
  tryCatch({
    mod <- lm(y ~ vote + income + age + gender + education, data = df_reg, weights = weight)
    s <- summary(mod)$coefficients
    results[[v]] <- s
    cat("\n---", v, "---\n")
    print(round(s, 3))
  }, error = function(err) cat("Error for", v, ":", err$message, "\n"))
}

### 2. Correlation matrix of budget support
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

### 3. Clustering of respondents
cat("\n=== K-means clustering of respondents (k=4) ===\n")
# Use complete cases only
complete_rows <- complete.cases(budget_accept)
set.seed(42)
km <- kmeans(budget_accept[complete_rows, ], centers = 4, nstart = 20)
e$cluster <- NA_integer_
e$cluster[complete_rows] <- km$cluster
cat("Cluster sizes:", table(km$cluster), "\n")

# Vote profile by cluster
cat("\nVote profile by cluster:\n")
for (cl in 1:4) {
  cat(sprintf("Cluster %d (n=%d): ", cl, sum(km$cluster == cl, na.rm=TRUE)))
  vote_tbl <- prop.table(table(e$vote_factor[complete_rows][km$cluster == cl]))
  cat(paste(round(vote_tbl*100, 1), names(vote_tbl), sep="% ", collapse=", "), "\n")
}

# Mean support by cluster
cat("\nMean support by cluster:\n")
cluster_means <- aggregate(budget_accept[complete_rows, ], by = list(cluster = km$cluster), FUN = mean, na.rm = TRUE)
print(round(cluster_means, 2))

### 4. Effect_program analyses
cat("\n=== Effect program: mean favorability by vote_factor ===\n")
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
for (v in variables_effect_program) {
  score <- ep_score(e[[v]])
  overall <- weighted.mean(score, e$weight, na.rm = TRUE)
  cat(v, ": overall =", round(overall, 3))
  means <- tapply(score * e$weight, e$vote_factor, function(x) sum(x, na.rm=TRUE)) /
           tapply(!is.na(score) * e$weight, e$vote_factor, function(x) sum(x, na.rm=TRUE))
  cat("  by vote:", round(means, 2), "\n")
}

### 5. Majority packages summary
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

cat("\nAnalyses complete.\n")
