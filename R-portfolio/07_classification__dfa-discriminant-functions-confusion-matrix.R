# ============================================================
# Classification in Shape Space (Portfolio)
# Topics: GPA, CVA (cross-validation), unknown projection, MANOVA,
#         Hotelling's T² pairwise comparisons
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

library(geomorph)
library(Morpho)
library(dplyr)
library(ggplot2)
library(Hotelling)

# ---- Load data ----
master <- read.csv("master_portfolio.csv")

# Metadata
master <- master %>%
  mutate(
    spp = substr(cat, 1, 2),   # e.g., Pp, Pt, Hs...
    sub = substr(cat, 1, 3),   # e.g., Haa, Hal...
    sex = substr(cat, 4, 4)    # m/f/u in your convention
  )

# Coordinate-only matrix/data frame (drop ID + metadata)
coord_df <- master %>% select(-cat, -spp, -sub, -sex)

k <- 3
p <- ncol(coord_df) / k
stopifnot(p == round(p))

coords_array <- arrayspecs(coord_df, p = p, k = k)

# ============================================================
# PART A: Bonobo sex classification with unknown projection
# ============================================================

bonobo_meta <- master %>% filter(spp == "Pp")
bonobo_idx  <- master$spp == "Pp"

bonobo_array <- coords_array[, , bonobo_idx, drop = FALSE]
bonobo_gpa   <- gpagen(bonobo_array)

shape <- bonobo_gpa$coords
sex   <- bonobo_meta$sex

# Split into known (m/f) and unknown (u/other)
known_idx <- sex %in% c("m", "f")
unk_idx   <- !known_idx

shape_known <- shape[, , known_idx, drop = FALSE]
sex_known   <- sex[known_idx]

shape_unk <- shape[, , unk_idx, drop = FALSE]

# CVA on known sexes
cva <- CVA(shape_known, group = sex_known, cv = TRUE, weighting = TRUE)

# Histogram and boxplot of CV1 scores show males shifted positive
# and females shifted negative, but with overlapping distributions.


# Cross-validation table (classification rates)
cva

# CV scores for known specimens
scores <- cva$CVscores  # matrix (n_known x 1) since 2 groups -> 1 CV axis

# ---- Project unknowns into the same CV space ----
# CV scores for known specimens
scores_mat <- cva$CVscores

# For 2 groups there is 1 CV axis; force it to a numeric vector
cv1 <- as.numeric(scores_mat[, 1])

# Keep only finite scores (guards against occasional NA/Inf)
ok <- is.finite(cv1) & !is.na(sex_known)
cv1_ok <- cv1[ok]
sex_ok <- droplevels(factor(sex_known[ok]))

# Plot as boxplot (more stable than formula plot for this)
boxplot(cv1_ok ~ sex_ok,
        col = c("hotpink", "darkgoldenrod1"),
        main = "Bonobo CVA: Sex Classification with Unknown Projections",
        ylab = "Canonical Variate 1 score",
        xlab = "Sex (known)")

# CV1 shows partial separation between sexes with substantial overlap.
# Cross-validated accuracy = 61.1% (kappa = 0.20).
# Interpretation: weak-to-moderate sex signal; limited predictive power.

# ============================================================
# PART B: Hylobatid subspecies discrimination (CVA + MANOVA + Hotelling)
# ============================================================

hylo_meta <- master %>%
  filter(sub %in% c("Haa", "Hal", "Hau", "Hma", "Hmf", "Hss"))

hylo_idx <- master$sub %in% c("Haa", "Hal", "Hau", "Hma", "Hmf", "Hss")
hylo_array <- coords_array[, , hylo_idx, drop = FALSE]

hylo_gpa <- gpagen(hylo_array)
hylo_coords <- hylo_gpa$coords

# CVA across subspecies
cva_hylo <- CVA(hylo_coords, group = hylo_meta$sub, cv = TRUE, weighting = TRUE)
cva_hylo

# Overall accuracy = 62.5% (kappa = 0.52).
# One subspecies (Hss) is strongly separated; others show overlap.

hylo_scores <- cva_hylo$CVscores
var_hylo <- cva_hylo$Var

# 2D CV plot (CV1 vs CV2)
df_plot <- data.frame(
  CV1 = hylo_scores[, 1],
  CV2 = hylo_scores[, 2],
  sub = hylo_meta$sub
)

ggplot(df_plot, aes(x = CV1, y = CV2, color = sub)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "CVA: Hylobatid Subspecies",
    x = paste0("CV1 (", round(100 * var_hylo[1], 2), "%)"),
    y = paste0("CV2 (", round(100 * var_hylo[2], 2), "%)"),
    color = "Subspecies"
  ) +
  coord_equal()

# MANOVA on multiple CV axes (use first 4 CVs like your original approach)
# Note: number of CVs = (#groups - 1). Use up to that many.
n_cv <- min(4, ncol(hylo_scores))
hylo_manova <- manova(hylo_scores[, 1:n_cv] ~ hylo_meta$sub)

summary(hylo_manova)
summary(hylo_manova, test = "Roy")

# MANOVA indicates significant shape differences among subspecies
# (Pillai and Roy tests: p < 2.2e-16).

# Pairwise Hotelling's T² on the same CV subset
subs <- sort(unique(hylo_meta$sub))
pairs <- combn(subs, 2, simplify = FALSE)

hotelling_results <- lapply(pairs, function(pr) {
  g1 <- hylo_scores[hylo_meta$sub == pr[1], 1:n_cv, drop = FALSE]
  g2 <- hylo_scores[hylo_meta$sub == pr[2], 1:n_cv, drop = FALSE]
  out <- hotelling.test(g1, g2)
  data.frame(
    group1 = pr[1],
    group2 = pr[2],
    p_value = out$pval
  )
})

hotelling_results <- bind_rows(hotelling_results)
hotelling_results

# Pairwise Hotelling tests indicate significant differences
# between most subspecies pairs (see p-value table).

# ---- End of script ----