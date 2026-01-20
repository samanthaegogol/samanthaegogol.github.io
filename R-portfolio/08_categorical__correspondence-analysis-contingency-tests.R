# ============================================================
# Distance-Based Clustering + Correspondence Analysis (Portfolio)
# Topics: GPA -> group mean shapes -> Euclidean distances -> UPGMA + NJ trees;
#         contingency tables -> correspondence analysis
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

library(geomorph)
library(dplyr)
library(ape)   # nj()
library(ca)    # ca(), mjca()

# ---- Load data ----
master <- read.csv("master_portfolio.csv")

# Metadata
master <- master %>%
  mutate(
    spp = substr(cat, 1, 2),
    sex = substr(cat, 4, 4)
  )

# ---- Landmarks -> array -> GPA ----
k <- 3
coord_df <- master[, 2:196]  # assumes cat is col 1, then 195 coords
p <- ncol(coord_df) / k
stopifnot(p == round(p))

A <- arrayspecs(coord_df, p = p, k = k)
gpa <- gpagen(A)
shape <- gpa$coords

# ============================================================
# PART A: Species mean shapes -> distance matrix -> trees
# ============================================================

# Compute mean shape per species in Procrustes space
spp_levels <- sort(unique(master$spp))

mean_vecs <- sapply(spp_levels, function(sp) {
  idx <- master$spp == sp
  m <- mshape(shape[, , idx, drop = FALSE])   # p x k
  as.vector(m)                               # vectorize to 1D
})

# mean_vecs is (p*k) x (n_species). Transpose to species x variables
mean_mat <- t(mean_vecs)
rownames(mean_mat) <- spp_levels

# Euclidean distances among species mean shapes
cran_dist <- dist(mean_mat, method = "euclidean")

# UPGMA / average-linkage clustering
upgma <- hclust(cran_dist, method = "average")
plot(upgma, lwd = 2, xlab = "", main = "UPGMA (Average Linkage) on Species Mean Shapes")

# Neighbor joining tree
nj_tree <- nj(cran_dist)
plot(nj_tree, main = "Neighbor-Joining Tree on Species Mean Shapes")

# Interpretation note:
# Trees are constructed from Euclidean distances among Procrustes mean shapes.
# They reflect shape similarity patterns, which may (or may not) match known phylogeny.

# ============================================================
# PART B: Sex-by-species contingency -> correspondence analysis
# ============================================================

# Build contingency table (replaces the nested loops)
sex_table <- table(master$spp, master$sex)

# Optional: order columns consistently if present
# sex_table <- sex_table[, intersect(c("f","m","u"), colnames(sex_table)), drop = FALSE]

sex_ca <- ca(sex_table)
summary(sex_ca)

plot(sex_ca, mass = TRUE, arrows = c(TRUE, TRUE),
     main = "Correspondence Analysis: Sex Distribution by Species")

# Interpretation note:
# CA visualizes association between taxa (rows) and sex categories (columns);
# points/vectors closer together indicate stronger association.

# ============================================================
# PART C: Example multiway CA (built-in dataset)
# ============================================================

data("HairEyeColor")

# Note: mjca is from package 'ca'
he_mjca <- mjca(HairEyeColor)
summary(he_mjca)

plot(he_mjca, arrows = c(TRUE, TRUE),
     main = "Multiple Correspondence Analysis: Hair/Eye Color (Example Dataset)")

# ---- End of script ----