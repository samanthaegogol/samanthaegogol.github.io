# ============================================================
# GM Visualization + Integration in R (Portfolio)
# Topics: GPA, mean shapes, thin-plate spline (TPS) deformation,
#         two-block integration test (integration.test; PLS)
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

library(geomorph)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# ---- Load data ----
master1 <- read.csv("master_portfolio.csv")

# Metadata
master1 <- master1 %>%
  mutate(
    spp = substr(cat, 1, 2),
    sex = substr(cat, 4, 4)
  )

# Coordinate-only data frame (drop discrete variables)
coord_df <- master1 %>%
  select(-cat, -spp, -sex)

# ---- Convert to geomorph array ----
k <- 3
p <- ncol(coord_df) / k
stopifnot(p == round(p))  # number of landmarks must be integer

coords_array <- arrayspecs(coord_df, p = p, k = k)

# ============================================================
# PART A: Mean shapes + TPS deformation (Humans vs Bonobos/Chimps)
# ============================================================

# GPA
gpa <- gpagen(coords_array)
shape <- gpa$coords

# If you need to reorient, keep it explicit and explain why.
# (If orientation is inconsistent across specimens, this step can be removed.)
shape <- shape * (-1)

# Subset to humans, chimps, bonobos (Ho, Pt, Pp)
keep_spp <- c("Ho", "Pt", "Pp")
idx <- master1$spp %in% keep_spp

shape_sub <- shape[, , idx, drop = FALSE]
meta_sub <- master1[idx, ]

# Split shape array by species
array_groups <- coords.subset(shape_sub, group = meta_sub$spp)

# Mean shapes per group
group_mean <- lapply(array_groups, mshape)

# Convert mean shapes to matrices for TPS plots
human_mat  <- as.matrix(group_mean$Ho)[, 1:3]
chimp_mat  <- as.matrix(group_mean$Pt)[, 1:3]
bonobo_mat <- as.matrix(group_mean$Pp)[, 1:3]

# TPS: reference -> target
plotRefToTarget(human_mat, bonobo_mat, method = "TPS",
                main = "TPS: Human (ref) → Bonobo (target)")

plotRefToTarget(bonobo_mat, human_mat, method = "TPS",
                main = "TPS: Bonobo (ref) → Human (target)")

# Alternative visualizations
plotRefToTarget(human_mat, bonobo_mat, method = "points",
                main = "Points: Human → Bonobo")

plotRefToTarget(human_mat, bonobo_mat, method = "vector",
                main = "Vectors: Human → Bonobo")

# TPS grids visualize shape change by warping a regular grid from
# the reference (mean) shape to a target configuration.
# Deformation magnitude and direction indicate where landmarks
# deviate most from the reference in each plane (X–Y, Y–Z).

# ============================================================
# PART B: Shape ordination (tangent space PCA) — use gm.prcomp()
# (plotTangentSpace() is defunct in newer geomorph versions)
# ============================================================

gmpca <- gm.prcomp(shape_sub)

pcscores <- as.data.frame(gmpca$x)
colnames(pcscores) <- paste0("PC", seq_len(ncol(pcscores)))

pcscores$spp <- meta_sub$spp
pcscores$sex <- meta_sub$sex

propvar <- (gmpca$sdev^2) / sum(gmpca$sdev^2)
pc1_lab <- paste0("PC1 (", round(100 * propvar[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * propvar[2], 1), "%)")

ggplot(pcscores, aes(x = PC1, y = PC2, color = spp, shape = sex)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Tangent Space PCA (Humans, Chimps, Bonobos)",
    x = pc1_lab,
    y = pc2_lab,
    color = "Species code",
    shape = "Sex"
  ) +
  coord_equal()


# PCA is performed in tangent space after Procrustes alignment.
# PC axes summarize major patterns of shape variation; TPS
# visualizations correspond to deformations along these axes.

# PC1 extremes (approximate) using shapes along PC1
# Here we show min/max along PC1 using TPS
# gmpca <- gm.prcomp(shape_sub)
scores <- gmpca$x
loadings <- gmpca$rotation
center <- gmpca$center  # vectorized mean shape (in tangent space)

# Identify min and max along PC1
i_min <- which.min(scores[, 1])
i_max <- which.max(scores[, 1])

# Reconstruct vectorized shapes at those extremes
v_min <- center + scores[i_min, 1] * loadings[, 1]
v_max <- center + scores[i_max, 1] * loadings[, 1]

# Convert vector -> p x k matrix (p landmarks, k=3)
k <- 3
p_sub <- dim(shape_sub)[1]

minshape <- matrix(v_min, nrow = p_sub, ncol = k, byrow = TRUE)
maxshape <- matrix(v_max, nrow = p_sub, ncol = k, byrow = TRUE)

plotRefToTarget(minshape, maxshape, method = "TPS",
                main = "PC1 Shape Change (min → max)")

# TPS grids shown here represent deformations relative to the
# Procrustes mean shape, allowing direct visual comparison of
# localized shape differences among groups.

plotRefToTarget(minshape, maxshape, method = "vector",
                main = "PC1 Shape Change (vectors)")

# Vectors indicate the direction and relative magnitude of landmark
# displacement along PC1, showing how shape changes from negative
# to positive scores on this axis.

plotRefToTarget(minshape, maxshape, method = "points",
                main = "PC1 Shape Change (points)")

# PC1 shape change (points) shows landmark positions at the
# extremes of PC1, emphasizing relative displacement without
# vector direction.


# ============================================================
# PART C: Integration test (facial vs cranial base blocks)
# ============================================================

quant <- master1 %>% select(-cat, -spp, -sex)

sub <- quant[, c(
  25:27, 34:36, 40:45, 58:69, 79:96,
  118:120, 127:129, 133:138, 142:156
)]

# Convert to array
p2 <- ncol(sub) / k
stopifnot(p2 == round(p2))

array2 <- arrayspecs(sub, p = p2, k = k)
gpa2 <- gpagen(array2)
coords2 <- gpa2$coords

# Landmark partitions (F = face, B = cranial base)
# NOTE: length must equal p2 (number of landmarks)
partition <- c(
  rep("F", 18),  # facial landmarks
  rep("B", 5)    # cranial base landmarks
)
stopifnot(length(partition) == p2)

# Integration test (two-block PLS)
int_test <- integration.test(coords2, partition.gp = partition)

# Proportion of shared variance explained by singular values
eigval <- int_test$svd$d / sum(int_test$svd$d)
eigval

# Scores
scores_face  <- int_test$XScores
scores_base  <- int_test$YScores

# Plot component 1 and 2 (base R; add legends correctly)
spp_sub <- meta_sub$spp  # same 3 taxa subset labels
#Select another 3 colors from the Set2 palette
col2<-brewer.pal(n=3,name='Set2')
#use set 2 in color brewer palate to color the species in dataset m2
spp.col<- col2[as.factor(meta_sub$spp)]

par(mfrow = c(1, 2))
plot(scores_face[, 1], scores_base[, 1],
     pch = 19, xlab = "Facial block — Comp 1", ylab = "Cranial base block — Comp 1",
     main = "Integration: Comp 1", col=spp.col)
legend("bottomright", legend = unique(meta_sub$spp),
       col = col2, pch = 19, bty = "n")

plot(scores_face[, 2], scores_base[, 2],
     pch = 19, xlab = "Facial block — Comp 2", ylab = "Cranial base block — Comp 2",
     main = "Integration: Comp 2", col=spp.col)
legend("bottomright", legend = unique(meta_sub$spp),
       col = col2, pch = 19, bty = "n")
par(mfrow = c(1, 1))

# Scatter plots compare component scores from separate landmark
# blocks (e.g., cranial vs facial) to assess covariation.
# Linear trends indicate coordinated shape change between blocks.

# ---- End of script ----
