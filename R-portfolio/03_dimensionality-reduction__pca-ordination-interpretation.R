# ============================================================
# PCA in R (Portfolio)
# Topic: Dimensionality reduction + interpretation + visualization
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
#setwd()

library(dplyr)
library(ggplot2)

# ---- Load data ----
cranial <- read.csv("cranial_portfolio.csv")

# ---- PCA ----
# PCA should generally be performed on scaled variables when units differ
pca <- prcomp(cranial[, 4:ncol(cranial)], scale. = TRUE)

# Variance explained
eigval <- pca$sdev^2
prop_var <- eigval / sum(eigval)

# Scree plot (variance explained)
scree <- data.frame(
  PC = paste0("PC", seq_along(prop_var)),
  Variance = prop_var
)

ggplot(scree, aes(x = PC, y = Variance)) +
  geom_col() +
  labs(
    title = "Scree Plot",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Quick reporting numbers
round(prop_var[1:4], 4)
round(sum(prop_var[1:2]), 4)

# Loadings (eigenvectors)
loadings <- pca$rotation
loadings

# ---- Scores + plotting ----
scores <- as.data.frame(pca$x)
scores$cat <- cranial$cat
scores$spp <- cranial$spp

# Label axes with variance %
pc1_lab <- paste0("PC1 (", round(100 * prop_var[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * prop_var[2], 1), "%)")

ggplot(scores, aes(x = PC1, y = PC2, color = spp)) +
  geom_point(alpha = 0.8) +
  labs(
    title = "PCA of Cranial Linear Measurements",
    x = pc1_lab,
    y = pc2_lab,
    color = "Species code"
  ) +
  coord_equal()

# Optional: 3D plotting (kept lightweight and optional)
# If you want 3D, use plotly
# library(plotly)
# plot_ly(scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~spp, type = "scatter3d", mode = "markers")

# ---- Interpretation notes ----
# - PCA rotates correlated measurements into orthogonal axes (PCs) ordered by variance explained.
# - Loadings indicate which measurements contribute most to each PC.
# - Score plots visualize clustering/separation among groups (here, species codes).

# ---- End of script ----