# ============================================================
# Multivariate Methods in R (Portfolio)
# Topics: bgPCA, PCoA (ordination from distance matrix), PLS
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
# setwd()

library(dplyr)
library(ggplot2)

# Only load these if you have them installed:
# install.packages("Morpho")
# install.packages("labdsv")
# install.packages("pls")
library(Morpho)
library(labdsv)
library(pls)

# ---- Load data ----
# Use a portfolio-safe copy if needed:
# cranial <- read.csv("data/cranial_portfolio.csv")
cranial <- read.csv("cranial_portfolio.csv")

# Identify measurement columns (assumes: columns 1–3 are IDs/meta; 4:14 are measurements)
measure_cols <- 4:14

# ============================================================
# 1) Between-group PCA (bgPCA)
# Question: Do bonobos (Pp) and chimp subspecies (Pt*) separate in cranial space?
# ============================================================

pan<-subset(cranial,cranial$spp %in% c('Pp', "Pt"))
#View(pan)                 

#EXERCISE 1 STEP 2
#Creating new variable for genus by shortening the cat variable by the first character
pan$genus <-substr(pan$cat, 1,1)
#Creating new variable for species by shortening the cat variable by the second character
pan$species <-substr(pan$cat, 2,2)
#Creating new variable for subspecies by shortening the cat variable by the third character
pan$subspecies <-substr(pan$cat, 3,3)

# bgPCA for subspecies with weighting for unequal group sizes
bg<-groupPCA(pan[,4:14],groups=pan$subspecies,weighting=T)

# Eigenvectors and variance explained
eigvec <- bg$groupPCs
propvar <- bg$Variance   # already proportions for bgPCA
propvar

# Prepare scores for plotting
bg_scores <- as.data.frame(bg$Scores)
bg_scores$subspecies <- pan$subspecies

# Optional: readable labels (only if your subspecies codes map cleanly)
sub_labels <- c(
  "n" = "Pan paniscus",
  "s" = "Pan troglodytes schweinfurthii",
  "t" = "Pan troglodytes troglodytes",
  "v" = "Pan troglodytes verus"
)

bg_scores$group <- ifelse(bg_scores$subspecies %in% names(sub_labels),
                          sub_labels[bg_scores$subspecies],
                          bg_scores$subspecies)

pc1_lab <- paste0("bgPC1 (", round(100 * propvar[1], 2), "%)")
pc2_lab <- paste0("bgPC2 (", round(100 * propvar[2], 2), "%)")

ggplot(bg_scores, aes(x = V1, y = V2, color = group)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Between-Group PCA (bgPCA): Genus Pan Subspecies",
    x = pc1_lab,
    y = pc2_lab,
    color = "Group"
  ) +
  coord_equal()

#Most of the between-group signal is concentrated on bgPC1, with visual separation driven primarily by bonobos vs. chimp subspecies; secondary axes contribute little additional group separation.

# ============================================================
# 2) Principal Coordinates Analysis (PCoA) from distance matrix
# Question: Does distance-based ordination reproduce PCA-like structure?
# ============================================================

# Keep only hominids: Gg, Hs, Po, Pp, Pt
cran<-droplevels(cranial,levls=cranial$spp, exclude=(c("Cm","Gb","Ha", "Hm", "Hs","Pb","Ph")))
#remove rows with NULL spp
cran_hom <- na.omit(cran, cols=seq_along(cran$spp), invert=FALSE)


# Euclidean distance matrix on measurements
d <- dist(cran_hom[, measure_cols], method = "euclidean")
dmat <- as.matrix(d)

# PCoA using labdsv::pco
# k = number of axes returned (<= nrow-1); here we request up to 11 
pcoa <- pco(dmat, k = min(11, nrow(cran_hom) - 1))

pcoa_scores <- as.data.frame(pcoa$points)
pcoa_scores$spp <- cran_hom$spp

ggplot(pcoa_scores, aes(x = V1, y = V2, color = spp)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "PCoA (Euclidean Distance) of Cranial Measurements",
    x = "PCoA 1",
    y = "PCoA 2",
    color = "Species code"
  ) +
  coord_equal()
#Distance-based ordination reveals clear multivariate structuring of cranial measurements, with many taxa forming distinct clusters and a smaller subset showing partial overlap in the central region of morphospace.

# ============================================================
# 3) Partial Least Squares (PLS)
# Question: Do palate/face measures covary strongly with cranial measures?
# ============================================================

# PLS: Y block = cranial; X block = palate + face
# (Uses formula interface from pls::plsr)
pls_fit <- plsr(
  cranht + cranwd + cranlg ~ paltlg + paltwd + faceht + facewd,
  data = cranial,
  validation = "none"
)

summary(pls_fit)

# Extract scores for the first two components
sX <- pls_fit$scores   # X scores
sY <- pls_fit$Yscores  # Y scores

pls_scores_1 <- data.frame(
  X_comp1 = sX[, 1],
  Y_comp1 = sY[, 1],
  spp = cranial$spp
)

pls_scores_2 <- data.frame(
  X_comp2 = sX[, 2],
  Y_comp2 = sY[, 2],
  spp = cranial$spp
)

p_pls1 <- ggplot(pls_scores_1, aes(x = X_comp1, y = Y_comp1, color = spp)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "PLS: Component 1 (Palate/Face vs Cranial)",
    x = "X scores (palate/face) — Comp 1",
    y = "Y scores (cranial) — Comp 1",
    color = "Species code"
  ) +
  coord_equal()

p_pls2 <- ggplot(pls_scores_2, aes(x = X_comp2, y = Y_comp2, color = spp)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "PLS: Component 2 (Palate/Face vs Cranial)",
    x = "X scores (palate/face) — Comp 2",
    y = "Y scores (cranial) — Comp 2",
    color = "Species code"
  ) +
  coord_equal()

print(p_pls1)
#Component 1 captures the dominant shared covariation between facial/palatal and cranial blocks; scores show a strong positive relationship consistent with strong integration along this primary axis.

print(p_pls2)
#Component 2 reflects secondary integration structure beyond the dominant axis, with more localized separation and greater overlap among taxa compared to Component 1.

# Optional: if you want 2 plots side-by-side, use patchwork (cleaner than gridExtra)
# install.packages("patchwork")
# library(patchwork)
# p_pls1 + p_pls2

# ---- End of script ----