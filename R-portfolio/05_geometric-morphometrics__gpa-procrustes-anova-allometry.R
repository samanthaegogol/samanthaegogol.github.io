# ============================================================
# Geometric Morphometrics in R (Portfolio)
# Topics: arrayspecs, GPA (gpagen), centroid size, permudist,
#         Procrustes ANOVA (procD.lm), allometry (procD.allometry),
#         tangent space PCA (plotTangentSpace) + 2D visualization
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
#setwd()

library(geomorph)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rgl)
library(Morpho)

# ---- Load data ----
master1 <- read.csv("master_portfolio.csv")

# ---- Metadata variables ----
rows <- master1$cat
#removing discrete vars from spreadsheet
master <- master1[ ,-1]
#set rownames as removed column
rownames(master) <- rows
#View(master)

# ---- Convert wide landmark table -> geomorph array ----
# Assumes columns: cat + 195 coordinate columns (65 landmarks * 3 coords)
coord_df <- master[, setdiff(names(master), c("cat", "genus", "species", "spp", "sex"))]

k <- 3
p <- ncol(coord_df) / k
stopifnot(p == round(p))  # must be an integer number of landmarks

coords_array <- arrayspecs(coord_df, p = p, k = k)

# ============================================================
# 1) GPA: align shapes into shape space
# ============================================================

gpa <- gpagen(coords_array)
coords <- gpa$coords
CS <- gpa$Csize

# Optional quick QC plot (can be slow / uses rgl-like device depending on settings)
# plotAllSpecimens(coords)


# ============================================================
# 2) Group differences in shape (permutation distances)
# ============================================================

#isolate variables
master1$genus <- substr(master1$cat, 1,1)
master1$species <- substr(master1$cat, 2,2)
master1$spp <- substr(master1$cat, 1, 2)
master1$sex <- substr(master1$cat, 4,4)
# Permutation test on Procrustes distances between groups
pop_diffs <- permudist(coords, groups = master1$spp, rounds = 1000)
pop_diffs

# ============================================================
# 3) Procrustes ANOVA: species and sex effects on shape
# ============================================================

gdf <- geomorph.data.frame(
  shape = coords,
  spp = as.factor(master1$spp),
  sex = as.factor(master1$sex),
  genus = as.factor(master1$genus),
  size = CS
)

# Species identity
panova_spp <- procD.lm(shape ~ spp, data = gdf, iter = 999)
summary(panova_spp)

# Sex effect
panova_sex <- procD.lm(shape ~ sex, data = gdf, iter = 999)
summary(panova_sex)

# ============================================================
# 4) Allometry: shape ~ centroid size
# ============================================================

allo <- procD.lm(shape ~ spp, data = gdf, iter = 999)
summary(allo)

# Regression score plot (safe, 2D)
plot(allo, method = "RegScore", pch = 19,
     main = "Allometry: Shape vs Centroid Size")

# ============================================================
# 5) Tangent space PCA (shape ordination) — geomorph >= 3.3.0
# ============================================================

# PCA in tangent space from GPA-aligned coordinates
gmpca <- gm.prcomp(coords)

summary(gmpca)      # variance explained, etc.
# plot(gmpca)       # base R plot method (optional)

# Scores for ggplot
pcscores <- as.data.frame(gmpca$x)

# Attach grouping variables (use master1, not master)
pcscores$spp <- master1$spp
pcscores$sex <- master1$sex

# Variance explained for axis labels
propvar <- (gmpca$sdev^2) / sum(gmpca$sdev^2)

pc1_lab <- paste0("PC1 (", round(100 * propvar[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * propvar[2], 1), "%)")
pc3_lab <- paste0("PC3 (", round(100 * propvar[3], 1), "%)")

# PC1 vs PC2
ggplot(pcscores, aes(x = Comp1, y = Comp2, color = spp, shape = sex)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Tangent Space PCA of Cranial Shape",
    x = pc1_lab,
    y = pc2_lab,
    color = "Species code",
    shape = "Sex"
  ) +
  coord_equal()

# PC2 vs PC3
ggplot(pcscores, aes(x = Comp2, y = Comp3, color = spp, shape = sex)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Tangent Space PCA (PC2 vs PC3)",
    x = pc2_lab,
    y = pc3_lab,
    color = "Species code",
    shape = "Sex"
  ) +
  coord_equal()


#3D Plot
library(rgl)

# pcscores is a data.frame of scores from gm.prcomp (or your PCA)
# Ensure numeric:
x <- pcscores[, 1]
y <- pcscores[, 2]
z <- pcscores[, 3]

# --- Map colors by species (robust, no brewer limits) ---
spp_fac <- factor(master1$spp)
cols <- grDevices::hcl.colors(nlevels(spp_fac), "Dark 3")  # base R, many distinct colors
spp_col <- cols[as.integer(spp_fac)]

# --- Map shapes by sex ---
sex_fac <- factor(master1$sex)
pch_vals <- c(17, 19, 4)  # triangle, circle, x
sex_pch <- pch_vals[as.integer(sex_fac)]

# --- Plot ---
open3d()
plot3d(
  x, y, z,
  col = spp_col,
  type = "s",        # spheres (more visible than points)
  radius = 0.02,     # adjust if needed
  xlab = "PC1", ylab = "PC2", zlab = "PC3"
)

# --- Legend (separate call!) ---
legend3d(
  "topright",
  legend = levels(spp_fac),
  pch = 16,
  col = cols,
  cex = 1
)


# ============================================================
# 6) Optional: subset example (Gibbons / Hylobatids) + GPA
# Mirrors your Lab 5 subset logic but in a cleaner form.
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)


master3 <- read.csv('master_portfolio.csv')
master3$spp <- substr(master3$cat, 1,2)
#Extract hylobates
sub_mast <- master3[master3$spp %in% c("Hs","Hm", "Hl","Hc", "Ha"), ]
cat <- sub_mast$cat
rownames(sub_mast) <- cat
#remove the cat and spp variables
gib.dat <- sub_mast[ ,c(-1,-197)]
View(gib.dat)

#Prep for gapgen
k <- 3 
p <- 195/k
array2<- arrayspecs(gib.dat, p, k)

gpa2<-gpagen(array2)
coords_gib <- gpa2$coords

# Optional QC plot
#plotAllSpecimens(coords2) 

# Tangent space PCA (geomorph >= 3.3)
gmpca_gib <- gm.prcomp(gpa2$coords)

# Scores
pcs_gib <- as.data.frame(gmpca_gib$x)
colnames(pcs_gib) <- paste0("PC", seq_len(ncol(pcs_gib)))


pcs_gib$spp <- sub_mast$spp
sub_mast$sex <- substr(sub_mast$cat, 4,4)
pcs_gib$sex <- sub_mast$sex

# Variance explained
propvar_gib <- (gmpca_gib$sdev^2) / sum(gmpca_gib$sdev^2)
pc1_lab <- paste0("PC1 (", round(100 * propvar_gib[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * propvar_gib[2], 1), "%)")

# 2D plot
ggplot(pcs_gib, aes(x = PC1, y = PC2, color = spp, shape = sex)) +
  geom_point(alpha = 0.85) +
  labs(
    title = "Tangent Space PCA (Gibbons subset)",
    x = pc1_lab,
    y = pc2_lab,
    color = "Species code",
    shape = "Sex"
  ) +
  coord_equal()

# ---- End of script ----
