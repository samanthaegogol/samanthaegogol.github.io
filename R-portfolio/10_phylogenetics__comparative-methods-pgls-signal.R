# ============================================================
# Phylogenetic Morphometrics (Portfolio)
# Topics: pruning a phylogeny, GPA on landmarks, species mean shapes,
#         phylogenetic signal, ancestral state reconstruction (size),
#         morphospace with projected ancestral shapes, phylogenetic PCA,
#         projecting individuals into phylogenetic PC space
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

library(geomorph)
library(phytools)
library(ape)

# ---- Inputs ----
tree_file  <- "SpringerRAxML_portfolio.tre"
data_file  <- "master2018_portfolio.csv"

# ============================================================
# 1) Load tree and data; prune to matched taxa
# ============================================================

tree <- read.tree(tree_file)

master <- read.csv(data_file, header = TRUE)

# Expect first three columns are: specimen ID / PhyloSpp / something else
stopifnot("PhyloSpp" %in% names(master))

tips <- tree$tip.label
data_taxa <- unique(master$PhyloSpp)

# Tips in tree but not in data -> drop
drop_taxa <- setdiff(tips, data_taxa)

sub.tree <- drop.tip(tree, drop_taxa)
sub.tree <- ladderize(sub.tree)

sub.tips <- sub.tree$tip.label

# Keep only rows in data that belong to the pruned tree
master <- master[master$PhyloSpp %in% sub.tips, ]

# ============================================================
# 2) GPA on landmark coordinates; convert to specimen x (p*k)
# ============================================================

k <- 3

# Landmark columns: 4:198 in your lab file (195 coordinates)
coord_cols <- 4:198
stopifnot(length(coord_cols) %% k == 0)

p <- length(coord_cols) / k
coords_array <- arrayspecs(master[, coord_cols], p = p, k = k)

gpa <- gpagen(coords_array)
shape <- gpa$coords
csize <- gpa$Csize

# Convert Procrustes coords to specimen x (p*k) matrix
# aperm -> (specimen, landmark, dim), then reshape
coords <- aperm(shape, c(3, 1, 2))
dim(coords) <- c(dim(shape)[3], p * k)

# Combine metadata + coords (handy for aggregation)
new.mast <- cbind(master[, 1:3], coords)

# ============================================================
# 3) Species mean shapes (Procrustes) + family labels
# ============================================================

means <- aggregate(new.mast[, 4:ncol(new.mast)],
                   list(PhyloSpp = new.mast$PhyloSpp),
                   FUN = mean)

rownames(means) <- means$PhyloSpp
means_mat <- means[, -1, drop = FALSE]   # species x (p*k)

# Family labels:
# If your master has a Family column, use it (preferred).
# Otherwise, supply a lookup table explicitly.
if ("Fam" %in% names(master)) {
  fam_lookup <- aggregate(master$Fam, list(PhyloSpp = master$PhyloSpp), function(x) x[1])
  fam <- fam_lookup$x
  names(fam) <- fam_lookup$PhyloSpp
  fam <- fam[rownames(means_mat)]
} else {
  # Replace this lookup with your own if Fam isn't in the CSV.
  # (I’m avoiding hard-coded ordering-by-hand.)
  fam <- rep(NA_character_, nrow(means_mat))
  names(fam) <- rownames(means_mat)
}

# Keep a species-mean data frame for convenience
means_df <- as.data.frame(means_mat)
means_df$Fam <- fam

# ============================================================
# 4) Phylogenetic signal in mean shape
# ============================================================

# physignal expects numeric matrix and a tree with matching tip labels
physig <- physignal(as.matrix(means_mat), sub.tree)
physig

# Tests whether species mean shapes covary with phylogeny.
# Observed K ≈ 0.71 (p = 0.001), indicating strong and significant
# phylogenetic signal and justifying phylogenetically informed analyses.

# ============================================================
# 5) Centroid size: ancestral state reconstruction + contMap
# ============================================================

# Mean centroid size per species
means_csize <- aggregate(csize, list(PhyloSpp = master$PhyloSpp), FUN = mean)
c.meanvec <- means_csize$x
names(c.meanvec) <- means_csize$PhyloSpp

# Order vector to match tree tips (important!)
c.meanvec <- c.meanvec[sub.tips]

anc.size <- ace(c.meanvec, sub.tree, type = "continuous")

# Contour map of centroid size across tree
contMap(sub.tree, c.meanvec, lwd = 4, fsize = 0.7)

# This plot visualizes how the trait/score varies across the tree via branch color.
# In the displayed output:
#   trait value scale ranges ~23.73 to ~60.381 (see legend)
#   branch length scale shown: length = 0.033
# Visually, closely related taxa share similar colors (clade-level continuity),
# consistent with the significant K reported above.


# ============================================================
# 6) GM morphospace (replacement for plotGMPhyloMorphoSpace)
# ============================================================

# Colors by family (only if Fam is known)
if (all(!is.na(means_df$Fam))) {
  fam_fac <- factor(means_df$Fam[sub.tips])
  fam_cols <- hcl.colors(nlevels(fam_fac), "Dark 3")
  fam.col <- fam_cols[as.integer(fam_fac)]
} else {
  fam.col <- "gray40"
}

# gm.prcomp on species mean shape variables
# means_mat is species x (p*k) matrix
gmpca <- gm.prcomp(means_mat)

# Summary + plot
summary(gmpca)

# mean.array: p x k x n_species (recommended for geomorph GM workflows)
gmpca <- gm.prcomp(means_mat, phy = sub.tree)

plot(gmpca,
     phylo = TRUE,
     pch = 19,
     col = fam.col,
     main = "GM PCA morphospace with phylogeny")

# This figure shows how the phylogeny traverses GM morphospace (PC1–PC2),
# connecting taxa according to the tree (projected into shape space).
# In the displayed plot, taxa cluster by major groups and the tree overlay
# provides a visual check that related taxa occupy nearby regions in morphospace.
# (Labels are dense/overlapping in this view; consider labeling only focal taxa.)

# ============================================================
# 7) Phylogenetic PCA on species means
# ============================================================

ppca <- phyl.pca(sub.tree, means_mat, method = "BM", mode = "cov")

# Proportion of variance explained
eig <- ppca$Eval
prop <- eig / sum(eig)

xlab <- paste0("pPC1 (", round(100 * prop[1], 1), "%)")
ylab <- paste0("pPC2 (", round(100 * prop[2], 1), "%)")

scores_sp <- ppca$S
labs <- rownames(scores_sp)

plot(scores_sp[,1], scores_sp[,2],
     xlab = xlab, ylab = ylab,
     main = "Phylogenetic PCA: species mean shapes",
     pch = 21, bg = "white", col = "gray20", cex = 1.1)

abline(h = 0, v = 0, lty = 3)

text(scores_sp[,1], scores_sp[,2],
     labels = labs,
     pos = 3, cex = 0.7)

# Summarizes major axes of interspecific shape variation.
# PC1 captures the dominant axis of shape differentiation,
# with subsequent PCs reflecting finer-scale morphological differences.

# ============================================================
# 8) Project individuals into phylogenetic PC space
# ============================================================

# Mean-center individual coords by column (tangent/procrustes coords already)
center_cols <- function(x) x - mean(x)
cent <- apply(coords, 2, center_cols)

# Multiply by eigenvectors from phyl.pca to get individual pPC scores
# (Evec is (p*k x p*k))
eigvec <- ppca$Evec
ind_scores <- cent %*% eigvec

# Combine metadata + first few pPCs
new.dat <- data.frame(master[, 1:3], ind_scores)

# Color by species (safe, no Brewer limits)
spp_fac <- factor(new.dat$PhyloSpp)
spp_cols <- hcl.colors(nlevels(spp_fac), "Dark 3")
spp.col <- spp_cols[as.integer(spp_fac)]

plot(new.dat[, 4], new.dat[, 5],
     col = spp.col, pch = 19, cex = 0.9,
     xlab = paste0("pPC1 (", round(100 * prop[1], 1), "%)"),
     ylab = paste0("pPC2 (", round(100 * prop[2], 1), "%)"),
     main = "Individuals projected into phylogenetic PC space")
abline(h = 0, v = 0, lty = 3)

legend("top", legend = levels(spp_fac), col = spp_cols, pch = 19, ncol = 4, cex = 0.6, bty = "n")

# pPCA scores (ppca$S) plotted on pPC1–pPC2 to summarize mean-shape variation
# after accounting for shared phylogenetic history (BM/cov).
# In the displayed output:
#   - Homo_sapiens is strongly separated along +pPC1 (far right)
#   - Papio_anubis is separated along negative pPC1 / negative pPC2 (far bottom-left)
#   - Several Hylobates taxa cluster in the upper portion of the plot
# Note: pPC2 variance is small; if axis labels show "0%" this may be rounding,
# not exactly zero variance.

# ============================================================
# 9) Example TPS comparisons (specimen indices should be data-driven)
# ============================================================

# Instead of hard-coding specimen numbers, pick extremes along pPC axes:
i_low  <- which.min(new.dat[, 5])  # most negative pPC2
i_high <- which.max(new.dat[, 5])  # most positive pPC2

plotRefToTarget(shape[, , i_low], shape[, , i_high],
                method = "vector",
                main = "Shape difference (pPC2 extreme: low → high)")


# This plot is a visual QC of Procrustes-aligned shape variation in 3D space.
# In the displayed view, the focal configuration appears closest to the Papio
# region of the scatter (qualitative similarity in this projection).
# Note: this is a visualization cue only; taxonomic affinity should be evaluated
# using the PCA/pPCA scores and formal classification/statistical comparisons.

# ---- End of script ----

