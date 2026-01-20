# ============================================================
# 01 Foundations (Portfolio): Data import + wrangling + summary stats
# Dataset: cranial measurements (course dataset; values may be perturbed for portfolio use)
# Author: Samantha E. Gogol
# ============================================================

# ---- Setup ----
rm(list = ls())
options(stringsAsFactors = FALSE)
##setwd()

# ---- Load data ----
df <- read.csv("cranial_foundations_portfolio.csv", header = TRUE)

# Inspect
dim(df)
head(df)

# ---- Standardize column names ----
colnames(df) <- c(
  "cat", "cranht", "cranwd", "cranlg",
  "faceht", "facewd", "aptrht", "aptrwd",
  "rorbht", "rorbwd", "paltlg", "paltwd"
)

# ---- Create derived variables ----
# Order and a simple taxon code
df$order <- "Primates"
df$taxonnum <- 3

# Sex code extracted from 4th character of 'cat'
df$sex <- substr(df$cat, 4, 4)

# Numeric sex coding:
# f = 1, m = 0, u/other = NA
df$sexn <- NA_integer_
df$sexn[df$sex == "f"] <- 1L
df$sexn[df$sex == "m"] <- 0L

# Cranial index: cranial height / cranial length
df$cranid <- df$cranht / df$cranlg

# ---- Summary statistics (cranid) ----
cranid_summary <- summary(df$cranid)
cranid_sd <- sd(df$cranid, na.rm = TRUE)
cranid_var <- var(df$cranid, na.rm = TRUE)
cranid_quant <- quantile(df$cranid, na.rm = TRUE)

cranid_summary
cranid_sd
cranid_var
cranid_quant

# Mode helper (base R)
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
cranid_mode <- get_mode(df$cranid)
cranid_mode

# ---- Taxon labels via lookup table ----
# 3-letter genus/species code from 'cat'
df$code3 <- substr(df$cat, 1, 3)

taxon_lookup <- data.frame(
  code3 = c("Cms","Ggb","Ggg","Ggr","Hsp","Haa","Hal","Hau","Hcc","Hcg","Hcl","Hll",
            "Hma","Hmf","Hss","Pbb","Pbl","Pbn","Pbo","Pbr","Pbt","Pha","Poa","Pop",
            "Ppn","Pts","Ptt","Ptv"),
  taxon = c("Cercopithecus mitis stuhlmanni",
            "Gorilla gorilla beringei",
            "Gorilla gorilla gorilla",
            "Gorilla gorilla graueri",
            "Homo sapiens sapiens",
            "Hylobates agilis agilis",
            "Hylobates agilis albibarbis",
            "Hylobates agilis unko",
            "Hylobates concolor concolor",
            "Hylobates concolor gabriellae",
            "Hylobates concolor leucogenys",
            "Hylobates lar lar",
            "Hylobates mulleri abbotti",
            "Hylobates mulleri funereus",
            "Hylobates syndactylus syndactylus",
            "Procolobus badius badius",
            "Procolobus badius langi",
            "Procolobus badius nigrimanus",
            "Procolobus badius oustaleti",
            "Procolobus badius rufomitratus",
            "Procolobus badius tholloni",
            "Papio hamadryas anubis",
            "Pongo pygmaeus abelii",
            "Pongo pygmaeus pygmaeus",
            "Pan paniscus",
            "Pan troglodytes schweinfurthii",
            "Pan troglodytes troglodytes",
            "Pan troglodytes verus"),
  stringsAsFactors = FALSE
)

# Merge labels onto main df
df <- merge(df, taxon_lookup, by = "code3", all.x = TRUE)

# ---- Subset to apes of interest ----
apes <- c(
  "Pan paniscus",
  "Pan troglodytes schweinfurthii",
  "Pan troglodytes troglodytes",
  "Pan troglodytes verus",
  "Gorilla gorilla beringei",
  "Gorilla gorilla gorilla",
  "Gorilla gorilla graueri"
)

df_apes <- df[df$taxon %in% apes, ]

# Summary stats for subset
summary(df_apes$cranid)
sd(df_apes$cranid, na.rm = TRUE)
var(df_apes$cranid, na.rm = TRUE)
get_mode(df_apes$cranid)
quantile(df_apes$cranid, na.rm = TRUE)

# ---- Group means: cranid by sex and genus (2-letter code) ----
df_apes$gen2 <- substr(df_apes$code3, 1, 2)

aggregate(cranid ~ sex + gen2, data = df_apes, FUN = mean)

# ---- End of script ----