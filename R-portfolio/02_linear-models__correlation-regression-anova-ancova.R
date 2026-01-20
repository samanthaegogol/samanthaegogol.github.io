# ============================================================
# Linear Models Workflow in R (Portfolio)
# Topics: feature engineering, correlation, regression, t-test,
#         ANOVA + post-hoc, ANCOVA
# Author: Samantha E. Gogol
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)

# ---- Load data ----
master <- read.csv("master_portfolio.csv", , header = TRUE)


# ---- Helper: Euclidean distance between two 3D points ----
dist3 <- function(ax, ay, az, bx, by, bz) {
  sqrt((ax - bx)^2 + (ay - by)^2 + (az - bz)^2)
}

# ---- Feature engineering: linear distances from landmark triplets ----
master <- master %>%
  mutate(
    cranht = dist3(var145, var146, var147, var49,  var50,  var51),
    cranwd = dist3(var4,   var5,   var6,   var97,  var98,  var99),
    cranlg = dist3(var55,  var56,  var57,  var1,   var2,   var3),
    faceht = dist3(var82,  var83,  var84,  var58,  var59,  var60),
    facewd = dist3(var10,  var11,  var12,  var103, var104, var105),
    aptrht = dist3(var61,  var62,  var63,  var79,  var80,  var81),
    aptrwd = dist3(var64,  var65,  var66,  var67,  var68,  var69),
    rorbht = dist3(var28,  var29,  var30,  var19,  var20,  var21),
    rorbwd = dist3(var16,  var17,  var18,  var25,  var26,  var27),
    paltlg = dist3(var82,  var83,  var84,  var157, var158, var159),
    paltwd = dist3(var175, var176, var177, var190, var191, var192)
  )

# Create new csv with new variables
master.2 <- select(master, cat,cranht,cranwd,cranlg,facewd,faceht,aptrwd,aptrht,rorbwd,rorbht,paltwd,paltlg)

# ------------------------------------------------------------
# 1) Correlation + EDA
# Question: Are palate length and face height strongly correlated?
# ------------------------------------------------------------

#x axis palate length, y axis face height
pal.face <- ggplot(master.2, aes(x = paltlg, y = faceht)) +
  geom_point() +
  labs(
    title = "Palate Length vs Face Height",
    x = "Palate Length",
    y = "Face Height"
  )
#view plot
print(pal.face)

#Calculate correlation coefficient for palate length and face height, closer to 1 == strongly correlated
r <- cor(master.2$paltlg, master.2$faceht, use = "complete.obs")
r

#calculate % of variation 
r2 <- r^2
r2

# Covariance matrix for measurement variables (excluding ID columns)
master.3 <- select(master.2, cranht,cranwd,cranlg,facewd,faceht,aptrwd,aptrht,rorbwd,rorbht,paltwd,paltlg)
cov<- cov(master.3)
#view degree to which each variable covaries
cov

# ------------------------------------------------------------
# 2) Linear regression of palate length and face height
# Model: paltlg ~ faceht
# ------------------------------------------------------------

lm_a <- lm(paltlg ~ faceht, data = master.2)
summary(lm_a)

# Plot with regression line answering question:  there is a positive linear relationship between palate length and face height?
plot(master.2$faceht, master.2$paltlg,
     pch = 19,
     xlab = "Face Height",
     ylab = "Palate Length",
     main = "Linear Regression: Palate Length ~ Face Height")
abline(lm_a, lwd = 2)

# Diagnostics
par(mfrow = c(2, 2))
plot(lm_a, pch = 19)
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# 3) Two-sample t-test
# Question: Do male and female gorillas differ in face height?
# ------------------------------------------------------------

master.2$sp <- substr(master.2$cat,1, 2)
gor <- master.2[master.2$sp %in% "Gg", ]
gor$sex <- substr(gor$cat,4, 4)

par(mfrow = c(1, 1))
boxplot(faceht ~ sex, data = gor,
        main = "Gorilla Face Height by Sex",
        xlab = "Sex",
        ylab = "Face Height")

t_gor <- t.test(faceht ~ sex, data = gor, alternative = "two.sided")
t_gor

# ------------------------------------------------------------
# 4) One-way ANOVA + post-hoc
# Question: Does facial width differ among Gorilla, Chimpanzee, Bonobo?
# spp codes: Gg, Pt, Pp
# ------------------------------------------------------------

AFapes <- master.2[master.2$sp %in% c("Gg", "Pt", "Pp"), ]

boxplot(facewd ~ sp, data = AFapes,
        main = "Facial Width by Species",
        xlab = "Species",
        ylab = "Facial Width")

#ANOVA
aov1 <- aov(facewd ~ sp, data = AFapes)
summary(aov1)

# Pairwise comparisons (Bonferroni-adjusted)
pairwise.t.test(AFapes$facewd, AFapes$sp, p.adjust.method = "bonf")

# Tukey HSD
TukeyHSD(aov1)
plot(TukeyHSD(aov1))

# ------------------------------------------------------------
# 5) ANCOVA
# Question: Does sex predict face height when controlling for cranial height?
# Example: Orangutans (spp == "Po")
# ------------------------------------------------------------

orang <- master.2[master.2$sp %in% "Po", ]
orang$sex <- substr(orang$cat,4, 4)

ancova_int <- lm(faceht ~ sex * cranht, data = orang)  # with interaction
ancova_add <- lm(faceht ~ sex + cranht, data = orang)  # without interaction

summary(ancova_int)
summary(ancova_add)

with(orang, plot(cranht, faceht, col = as.factor(sex), pch = 19,
                 xlab = "Cranial Height",
                 ylab = "Face Height",
                 main = "ANCOVA: Face Height vs Cranial Height by Sex"))
legend("topleft", legend = levels(as.factor(orang$sex)),
       col = seq_along(levels(as.factor(orang$sex))), pch = 19)

# ---- End of script ----