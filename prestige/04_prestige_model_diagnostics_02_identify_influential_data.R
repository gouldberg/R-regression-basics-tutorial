rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics\\prestige")



# ------------------------------------------------------------------------------
# data:  Prestige
# ------------------------------------------------------------------------------

data <- read.csv("Prestige.txt", header = T, sep = "\t")


str(data)


car::some(data)



# ----------
# prepare levels

table(data$type, useNA = "always")

data$type <- factor(data$type, levels = c("bc", "wc", "prof"))

levels(data$type)




# ----------
linmod <- lm(prestige ~ education + log2(income) + women, data = data)
linmod2 <- update(linmod, ~ . - women + type)
linmod3 <- update(linmod2, ~ . + education:type + log2(income):type, data = data)
linmod4 <- update(linmod3, ~ . - education:type, data = data)




# ------------------------------------------------------------------------------
# model diagnostics:  Studentized (deletion) residuals by index
# ------------------------------------------------------------------------------

# standardized Residuals
stand.resid4 <- rstandard(linmod4)

# studentized Residuals
stud.resid4 <- rstudent(linmod4)



# ----------
par(mfrow=c(2,1))
plot(stand.resid4, ylim = c(min(-3, stand.resid4), max(3, stand.resid4)), main = "Standardized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

plot(stud.resid4, ylim = c(min(-3, stud.resid4), max(3, stud.resid4)), main = "Studentized residuals", type = "h")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))



# ----------
car::influenceIndexPlot(linmod4, vars = c("studentized"))



# ------------------------------------------------------------------------------
# car::OutlierTest()
#  - give a formal test of significance of the largest absolute studentized residuals, witha Bonferroni-adjusted p-value accounting
#    for choosing the largest values among n such tests.
# ------------------------------------------------------------------------------

car::outlierTest(linmod4)




# ------------------------------------------------------------------------------
# model diagnostics:  Studentized (deletion) residuals vs. leverage, showing the value of Cook's distance by the area of the bubble symbol
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
influencePlot(linmod4)



# ----------
op <- par(mar = c(5, 4, 1, 1) + .1, cex.lab = 1.2)
res <- car::influencePlot(linmod4, scale = 8)
k <- length(coef(linmod4))
n <- nrow(data)
text(x = c(2, 3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)



# ----------
# show data together with diagnostics for influential cases
idx <- which(rownames(data) %in% rownames(res))

cbind(data[idx,], res)



# -->
# general.managers and pysicians:  very high in income
# medical.technicians:  education is high (type = wc) but income with lower than average, but high prestige
# farm.workers:  prestige is very low
# electronic.workers:  education is low, but prestige is higher than mean




# ------------------------------------------------------------------------------
# The collection of index plot
#  - plots various influence diagnostics against the observation numbers in the data
#  - id.n argument here works to label that number of the most extreme observations individually for each measure plotted.
# ------------------------------------------------------------------------------

car::influenceIndexPlot(linmod4, vars = c("Cook", "studentized", "hat"), id.n = 4)


