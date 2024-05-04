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
linmod <- lm(prestige ~ education + log2(income) + women, data = data)




# ------------------------------------------------------------------------------
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

resi <- residuals(linmod)
# resi <- resid(linmod)


summary(resi)



car::densityPlot(resi)

# hist(resi)



# ----------
car::residualPlots(linmod)



# -->
# there remains some curvatures ...
# espeically "women" lack of fit
# Tukey's test for non-additivity (adding the squares fo the fitted values to the model and refitting):  still no-problem



# ----------
# residual plot by group

residualPlot(linmod, groups = data$type)





# ------------------------------------------------------------------------------
# Identify large residuals
# ------------------------------------------------------------------------------


stand.resid <- rstandard(linmod)


which.max(stand.resid)



# ----------
par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized residuals", type = "h")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))





# ------------------------------------------------------------------------------
# check for normality
# ------------------------------------------------------------------------------

qqnorm(resi)
qqline(resi)



car::qqPlot(stand.resid, xlab = "Normal Quantiles", ylab = "Standardized residuals")



# ----------
# by group
car::qqPlot(stand.resid, group = data$type, xlab = "Normal Quantiles", ylab = "Studentized residuals")



