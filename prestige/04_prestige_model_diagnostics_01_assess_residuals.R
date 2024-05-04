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
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

resi4 <- residuals(linmod4)
# resi4 <- resid(linmod4)


summary(resi4)



car::densityPlot(resi4)

# hist(resi4)



# ----------
car::residualPlots(linmod4)



# lack of fit test --> no problem
# Tukey's test for non-additivity (adding the squares fo the fitted values to the model and refitting):  no-problem



# ----------
# residual plot by group

residualPlot(linmod2, groups = data$type)
residualPlot(linmod4, groups = data$type)




# ------------------------------------------------------------------------------
# model diagnostics:  curvature test
# ------------------------------------------------------------------------------

# residuals against each predictor to check systematic variation
# quadratic = TRUE:  computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero

par(mfrow = c(1,1))

car::residualPlot(linmod4, "education", type = "rstandard", 
                  quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5, 5))


car::residualPlot(linmod4, "log2(income)", type = "rstandard", 
                  quadratic = TRUE, col.quad = gray(0.7), ylim = c(-5, 5))




