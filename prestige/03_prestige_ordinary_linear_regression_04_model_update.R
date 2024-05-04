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



# ----------
# prepare levels

table(data$type, useNA = "always")

data$type <- factor(data$type, levels = c("bc", "wc", "prof"))

levels(data$type)




# ------------------------------------------------------------------------------
# delete "women" and add "type"
# ------------------------------------------------------------------------------


linmod2 <- update(linmod, ~ . - women + type)


summary(linmod2)


# -->
# note that message "4 observations deleted due to missingsness


# -->
# degrees of freedom:
# 102 - 4 - (1 + 2 + 2)



# ----------
# goodness of fit (GOF) = Multiple R-squared = 0.8355
# adjusted:  0.8493


# type (each factor level) is not significant



# ------------------------------------------------------------------------------
# Model terms significance:  Sequential Analysis of Variance  (Type I analysis of variance)
# ------------------------------------------------------------------------------

anova(linmod2)


# 1. intercept only                         vs. + education
# 2. incercept + education                  vs. + log2(income)
# 3. incercept + education + log2(income)   vs. + type


# BUT type (as overall) is SIGNIFICANT by this test



# ----------
# for reference
anova(lm(prestige ~ type + log2(income) + education, data = data))




# ------------------------------------------------------------------------------
# Model terms significance:  Type II test
# ------------------------------------------------------------------------------

car::Anova(linmod2)


# 1. education not included  vs. linmod2
# 2. log2(income) not included  vs. linmod2
# 3. type not included  vs. linmod2


# type (as overall) is SIGNIFICANT by this test




# ------------------------------------------------------------------------------
# model diagnostics:  residuals
# ------------------------------------------------------------------------------

resi2 <- residuals(linmod2)
# resi2 <- resid(linmod2)


summary(resi2)



car::densityPlot(resi2)

# hist(resi2)



# ----------
car::residualPlots(linmod2)



# -->
# there remains some curvatures ...



# ----------
# residual plot by group

residualPlot(linmod2, groups = data$type)




# ------------------------------------------------------------------------------
# model diagnostics:  confidence interval
# ------------------------------------------------------------------------------

car::Confint(linmod2)




# ------------------------------------------------------------------------------
# model diagnostics:  standard erros for differences in adjusted means
# ------------------------------------------------------------------------------

library(emmeans)


emmeans(linmod2, pairwise ~ type)$contrasts




# ------------------------------------------------------------------------------
# Diagnostic plot:  Added-variable plots (partial-regression plots)
#  - Marginal plots of response variable against the predictor variables can conceal or misrepresent the relationships in a model
#    including several predictors together due to correlations or associations among the predictor.
#    Added-variable plots solve this problem by plotting the residuals which are less discrete than the marginal responses in Y
#  - Sets of two (or more) observations can have joint influence, which greatly exceeds their individual influential.
#    Similarly, the influence of one discrepant point can be offset by another influential point in an opposite direction, a phenomenon called masking.
#  - Added-variable plots, showing the partial regression for one predictor controlling or adjusting for all others, can make such cases visually apparent.
# ------------------------------------------------------------------------------

graphics.off()

car::avPlots(linmod2, cex.lab = 1.3, id = list(n = 4, cex = 1.2))



# -->
# the labeled points:  either large absolute model residuals or extreme x residuals, given all other predictors. 
# the solid red line:  partial slope beta for the focal predictor, controlling for all others.




# ------------------------------------------------------------------------------
# model understanding:  Main effect plot
# ------------------------------------------------------------------------------

library(effects)


plot(predictorEffects(linmod2))

predictorEffects(linmod2)



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve

plot(predictorEffects(linmod2, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))



