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




# ------------------------------------------------------------------------------
# excluding educaiton:type
# ------------------------------------------------------------------------------

linmod4 <- update(linmod3, ~ . - education:type, data = data)


summary(linmod4)


anova(linmod4)

car::Anova(linmod4)




# ------------------------------------------------------------------------------
# if theory exists, the the coefficient
# ------------------------------------------------------------------------------


coef(linmod4)


car::linearHypothesis(linmod4, c("(Intercept) = -110", "education = 3.2"))


car::linearHypothesis(linmod4, c("(Intercept) = -110", "education = 5"))




# ------------------------------------------------------------------------------
# model diagnostics:  confidence interval
# ------------------------------------------------------------------------------

car::Confint(linmod4)




# ------------------------------------------------------------------------------
# model understanding:  Main effect plot
# ------------------------------------------------------------------------------

library(effects)


plot(predictorEffects(linmod4))

predictorEffects(linmod4)




# ----------
# effect plot for interaction

plot(predictorEffects(linmod3, ~ education), lines = list(multiline = TRUE, lty = 1:3, lwd = 2))



plot(predictorEffects(linmod4, ~ log2(income)), lines = list(multiline = TRUE, lty = 1:3, lwd = 2))

plot(predictorEffects(linmod4, ~ type), xlevels = list(income = c(1000, 5000, 8000, 12000)),
     lines = list(multiline = TRUE), confint = list(style = "bars"))




