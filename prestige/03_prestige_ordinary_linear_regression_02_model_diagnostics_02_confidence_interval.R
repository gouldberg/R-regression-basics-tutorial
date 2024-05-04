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
# model diagnostics:  confidence interval
# ------------------------------------------------------------------------------

confint(linmod)



# Wald confidence interval (based on asymptotic normality)

confint.default(linmod, level = 0.95)



# ----------
car::Confint(linmod)



# -->
# Note that confidence interval of "women"'s coefficients include zero value


