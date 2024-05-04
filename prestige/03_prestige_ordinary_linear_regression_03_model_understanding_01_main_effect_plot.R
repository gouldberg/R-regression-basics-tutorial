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
# Main effect plot
# ------------------------------------------------------------------------------

library(effects)


eff <- effects::allEffects(linmod)


eff



# ----------
# plot main effets of each variable

plot(eff)

plot(predictorEffects(linmod))

predictorEffects(linmod)



# ----------
# plot main effects with partial residuals for numeric variables with smoothed loess curve

plot(predictorEffects(linmod, residuals = TRUE), partial.residuals = list(cex = 0.35, col = gray(0.5), lty = 2))


