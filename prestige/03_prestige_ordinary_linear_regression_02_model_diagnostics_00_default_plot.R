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
# default model diagnostics plot
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(linmod)

