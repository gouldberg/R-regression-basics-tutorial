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




# ------------------------------------------------------------------------------
# Multiple linear regression
# ------------------------------------------------------------------------------


linmod <- lm(prestige ~ education + log2(income) + women, data = data)


summary(linmod)



# -->
# degrees of freedom:
# 102 - (1 + 3)



# ----------
# goodness of fit (GOF) = Multiple R-squared = 0.8351
# adjusted:  0.83


# "women" are not significant
