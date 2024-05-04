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
# Data Exploration:  data transformation for "income"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ income, data = data)



# transforming for symmetry  --> log transform is good !!
car::symbox(~ income, data = data)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(income ~ 1, data = data, family = "bcPower")

summary(p1)


car::testTransform(p1, lambda = 0)



# ---------
# transform by log (either of log10, log2, log)
tmp <- data %>% mutate(income2 = log(income))

# tmp <- data %>% mutate(income2 = income^(0.2))

car::densityPlot( ~ income2, data = tmp)




# ----------
# by group

formula <- ~ education + income2 + women + prestige | type

scatterplotMatrix(formula, data = tmp, cex = 1.2,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black", "blue"), pch = 1:3)



