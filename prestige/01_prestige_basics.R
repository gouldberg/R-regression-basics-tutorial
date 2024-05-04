rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\04_regression_basics\\prestige")



# ------------------------------------------------------------------------------
# data:  Prestige
#   - Occupational prestige scores for 102 Canadian occupations in the mid-1960s
#   - including potential predictors of occupational prestige scores:
#      - average years of education of incumbents in each occupation in 1970
#      - average income of occupation
#      - percentage of women in the occupation
#      - type of occupation, a factor with levels "bc" (blue color), "prof" (professional or managerial), and "wc" (white collar)
#     The first 3 predictors are from the 1971 Canadian Census.
# ------------------------------------------------------------------------------

data <- read.csv("Prestige.txt", header = T, sep = "\t")


str(data)


car::some(data)

