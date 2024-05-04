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




# ------------------------------------------------------------------------------
# model update incorporating interaction
# ------------------------------------------------------------------------------

# we apply ":" not "*"


some(model.matrix(~ type + education + education:type + log2(income):type, data = data), 8)


linmod3 <- update(linmod2, ~ . + education:type + log2(income):type, data = data)


summary(linmod3)



# -->
# degrees of freedom:
# 102 - 4 - (1 + 2 + 2 * 3)



# ----------
# goodness of fit (GOF) = Multiple R-squared = 0.871
# adjusted:  0.8595



# ------------------------------------------------------------------------------
# Model terms significance:  Sequential Analysis of Variance  (Type I analysis of variance)
# ------------------------------------------------------------------------------

anova(linmod3)


# 1. intercept only                         vs. + education
# 2. incercept + education                  vs. + log2(income)
# 3. incercept + education + log2(income)   vs. + type
# 4. incercept + education + log2(income) + type   vs. + education:type
# 5. incercept + education + log2(income) + type + education:type   vs. + log2(income):type


# education:type IS NOT SIGNIFICANT
# log2(income):type IS SIGNIFICANT




# ------------------------------------------------------------------------------
# Model terms significance:  Type II test
# ------------------------------------------------------------------------------

car::Anova(linmod3)


# 1. education not included  vs. linmod3
# 2. log2(income) not included  vs. linmod3
# 3. type not included  vs. linmod3
# 4. education:type not included  vs. linmod3
# 5. log2(income):type not included  vs. linmod3


# education:type IS NOT SIGNIFICANT,  but marginally ... (p-value = 0.12)


