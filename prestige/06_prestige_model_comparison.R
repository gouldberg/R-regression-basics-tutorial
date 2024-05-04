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
linmod2 <- lm(prestige ~ education + log2(income) + type, data = data)
linmod3 <- lm(prestige ~ education + log2(income) + type + type : education + type : log2(income), data = data)
linmod4 <- lm(prestige ~ education + log2(income) + type + type : log2(income), data = data)

lmod_leaps <- lm(prestige ~ educatoin * log2(income) + type + women + type : log2(income) + type : women, data = data)
lmod_step <- lm(prestige ~ education + log2(income) + type + women + type : log2(income) + type : women, data = data)



# ----------
# before transformation
linmod5 <- lm(prestige ~ education + income + type + income:type, data = data)

summary(linmod4)
summary(linmod5)




# ------------------------------------------------------------------------------
# model comparison: terms and coefficient
# ------------------------------------------------------------------------------

stargazer::stargazer(linmod, linmod2, linmod3, linmod4, linmod5, lmod_leaps, lmod_step, type = "text")


car::compareCoefs(linmod, linmod2, linmod3, linmod4, linmod5, lmod_leaps, lmod_step)



# -->
# linmod5 may be best  (the significance of income:type is better)


# lmod_leaps and lmod_step are DIFFICULT FOR MODEL INTERPRETATION !!!




# ------------------------------------------------------------------------------
# model comparison by likelihood-ratio tests and the analysis of variance:  ONLY APPLICABLE TO NETSTED MODELS
# ------------------------------------------------------------------------------


# intercept only model
linmod0 <- update(linmod, . ~ 1)

summary(linmod0)



# ----------
# linmod is better than intercept only model ?  --> better

anova(linmod0, linmod)



# ----------
# linmod4 is better than linmod2 ?  --> BETTER

anova(linmod2, linmod4)



# ----------
# linmod3 is better than linmod4 ?  --> NOT MUCH BETTER

anova(linmod4, linmod3)




# ----------

Anova(lmod_leaps)

Anova(lmod_step)




# ------------------------------------------------------------------------------
# model comparison by AIC:  APPLICABLE TO NON-NESTED MODELS
# ------------------------------------------------------------------------------

AIC(linmod, linmod2, linmod3, linmod4, linmod5, lmod_leaps, lmod_step)



# -->
# linmod4 and linmod3, linmod5 also be chosen ...

# lmod_step is best... but DIFFICULT FOR INTERPRETATION



