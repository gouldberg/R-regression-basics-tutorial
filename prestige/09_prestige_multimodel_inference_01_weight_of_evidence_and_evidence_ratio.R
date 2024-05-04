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
linmod3 <- lm(prestige ~ education + log2(income) + type + type : education + log2(income) : type, data = data)
linmod4 <- lm(prestige ~ education + log2(income) + type + log2(income) : type, data = data)

lmod_leaps <- lm(prestige ~ educatoin * log2(income) + type + women + type : log2(income) + type : women, data = data)
lmod_step <- lm(prestige ~ education + log2(income) + type + women + type : log2(income) + type : women, data = data)



# ----------
# before transformation
linmod5 <- lm(prestige ~ education + income + type + income:type, data = data)

summary(linmod4)
summary(linmod5)




# ------------------------------------------------------------------------------
# Multimodel inference:  AICc and Weight of Evidence
#   -  weight of evidence of the hth model: W(h) = exp(-0.5 * delta(h)) / sum( exp(-0.5 * delta) )
#      Given the data, the set of models, and the uknowable true model, W(h) indicates the probability
#      that model h is the best approximating model
#      It has been suggested that W(h) = 0.90 and 0.95 are reasonable benchamrks
#      Farily confident the best-fitting model is, in fact, the true best approximating model (not true model)
#      if its probability of being so is at least 0.90
# ------------------------------------------------------------------------------


library(AICcmodavg)


mynames <- paste0("M", as.character(1:7))



# second.ord = TRUE/FALSE:  AICc/AIC is computed

myaicc <- aictab(cand.set = list(linmod, linmod2, linmod3, linmod4, linmod5, lmod_leaps, lmod_step),
                modnames = mynames, sort = TRUE, second.ord = TRUE)


as.data.frame(myaicc)



# -->
# "AICcwt":  best M7 is best 0.67 in AICcwt




# ------------------------------------------------------------------------------
# Multimodel inference:  confidence sets
#   - It is not always the case that a single model will have such a large (0.9 or 0.95) probability.
#     In such cases, it is convenient to form a confidence set of the models whose probabilities sum to 0.90 to 0.95
# ------------------------------------------------------------------------------

confset(cand.set = list(linmod, linmod2, linmod3, linmod4, linmod5, lmod_leaps, lmod_step), modnames = mynames, level = 0.75)



# -->
# M7 and M6 constitutes sum of probability up to 0.99



# ------------------------------------------------------------------------------
# Multimodel inference:  Evidence Ratio
# ------------------------------------------------------------------------------

# Evidence ratio for the hth model = W(max) / W(h)


# single evidence ratio
evidence(myaicc)



# -->
# M7 is the best approximating model is about 2.1 to 1 over M6




# ----------
# all evidence ratio

# exclude logLik
myaicc2 <- as.data.frame(myaicc)[,-7]

myaicc2$Eratio <- round(max(myaicc2$AICcWt) / myaicc2$AICcWt, 3)



data.frame(Model = myaicc2$Modnames, round(myaicc2[,-1], 3))





