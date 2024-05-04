]setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ----------
# prepare levels

table(data$type, useNA = "always")

data$type <- factor(data$type, levels = c("bc", "wc", "prof"))

levels(data$type)





# ------------------------------------------------------------------------------
# Exhaustively searches all possible combinations of the predictors by regsubsets
# ------------------------------------------------------------------------------


# check model for all 2-way interactions
lmod_int2 <- lm(prestige ~ (education + log2(income) + women + type)^2, data = data)


summary(lmod_int2)




# ----------
library(leaps)



# for each size of model p, it finds the variables that produce the minimum RSS
b <- regsubsets(prestige ~ (education + log2(income) + women + type)^2, data = data, method = "exhaustive", nvmax = 12)

b



# ----------
( rs <- summary(b) )


rs$which



# ----------
( AIC <- nrow(data) * log(rs$rss / nrow(data)) + (1:12) * 2 )


plot(AIC ~ I(1:12), ylab = "AIC", xlab = "Number of Predictors")




# ----------
rs$which[7,][which(rs$which[7,] == "TRUE")]




# ----------
# final model
lmod_leaps <- lm(prestige ~ log2(income) * education + type + women + type : log2(income) + type : women, data = data)

summary(lmod_leaps)



# ----------
# diagnostics

par(mfrow = c(2,2))

plot(lmod_leaps)



car::residualPlots(lmod_leaps)




