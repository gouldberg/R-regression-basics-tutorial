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
# models
# ------------------------------------------------------------------------------

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
# M-estimation by Huber method
# ------------------------------------------------------------------------------

library(MASS)


rlmod <- rlm(prestige ~ (education + income + type + women)^2, data = data)


summary(rlmod)

summary(linmod5)



# -->
# R2 statistics is not given because it does not make sense in the context of a robust regression
# p-values are not given, too.
# but t-value for income, type : income is large

# The numerical values of the coefficients have changed somewhat and the standard errors are generally smaller.

# Although the robust fit gives numerically different output,
# the overall impression of what predictors are significant in explaining the response is unchanged.
# Thus the robust regression has provided some measure of confirmation.



# ------------------------------------------------------------------------------
# weights assigned by the model 
# ------------------------------------------------------------------------------

wts <- rlmod$w


sort(wts)



# -->
# We can ses that a few records are substantially discounted in the calculation of the robust fit.




# ------------------------------------------------------------------------------
# robust regression final model
# ------------------------------------------------------------------------------

rlmod_fin <- rlm(prestige ~ education + log2(income) + type : log2(income) + type, data = data)

rlmod_fin2 <- rlm(prestige ~ education + income + type : income + type, data = data)


summary(rlmod_fin)


car::compareCoefs(linmod4, rlmod_fin)

car::compareCoefs(linmod5, rlmod_fin2)

