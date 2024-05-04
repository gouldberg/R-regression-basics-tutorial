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
# Stepwise selection
# ------------------------------------------------------------------------------

# check model for all 2-way interactions
lmod_int2 <- lm(prestige ~ (education + log2(income) + women + type)^2, data = data)


summary(lmod_int2)




# ---------
lmod_step <- step(lmod_int2)


summary(lmod_step)





# ----------
lmod_step <- lm(prestige ~ education + log2(income) + women + type + log2(income) : type + women : type, data = data)


summary(lmod_step)

