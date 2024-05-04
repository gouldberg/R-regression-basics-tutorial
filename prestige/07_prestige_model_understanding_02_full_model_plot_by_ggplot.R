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
# Full-model plots by ggplots
# ------------------------------------------------------------------------------


mod_obj <- linmod4




fitp <- cbind(mod_obj$model, pred = predict(mod_obj))

head(fitp)

colnames(fitp) <- c("prestige", "education", "log2income", "type", "pred")




# ----------
library(ggplot2)

graphics.off()

gg1 <- ggplot(fitp, aes(x = education, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1

gg1 + facet_grid(~ type)



# -->
# the range of prestige of wc is included in range of bc




# ----------
gg2 <- ggplot(fitp, aes(x = log2income, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg2

gg2 + facet_grid(~ type)





# ----------

mod_obj <- lmod_step

fitp <- cbind(mod_obj$model, pred = predict(mod_obj))

head(fitp)

colnames(fitp) <- c("prestige", "education", "log2income", "type", "women", "pred")


graphics.off()

gg1 <- ggplot(fitp, aes(x = education, y = pred)) + theme_bw() + geom_point(size = 2.5) + geom_point(colour = "black", size = 1.5) + geom_smooth()

gg1

gg1 + facet_grid(~ type)

