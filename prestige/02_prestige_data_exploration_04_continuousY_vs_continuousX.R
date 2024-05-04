rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\regression_basics\\prestige")



# ------------------------------------------------------------------------------
# data:  Prestige
# ------------------------------------------------------------------------------

data <- read.csv("Prestige.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# data exploration:  Y vs. continuous X  by ggplot
# ------------------------------------------------------------------------------

# scale_y_log10(): plot the response and all other features on a log scale
# also plotting a confidence band around the smoothed curve, and adds a linear regression line

library(ggplot2)

gg <- ggplot(data, aes(education, prestige)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
#  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "education", x = "prestige")


gg



# by group
gg + facet_grid(~ type)




# ----------
gg <- ggplot(data, aes(income, prestige)) + 
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE) +
  #  scale_y_log10(breaks = c(1, 2, 5, 10, 20)) +
  labs(y = "income", x = "prestige")


gg



# by group
gg + facet_grid(~ type)

