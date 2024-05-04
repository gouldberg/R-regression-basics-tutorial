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
# Data exploration:  pairs plot
# ------------------------------------------------------------------------------


MyVar <- c("education", "income", "women", "prestige")

psych::pairs.panels(na.exclude(data[,MyVar]), stars = TRUE)


psych::pairs.panels(na.exclude(data[,MyVar]), method = "spearman", stars = TRUE)




# -->
# "women"'s correlation with prestige is almost zero.
# "income" and education is highly correlated with prestige.
# may need to transform "income" data




# ------------------------------------------------------------------------------
# Data exploration:  corrplot
# ------------------------------------------------------------------------------

library(corrplot)


cor_mat <- cor(data %>% dplyr::select(-type), method = "spearman")


corrplot(cor_mat, hclust.method = "ward.D2", addrect = TRUE)




# ------------------------------------------------------------------------------
# Data exploration:  paris plot by scatterplot matrix
# ------------------------------------------------------------------------------


library(car)


formula <- ~ education + income + women + prestige


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)





# ------------------------------------------------------------------------------
# Data exploration:  distribution by group
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

boxplot(education ~ type, data = data)

boxplot(income ~ type, data = data)

boxplot(women ~ type, data = data)

boxplot(prestige ~ type, data = data)



# -->
# wc's income is unexpectedly low ... the range is almost same with bc
# there seems to be interaction effect of income * type for prestige

# education effect may be covered by "type" effect




# ------------------------------------------------------------------------------
# Data exploration:  paris plot by scatterplot matrix by group
# ------------------------------------------------------------------------------

# by group

formula <- ~ education + income + women + prestige | type

scatterplotMatrix(formula, data = data, cex = 1.2,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black", "blue"), pch = 1:3)


# -->
# income is different intercept and slope by type

