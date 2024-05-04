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
# Least Trimmed Squares
#   - minimize the sum of squares of the q smallest residuals
#   - This method has a high breakdown point because it can tolerate a large number of outliers depending on how q is chosen.
#   - default choice of q = [n/2] + [(p+1)/2] where [x] indicates the largest integer less than or equal to x.
# ------------------------------------------------------------------------------

tmp <- data %>% dplyr::filter(! is.na(type))



set.seed(123)


# add women and type : women
ltsmod <- ltsreg(prestige ~ education + income + type : income + type + women + type : women, data = tmp)


ltsmod


coef(ltsmod)




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  exhaustive search
# ------------------------------------------------------------------------------

# this produces errors

ltsmod_e <- ltsreg(prestige ~ education + income + type : income + type + women + type : women, data = tmp, nsamp = "exact")


coef(ltsmod_e)




# ------------------------------------------------------------------------------
# Least Trimmed Squares:  bootstrapping
# ------------------------------------------------------------------------------


# nsamp = "best":  in order to avoid long computing time, we use "best", 
# but bootstrap estimates of variability will be somewhat on the high side

bcoef <- matrix(0, 1000, 10)


for(i in 1:1000){
  
  newy <- predict(ltsmod) + residuals(ltsmod)[sample(nrow(tmp), rep = T)]
  
  brg <- ltsreg(newy ~ education + income + type : income + type + women + type : women, data = tmp, nsamp = "best")
  
  bcoef[i,] <- brg$coef
}


bcoef <- data.frame(bcoef)

colnames(bcoef) <- names(coef(ltsmod))


( tmp <- apply(bcoef, 2, function(x) quantile(x, c(0.025, 0.975))) )



# ----------

library(ggplot2)


ggplot(bcoef, aes(x = income)) + geom_density() + geom_vline(xintercept = tmp[,"income"], lty = 2) + theme_bw()


ggplot(bcoef, aes(x = women)) + geom_density() + geom_vline(xintercept = tmp[,"women"], lty = 2) + theme_bw()




# -->
# women is not significant ..


