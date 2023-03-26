# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
library(tidyverse)
library(nnet)
library(MASS)


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
########question 1
glimpse(data)
data <- gdpChange
###make character first 
ndaf <-as.character(data)
ndaf <- factor(ifelse(data$GDPWdiff < 0, "negative",
                      ifelse(data$GDPWdiff == 0, "neutral", "positive")),
               levels = c("negative", "neutral", "positive"))
##replacing old measure
data$GDPWdiff <- ndaf
#######################ordered
model1 <- multinom(GDPWdiff ~ REG + OIL, data)
summary(model1)
#intereptation
exp(coef(model1))
# get p values
z <- summary(model1)$coefficients/summary(model1)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

#####impact ?
pp <- data.frame(fitted(model1))
head(data.frame(attitude = data$GDPWdiff,
                N = pp$negative,
                Neut = pp$neutral,
                P = pp$positive))
        
#question 1b
data$GDPWdiff <- factor(data$GDPWdiff, ordered = TRUE,
                        levels = c("negative", "no change", "positive"))

model_ordered <- polr(GDPWdiff ~ REG + OIL, data = data, Hess = TRUE)

summary(model_ordered)

# Calculate a p value
ctable <- coef(summary(model_ordered))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(model_ordered))

# convert to odds ratio
exp(cbind(OR = coef(model_ordered), ci))

###question 2
dat <- MexicoMuniData
glimpse(dat)

model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = dat, family = poisson)
summary(model)
#interpreting outputs
cfs <- coef(model)

# calculate pseudo R squared
1 - (model$deviance/model$null.deviance)

# calculate RMSE
sqrt(mean((model$model$PAN.visits.06 - model$fitted.values)^2))

####2b 
model$coefficients

#####2c

newdata <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)

mean_visits <- exp(predict(model, newdata, type = "response"))
mean_visits




