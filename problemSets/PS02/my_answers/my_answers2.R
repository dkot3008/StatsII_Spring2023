#####################
# load libraries
# set wd
# clear global .envir
#####################

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
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport
typeof(data$choice) 
##changing from integer to logical
data$choice<- as.logical(as.numeric(as.factor(data$choice))-1) 

#additive model
model_add <- glm(choice ~ ., data = data, family = 'binomial')
summary(model_add)

##Null model
nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = data, 
               family = "binomial")
##Null v not
anova(nullMod, model_add, test = "Chisq")
anova(nullMod, model_add, test = "LRT")
##exponential
exp(confint(model_add))
##some extra
confMod <- data.frame(cbind(lower = exp(confint(model_add)[,1]), 
                            coefs = exp(coef(model_add)), 
                            upper = exp(confint(model_add)[,2])))

# Then use this to make a plot
library(ggplot2)
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")


##Question 2(a)
# Subset the data for policy with 160 countries
policy_160 <- filter(climateSupport, countries == "160 of 192")

# Fit logistic regression model
model160 <- glm(choice ~ sanctions, data = policy_160, family = binomial)

# Estimate the change in odds when sanctions increase from 5% to 15%
odds_ratio <- exp(coef(model160)[2] * (15 - 5) / 100)

# Print the result
cat("Increasing sanctions from 5% to 15% increases the odds of supporting the policy by a factor of", round(odds_ratio, 2), "\n")


#####Q2B
# Subset the data for policy with 80 countries
policy_80 <- filter(climateSupport, countries == "80 of 192")

# Fit logistic regression model
model80 <- glm(choice ~ sanctions, data = policy_80, family = binomial)

summary(model80)

####Q2(C)
log_reg_int_model <- glm(choice ~ countries * sanctions  , 
                         data = climateSupport,
                         family = "binomial")
anova(model_add, log_reg_int_model, test = "Chi")
#The output of the ANODEV test shows that the p-value is 0.3912, which is greater than the usual significance level of 0.05. This suggests that including the interaction term does not significantly improve the model fit, and the additive model is adequate. Therefore, we can conclude that the relationship between the response variable and the explanatory variables is well-captured by the additive model that includes only the main effects.











