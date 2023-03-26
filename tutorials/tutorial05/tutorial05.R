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

install.packages("foreign")
install.packages("nnet")
install.packages("reshape2")
install.packages("MASS")
library('MASS')
library('foreign')
library("nnet")
library(reshape2)
# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?
table(workingMoms$attitude)
table(workingmomsvector)
typeof(workingMoms$attitude)
# Specify that they are ordinal variables with the given levels
workmv <- factor(workingMoms$attitude, order = TRUE, 
                                    levels = c("SD", "D", "A","SA"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                              levels = c(0,1),
                              labels = c('non-white','white'))

#workingMoms$year <- factor(workingMoms$year,
#                           levels = c('year1977','year1998'),
#                           labels = c("1977",'1988'))

# Print the result to the console
workingMoms$attitude <- workmv
modb <- glm(attitude ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = workingMoms, 
           family = "binomial")

modl <- glm(attitude ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = workingMoms, 
           family = binomial(link = "logit"))
summary(modb)

#####martyn way
m <- polr(attitude~.,
     data = workingMoms,Hess = TRUE)
summary(m)
ctable <- coef(summary(m,))
p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE * 2
ctable <- cbind(ctable,"p value"= p)

ci <- confint(m)
exp(cbind(OR = coef(m),ci))
# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.
workingMoms$attitude <- relevel(workingMoms$attitude)


# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?