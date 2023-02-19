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
model_add <- glm(choice ~ 1., data = data, family = 'binomial')
summary(model_additive)
##same as rough work
model_additive1 <- glm(choice1 ~ 1., data = data, family = 'binomial')
data$choice1 <- as.logical(data$choice == 'Supported','Not supported',1,0)
summary(model_additive1)
##Null model
nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = data, 
               family = "binomial")
##Null v not
anova(nullMod, model_add, test = "Chisq")
anova(nullMod, model_add, test = "LRT")
##exponential
exp(confint(model_add))



                                        




