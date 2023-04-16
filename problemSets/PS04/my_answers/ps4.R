
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
 lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)
## Load library
library(ggplot2)
library(tidyverse)
library(lubridate)
library(eha)

data("child")
dat <- child
glimpse(dat)
####
?Surv
child_surv <- with(dat, Surv(enter, exit, event))
km <- survfit(child_surv ~ 1, data = dat)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

### effcts
##mage not working bins as ocntinues so bins
dat$m.age <-cut(dat$m.age, breaks=c(15, 20,25, 35, 40,45, 50), right = FALSE)
kn_mage <- survfit(child_surv ~ m.age, data = dat)
autoplot(kn_mage)
##sex
kn_s <- survfit(child_surv ~ sex, data = dat)
autoplot(kn_s)
##sex and mage additive
kn_s_mage_a <- survfit(child_surv ~ sex + m.age, data = dat)
autoplot(kn_s_mage_a)
##sex and mage interactive
cox_s_mage_i <- coxph(child_surv ~ sex * m.age, data = dat)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")
