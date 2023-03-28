# JOP
# LPM sensitivity using Cinelli and Hazlett package
# Last updated April 15, 2022
library(tidyverse)
library(texreg)
library(sensemakr)
library(fixest)
library(here)
### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

# LPM sensitivity analysis based on Table 7 appendix -----
# just do on main model
cluster_variable <- "new_group_id"
orig$year_factor <- factor(orig$year)
lpm_main1 <- lm(fatal~Black + Hispanic + Asian +trauma10 +
                  city_clean, 
                data=orig)
s1 <- sensemakr(model = lpm_main1,
                treatment = "Black",
                benchmark_covariates = c("city_cleanLos Angeles",
                                         "city_cleanSeattle",
                                         "trauma10"),
                kd=3,
                ky =3)


t1 <- ovb_minimal_reporting(s1, format = 'latex')
cat(t1,file=here("JOP_RacialBias/tables","lpm-sensitivity1.tex"))

summary(s1)
stable <- summary(s1)
round(stable$R2yz.dx*100,1)
round(stable$R2dz.x*100,1)

