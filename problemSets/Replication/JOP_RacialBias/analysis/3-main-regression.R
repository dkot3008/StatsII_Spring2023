# Regression table in main analysis
# Last updated Apr 13 2022
library(tidyverse)
library(texreg)
library(fixest)
library(here)
### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

# Table 2 main analysis Logistic -------
glm_main1 <- feglm(fatal~Black + Hispanic + Asian,
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glm_main2 <- feglm(fatal~Black + Hispanic + Asian +city_clean,
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glm_main3 <- feglm(fatal~Black + Hispanic + Asian +trauma10,
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glm_main4 <- feglm(fatal~Black + Hispanic + Asian +trauma10 +
                     city_clean, 
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")


name_map <- list( "Black" = "Black",
                  "Hispanic" = "Hispanic",
                  "Asian" = "Asian/AI/AN/PI",
                  "trauma10" = "Distance",
                  "city_cleanHouston" = "Houston",
                  "city_cleanKing County" = "King County",
                  "city_cleanLos Angeles" = "Los Angeles",
                  "city_cleanOrlando" = "Orlando",
                  "city_cleanSan Jose" = "San Jose",
                  "city_cleanSeattle" = "Seattle",
                  "city_cleanTucson" = "Tucson",
                  "(Intercept)" = "Intercept")



screenreg(list(glm_main1,glm_main2,glm_main3,glm_main4),
          custom.coef.map = name_map,
          digits=2,
          caption = "Estimated relationship between civilian race and probability of fatality conditional upon being involved in an officer-involved shooting. Cells show logit coefficients with cluster-robust standard errors. Omitted category is White civilians and Charlottte. Distance is in miles.",
          booktabs = T,
          label = "tab:mainLogitTable",
          float.pos = "ht!",
          include.adjrs = FALSE,
          include.aic = FALSE,
          include.rs = FALSE,
          include.bic = FALSE,
          include.loglik = FALSE,
          include.deviance = FALSE,
          include.pseudors = FALSE,
          use.packages = F)

# table 2 main paper ------
texreg(list(glm_main1,glm_main2,glm_main3,glm_main4),
       custom.coef.map = name_map,
       digits=2,
       caption = "Estimated relationship between civilian race and probability of fatality conditional upon being involved in an officer-involved shooting. Cells show logit coefficients with cluster-robust standard errors. Omitted category is White civilians and Charlotte. Distance is in tens of miles.",
       booktabs = T,
       label = "tab:mainLogitTable",
       float.pos = "ht!",
       include.adjrs = FALSE,
       include.aic = FALSE,
       include.rs = FALSE,
       include.bic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       include.pseudors = FALSE,
       use.packages = F,
       file = here("JOP_RacialBias/tables","main-glm-regression.tex"))

# predicted probabilities of main specification model 1-----
round(exp(coef(glm_main1)) / (1 + exp(coef(glm_main1))),2)

# how many unique incidents ---
length(unique(orig$new_group_id)) #748
# how many unique incidents models with distance---
orig %>% filter(!is.na(trauma10)) %>% 
  select(new_group_id) %>%
  unique() %>%
  tally()
# 729

