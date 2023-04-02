# Regression table in main analysis
# Last updated Apr 13 2022
library(tidyverse)
library(texreg)
library(fixest)
library(here)
### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

origo <- ois %>% 
  filter(!is.na(fatal)) %>%
  #filter(civilian_race_factor %in% c("White","Black")) %>%
  select(city_clean, civilian_race_factor,fatal, year,closesttraumamiles,new_group_id,officer_race_new)
# Make city dummies
origo <- origo %>%
  mutate(Houston = ifelse(city_clean=='Houston',1,0),
         King_County = ifelse(city_clean == 'King County',1,0),
         Los_Angeles = ifelse(city_clean == 'Los Angeles',1,0),
         Orlando = ifelse(city_clean == 'Orlando',1,0),
         San_Antonio = ifelse(city_clean == 'San Antonio',1,0),
         San_Jose = ifelse(city_clean == 'San Jose',1,0),
         Seattle = ifelse(city_clean == 'Seattle',1,0),
         Tucson = ifelse(city_clean == 'Tucson',1,0),
         Charlotte = ifelse(city_clean == 'Charlotte',1,0))
# Make Race dummies
origo <- origo %>%
  mutate(White = ifelse(civilian_race_factor == 'White', 1, 0),
         Black = ifelse(civilian_race_factor == 'Black', 1, 0),
         Hispanic = ifelse(civilian_race_factor == 'Hispanic', 1, 0),
         Asian = ifelse(civilian_race_factor == 'Asian/AI/AN/PI', 1, 0))

# re-scale distance variable
origo$trauma10 <- origo$closesttraumamiles / 10

origo$city_factor <- factor(origo$city_clean,
                           levels = c("Charlotte","Houston","King County",
                                      "Los Angeles","Orlando",
                                      "San Jose","Seattle","Tucson"))
origo$city_factor <- relevel(origo$city_factor, ref="Charlotte")
levels(origo$city_factor)
# Make subsets of data -----
orig_bl_wh <- origo %>% filter(civilian_race_factor == "White" |
                                civilian_race_factor == "Black")

orig_hisp_wh <- origo %>% filter(civilian_race_factor == "White" |
                                  civilian_race_factor == "Hispanic")

orig_asian_wh <- origo %>% filter(civilian_race_factor == "White" |
                                   civilian_race_factor == "Asian/AI/AN/PI")

save(origo,orig_bl_wh,orig_hisp_wh,orig_asian_wh,ois,
     file = here("JOP_racialBias/data","ois_data_for_models.RData"))

#order race variable
ois$officer_race_new <- factor(ois$officer_race_new, 
                                   levels = c("White", "Black", "Hispanic","Asian/AI/AN/PI"))

#unique(ois$officer_race_new)
origo <- origo %>%
  mutate(White = ifelse(officer_race_new == 'Whiteo', 1, 0),
         Black = ifelse(officer_race_new == 'Blacko', 1, 0),
         Hispanic = ifelse(officer_race_new == 'Hispanico', 1, 0),
         Asian = ifelse(officer_race_new == 'Asian/AI/AN/PIo', 1, 0))



# Table 2 main analysis Logistic -------
glmo1 <- feglm(fatal~Black + Hispanic + Asian ,
                   data=origo,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glmo2 <- feglm(fatal~Black + Hispanic + Asian +city_clean ,
                   data=origo,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glmo3 <- feglm(fatal~Black + Hispanic + Asian+ trauma10  ,
                   data=origo,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")
glmo4 <- feglm(fatal~Black + Hispanic + Asian +trauma10 +
                     city_clean, 
                   data=origo,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster="new_group_id")

glmo5 <- feglm(fatal~Black + Hispanic + Asian +trauma10 +
                 city_clean+ officer_race_new, 
               data=origo,
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
                  "(Intercept)" = "Intercept",
                  "officer_race_new"="Officer Race")



screenreg(list(glmo1,glmo2,glmo3,glmo4,glmo5),
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
texreg(list(glmo1,glmo2,glmo3,glmo4,glmo5),
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
       file = here("JOP_RacialBias/tables","main-glmo-regression.html"))

# predicted probabilities of main specification model 1-----
round(exp(coef(glmo1)) / (1 + exp(coef(glmo1))),2)

# how many unique incidents ---
length(unique(origo$new_group_id)) #748
# how many unique incidents models with distance---
origo %>% filter(!is.na(trauma10)) %>% 
  select(new_group_id) %>%
  unique() %>%
  tally()
# 729

