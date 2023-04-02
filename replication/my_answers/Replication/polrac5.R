# Regression tables in Appendix for JOP
# Last updated April 13 2022
library(tidyverse)
library(texreg)
library(fixest)
library(sandwich)
library(lubridate)
library(lfe)
library(here)

### load data -------
load(here('JOP_racialbias/data','ois_data_for_models.RData'))
cluster_variable <- "new_group_id"
orig$year_factor <- factor(orig$year)
# Table 6 adding year fixed effects Logistic Appendix -----
glm_main1 <- feglm(fatal~Black + Hispanic + Asian +trauma10 +
                     city_clean, 
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster=cluster_variable)

glm_main2 <- feglm(fatal~Black + Hispanic + Asian + trauma10 +
                     year_factor, 
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster=cluster_variable)

glm_main3 <- feglm(fatal~Black + Hispanic + Asian + trauma10 +
                     city_clean + year_factor,
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster=cluster_variable)


glm_main4 <- feglm(fatal~Black + Hispanic + Asian + trauma10 +
                     city_clean+year_factor,
                   data=orig,
                   family=binomial(link="logit"),
                   se="cluster",
                   cluster=cluster_variable)

# name mapping list
coefsnames <- list('Black' = 'Black',
                   "Hispanic" = 'Hispanic',
                   'Asian' = 'Asian/AI/AN/PI',
                   'trauma10' = 'Closest Trauma (10s miles)',
                   'city_cleanHouston' = 'Houston',
                   'city_cleanKing County' = 'King County',
                   'city_cleanLos Angeles' = 'Los Angeles',
                   'city_cleanOrlando' = 'Orlando',
                   'city_cleanSan Jose' = 'San Jose',
                   'city_cleanSeattle' = 'Seattle',
                   'city_cleanTucson' = 'Tucson',
                   'year_factor2011' = '2011',
                   'year_factor2012' = '2012',
                   'year_factor2013' = '2013',
                   'year_factor2014' = '2014',
                   'year_factor2015' = '2015',
                   'year_factor2016' = '2016',
                   'year_factor2017' = '2017',
                   '(Intercept)' = 'Intercept')

screenreg(list(glm_main1,glm_main2,glm_main3,glm_main4),
          custom.coef.map = coefsnames,
          omit.coef = ":",
          include.dev = FALSE,
          include.loglik = FALSE,
          include.pseudors = FALSE)
capt <- "Estimated relationship between civilian race and probability of fatality conditional upon being involved in an officer-involved shooting. Cells show logistic coefficients with clustered-robust standard errors. Omitted category is white civilians, Charlotte and the year 2010."
# save table
texreg(list(glm_main1,glm_main2,glm_main3,glm_main4),
       custom.coef.map = coefsnames,
       omit.coef = ":",
       include.dev = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       caption = capt,
       booktabs = TRUE,
       use.package=FALSE,
       float.pos = "ht!",
       file = here("JOP_racialbias/tables","logistic-FE-models.tex"),
       label="tab:logistic_FE")


# Table 7 adding year fixed effects LPM model Appendix -----
lpm_main1 <- feols(fatal~Black + Hispanic + Asian +trauma10 +
                     city_clean, 
                   data=orig,
                   se="cluster",
                   cluster=cluster_variable)

lpm_main2 <- feols(fatal~Black + Hispanic + Asian +trauma10 +
                     year_factor, 
                   data=orig,
                   se="cluster",
                   cluster=cluster_variable)

lpm_main3 <- feols(fatal~Black + Hispanic + Asian + trauma10 +
                     city_clean + year_factor,
                   data=orig,
                   se="cluster",
                   cluster=cluster_variable)


lpm_main4 <- feols(fatal~Black + Hispanic + Asian + trauma10 +
                     city_clean*year_factor,
                   data=orig,
                   se="cluster",
                   cluster=cluster_variable)
capt <- "Estimated relationship between civilian race and probability of fatality conditional upon being involved in an officer-involved shooting. Cells show linear probability model coefficients with clustered-robust standard errors. Omitted category is white civilians, Charlotte and the year 2010."
screenreg(list(lpm_main1,lpm_main2,lpm_main3,lpm_main4),
          omit.coef = "city|year")
# save table
texreg(list(lpm_main1,lpm_main2,lpm_main3,lpm_main4),
       custom.coef.map = coefsnames,
       omit.coef = ":",
       include.dev = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       include.rs = FALSE,
       include.adjrs = FALSE,
       caption = capt,
       booktabs = TRUE,
       use.package=FALSE,
       float.pos = "ht!",
       file = here("JOP_racialbias/tables","lpm-FE-models.tex"),
       label="tab:lpm_FE")


# Table 8 adding year fixed effects poisson appendix ------
pois_main1 <- fepois(fatal~Black + Hispanic + Asian +trauma10 +
                       city_clean, 
                     data=orig,
                     se="cluster",
                     cluster=cluster_variable)

pois_main2 <- fepois(fatal~Black + Hispanic + Asian +trauma10 +
                       year_factor, 
                     data=orig,
                     se="cluster",
                     cluster=cluster_variable)

pois_main3 <- fepois(fatal~Black + Hispanic + Asian + trauma10 +
                       city_clean + year_factor,
                     data=orig,
                     se="cluster",
                     cluster=cluster_variable)


pois_main4 <- fepois(fatal~Black + Hispanic + Asian + trauma10 +
                       city_clean*year_factor,
                     data=orig,
                     se="cluster",
                     cluster=cluster_variable)
capt <- "Estimated relationship between civilian race and probability of fatality conditional upon being involved in an officer-involved shooting. Cells show Poisson coefficients with clustered-robust standard errors. Omitted category is white civilians, Charlotte and the year 2010."
screenreg(list(pois_main1,pois_main2,pois_main3,pois_main4),
          omit.coef=c("city|year"))
# save table
texreg(list(pois_main1,pois_main2,pois_main3,pois_main4),
       custom.coef.map = coefsnames,
       omit.coef = ":",
       include.dev = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       caption = capt,
       booktabs = TRUE,
       use.package=FALSE,
       float.pos = "ht!",
       file = here("JOP_racialbias/tables","poisson-FE-models.tex"),
       label="tab:poisson_FE")

# Table 11 Poisson appendix ------
# estimate subset models

glm_bl_wh <- fepois(fatal ~ White + trauma10  + 
                      i(city_clean, ref="Charlotte"),
                    data = orig_bl_wh,
                    se = "cluster",
                    cluster = cluster_variable)
glm_hisp_wh <- fepois(fatal ~ White + trauma10  + 
                        i(city_clean, ref="Charlotte"),
                      data = orig_hisp_wh,
                      se = "cluster",
                      cluster = cluster_variable)
glm_asian_wh <- fepois(fatal ~ White + trauma10  + 
                         i(city_clean, ref="Charlotte"),
                       data = orig_asian_wh,
                       se = "cluster",
                       cluster = cluster_variable)
# save output to table 
mynames <- list('White' = 'White',
                'Black' = 'Black',
                "Hispanic" = 'Hispanic',
                'Asian' = 'Asian',
                "city_clean::Houston" = "Houston",
                "city_clean::King County" = "King County",
                "city_clean::Los Angeles" = "Los Angeles",
                "city_clean::Orlando" = "Orlando",
                "city_clean::San Jose" = "San Jose",
                "city_clean::Seattle" = "Seattle",
                "city_clean::Tucson" = "Tucson",
                'trauma10' = 'Closest Trauma (10s miles)',
                '(Intercept)' = "Intercept")


screenreg(list(glm_bl_wh, glm_hisp_wh,glm_asian_wh),
          custom.coef.map = mynames,
          custom.model.names = c("Black/White","White/Hispanic",
                                 "White/Asian"),
          include.pseudors = FALSE,
          include.aic = TRUE)

capt <- "Poisson model regression results. Each model is run on a subset of the data to make either a Black/White, Hispanic/White or Asian/White comparison. Cluster-robust standard errors clustered on city. See \\citet{zou2004modified} for a discussion of the robust standard error correction when Poisson models are used to calculate relative risks."
texreg(list(glm_bl_wh, glm_hisp_wh,glm_asian_wh),
       file = here('JOP_racialbias/tables','regresssion_all_race_glm.tex'),
       custom.coef.map = mynames,
       custom.model.names = c("Black/White","White/Hispanic",
                              "White/Asian"),
       label = "table:regression_all_comparisons_glm",
       booktabs = TRUE,
       caption = capt,
       use.package=FALSE,
       float.pos = "ht!",
       include.aic = TRUE,
       include.bic = FALSE,
       include.loglik = FALSE,
       include.adjrs = FALSE,
       include.rs = FALSE,
       include.deviance = FALSE,
       include.nobs = TRUE,
       include.pseudors = FALSE)
#custom.gof.names = c("AIC","N"),
#reorder.gof = c(2,1))

# Relative risk on white coefficient ----
exp(0.37)

# Table 12 Appendix LPM -------
lpm_bl_wh <- feols(fatal ~ White + trauma10  + 
                     i(city_clean, ref="Charlotte"),
                   data = orig_bl_wh,
                   se = "cluster",
                   cluster = cluster_variable)
lpm_hisp_wh <- feols(fatal ~ White + trauma10  + 
                       i(city_clean, ref="Charlotte"),
                     data = orig_hisp_wh,
                     se = "cluster",
                     cluster = cluster_variable)
lpm_asian_wh <- feols(fatal ~ White + trauma10  + 
                        i(city_clean, ref="Charlotte"),
                      data = orig_asian_wh,
                      se = "cluster",
                      cluster = cluster_variable)


rmse1 <- fitstat(lpm_bl_wh, type = "rmse")$rmse
rmse2 <- fitstat(lpm_hisp_wh, type = "rmse")$rmse
rmse3 <- fitstat(lpm_asian_wh, type = "rmse")$rmse
capt <- "Linear probability model regression results. Each model is run on a subset of the data to make either a Black/White, Hispanic/White or Asian/White comparison. Cluster-robust standard errors clustered on city."

screenreg(list(lpm_bl_wh, lpm_hisp_wh,lpm_asian_wh))

texreg(list(lpm_bl_wh, lpm_hisp_wh,lpm_asian_wh),
       file = 'JOP_racialbias/tables/regresssion_all_race_lm.tex',
       custom.coef.map = mynames,
       custom.model.names = c("Black/White","White/Hispanic",
                              "White/Asian"),
       label = "table:regression_all_comparisons_lm",
       booktabs = TRUE,
       caption = capt,
       use.package=FALSE,
       float.pos = "ht!",
       include.nobs = TRUE,
       custom.gof.rows = list("RMSE" = c(rmse1,rmse2,rmse3)),
       include.rs = FALSE,
       include.adjrs = FALSE)

# Table 13 Appendix Logistic -----
logit_bl_wh <- feglm(fatal ~ White + trauma10  + 
                       i(city_clean, ref="Charlotte"),
                     data = orig_bl_wh,
                     se = "cluster",
                     cluster = cluster_variable,
                     family = binomial(link = "logit"))
logit_hisp_wh <- feglm(fatal ~ White + trauma10  + 
                         i(city_clean, ref="Charlotte"),
                       data = orig_hisp_wh,
                       se = "cluster",
                       cluster = cluster_variable,
                       family = binomial(link = "logit"))
logit_asian_wh <- feglm(fatal ~ White + trauma10  + 
                          i(city_clean, ref="Charlotte"),
                        data = orig_asian_wh,
                        se = "cluster",
                        cluster = cluster_variable,
                        family = binomial(link = "logit"))

capt <- "Logistic model regression results. Each model is run on a subset of the data to make either a Black/White, Hispanic/White or Asian/White comparison. Cluster-robust standard errors clustered on city."

screenreg(list(logit_bl_wh, logit_hisp_wh, logit_asian_wh))
texreg(list(logit_bl_wh, logit_hisp_wh, logit_asian_wh),
       file = here('JOP_racialbias/tables','regresssion_all_race_logit.tex'),
       custom.coef.map = mynames,
       custom.model.names = c("Black/White","White/Hispanic",
                              "White/Asian"),
       label = "table:regression_all_comparisons_logit",
       booktabs = TRUE,
       caption = capt,
       use.package=FALSE,
       float.pos = "ht!",
       include.nobs = TRUE,
       #custom.gof.rows = list("RMSE" = c(rmse1,rmse2,rmse3)),
       include.rs = FALSE,
       include.adjrs = FALSE,
       include.pseudors = FALSE)


# Appendix page 13 -------
# RR from logistic model 
logit_bl_wh <- glm(fatal ~ White + trauma10  + 
                     Houston + King_County + Los_Angeles + Orlando
                   + San_Jose + Seattle  + Tucson,
                   data = orig_bl_wh,
                   family = binomial(link = "logit"))
temp_W <- temp_B <- orig_bl_wh
temp_W$White <- 1
temp_B$White <- 0
coef(logit_bl_wh)["White"] #0.70
# just use base glm because fixest object not working with predict
RR_W <- mean(predict.glm(logit_bl_wh, newdata = temp_W, type = 'response'),na.rm=T)
RR_B <- mean(predict.glm(logit_bl_wh, newdata = temp_B, type = 'response'),na.rm=T)
(RR <- RR_W/RR_B)
round(RR,2)
1 - (1/RR) # 1.46

# linear model
coef_W <- coefficients(lpm_bl_wh)["White"]
temp <- orig_bl_wh %>% filter(!is.na(trauma10))
mean_B <- tapply(temp$fatal,temp$White,mean)["0"]

round(coef_W / (coef_W + mean_B),2)
table(temp$White)["0"]*0.31 #148

orig$year_factor <- factor(orig$year)


temp <- orig %>% filter(!is.na(trauma10))
dim(temp)
length(unique(temp$new_group_id))
length(unique(orig$new_group_id)) # unique incidents
table(orig$fatal)
608/(666+608)
table(orig$city_clean)

# Table 5 Appendix --------
load(here("JOP_racialbias/data","sqf.RData"))
dc <- read_csv(here('JOP_racialbias/data','DC_stop_data.csv'))

#format NYC SQF race variable
sqf$clean.race <- ifelse(sqf$race=='B', "Black",
                         ifelse(sqf$race=='P', 'Black-Hispanic',
                                ifelse(sqf$race=='Q', 'White-Hispanic',
                                       ifelse(sqf$race=='A', 'Asian',
                                              ifelse(sqf$race=='I', 'Native American',
                                                     ifelse(sqf$race=='X', 'Unknown',
                                                            ifelse(sqf$race=='W', 'White',
                                                                   ifelse(sqf$race=='Z', 'Other', NA))))))))

## NYC model with month, year, and precinct FEs
sqf$clean.race <- factor(sqf$clean.race,
                         levels = c('White',
                                    'Black',
                                    'Black-Hispanic',
                                    'White-Hispanic',
                                    'Asian',
                                    'Native American',
                                    'other',
                                    'Unknown'))
summary(felm(any.force~as.factor(clean.race)|month(datestop)+year(datestop)+pct, data=sqf))

## DC model ith date and district FEs
dc$race_ethnicity <- factor(dc$race_ethnicity,
                            levels = c('White',
                                       'Black',
                                       'Hispanic',
                                       'Asian',
                                       'Multiple',
                                       'Other',
                                       'Unknown'))
summary(felm(person_search_or_protective_pat_down~as.factor(race_ethnicity)|stop_date+stop_district, data=dc))

