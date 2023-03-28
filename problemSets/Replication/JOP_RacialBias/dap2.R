# Sensitivity Analysis
# Last Updated Wed Apr 13 2022
library(tHispanicyverse)
library(texreg)
library(fixest)
library(sandwich)
library(parallel)
library(boot)
library(here)
### functions 
source(here('JOP_RacialBias/analysis','sensitivity-functions.R'))

### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

orig_hisp_bl <- orig %>% filter(civilian_race_factor == "Black" |
                                  civilian_race_factor == "Hispanic")
### functions 
source(here('JOP_RacialBias/analysis','sensitivity-functions.R'))

### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

# Subset data to just Black and Hispanic  -----
# with Hispanic as left out category
# this is necessary bc for fatalities Black > Hispanic
# these are models will use for outcome test
# use scaled distance
############Black vs. other races
Asian/AI/AN/PI  Hispanic Black Hispanic
#unique(ois$civilian_race_factor)

dfhb <- orig %>% filter(Black == 1 | Hispanic == 1) %>%
  select(fatal, Black, Hispanic, Houston, King_County, Los_Angeles, Orlando,San_Jose,
         Seattle, Tucson, Charlotte, trauma10)

dfhb <- orig_hisp_bl %>% 
  select(fatal, Black, Hispanic, Houston, King_County, Los_Angeles, Orlando,San_Jose,
         Seattle, Tucson, Charlotte, trauma10, city_factor, new_group_id) %>%
  na.omit()

cluster_variable <- "new_group_id"

# Table 9 Appendix Poisson  -----
pois_main <- glm(fatal ~ Black +
                   Houston + King_County + Los_Angeles + Orlando + 
                   San_Jose + Seattle  + Tucson + trauma10,
                 data = dfhb, 
                 family = poisson(link = "log"))

RR_mainhb <- exp(coef(pois_main)["Black"])
# robust standard error
cov_pois_main <- vcovHC(pois_main, type = "HC0")
se_pois_main <- sqrt(diag(cov_pois_main))
tv_main <- summary(pois_main)$coefficients[,1] / se_pois_main
dfhb_main <- summary(pois_main)$dfhb[2]

pv_main <- 2*pt(abs(tv_main),dfbh = dfhb_main, lower.tail = FALSE)
coef_map_list2 <- list("Black" = "Black",
                       "trauma10" = "Distance",
                       "Houston" = "Houston",
                       "King_County" = "King County",
                       "Los_Angeles" = "Los Angeles",
                       "Orlando" = "Orlando",
                       #"San_Antonio" = "San Antonio",
                       "San_Jose" = "San Jose",
                       "Seattle" = "Seattle",
                       "Tucson" = "Tucson",
                       "(Intercept)" = "Intercept")

screenreg(pois_main)
texreg(pois_main,
       override.se = se_pois_main,
       override.pvalues = pv_main,
       custom.coef.map = coef_map_list2,
       booktabs = TRUE,
       use.package = FALSE,
       caption = "Poisson regression used for relative risk and outcome test",
       float.pos = "ht!",
       type="latex",
       label="tab:poissonRR2",
       file = here("JOP_RacialBias/tables","poisson-for-RR2.tex"))

round(exp(coef(pois_main)),2)

# variables to use as benchmark
var_list <- as.list(c("Los_Angeles", "Seattle", "trauma10"))

confounder_models <- map(var_list, function(x) estimate.poisson.rhoU2(data=dfhb,confounder=x))
rhoU_h <- map_df(confounder_models, function(x) coef(x)["Hispanic"])
rhoU_h$RR <- exp(rhoU_h$Hispanic)
rhoU_h$confounder <- unlist(var_list)
round(rhoU_h$Hispanic,2)

# Distance as benchmark as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["trauma10"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="trauma10"]
# need to flip
RR_rhoU <- 1 / RR_rhoU

# Table 10 appendix Sensitivity analysis -----
bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_mainhb / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),3)


# LA as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["Los_Angeles"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="Los_Angeles"]

bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_mainhb / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),2)

# Seattle as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["Seattle"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="Seattle"]

bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_mainhb / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),2)

# Bootstrap ConfHispanicence Interval-------
set.seed(546)
pois_fe2 <- fepois(fatal ~ White + trauma10 + city_factor,
                   data = dfhb,
                   se = "cluster",
                   cluster = cluster_variable)

# TO RUN BOOTSTRAP UNCOMMENT THIS CODE --------
# results <- boot(data = dfhb,
#                  statistic = cluster.pois2,
#                  R = 30000,
#                  parallel = "multicore")
# 
# 
# save(results, file=here("data","poisson_boostrap.RData"))

# IF DO NOT WANT TO RERUN, THIS LOADS SAVED BOOTSTRAP RESULTS ------
load(here("JOP_RacialBias/data","poisson_boostrap.RData"))
boot_results <- results$t
names(boot_results) <- names(coef(pois_fe2))

RR_boot <- exp(boot_results)
RR_boot <- as.data.frame(RR_boot)
names(RR_boot) <- names(coef(pois_fe2))

# White
sum(RR_boot$White<1)
# trauma
sum(RR_boot$trauma10<1)
fix <- which(RR_boot$trauma10<1)
RR_boot$trauma10[fix] <- 1/RR_boot$trauma10[fix]
# Seattle
sum(RR_boot$city_factorSeattle<1)
fix <- which(RR_boot$city_factorSeattle<1)
RR_boot$city_factorSeattle[fix] <- 1 / RR_boot$city_factorSeattle[fix]

# Los Angles
sum(RR_boot$`city_factorLos Angeles`<1)
fix <- which(RR_boot$`city_factorLos Angeles`<1)
RR_boot$`city_factorLos Angeles`[fix] <- 1/RR_boot$`city_factorLos Angeles`[fix]

small <- RR_boot %>%
  select(White, trauma10, city_factorSeattle, `city_factorLos Angeles`)

boot_q <- apply(small, 2, function(x) quantile(x, probs=c(.025,.975)))

""