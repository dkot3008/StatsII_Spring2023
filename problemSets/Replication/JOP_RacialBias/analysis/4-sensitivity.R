# Sensitivity Analysis
# Last Updated Wed Apr 13 2022
library(tidyverse)
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

# Subset data to just Black and white  -----
# with Black as left out category
# this is necessary bc for fatalities White > Black
# these are models will use for outcome test
# use scaled distance
df <- orig %>% filter(White == 1 | Black == 1) %>%
  select(fatal, White, Black, Houston, King_County, Los_Angeles, Orlando,San_Jose,
         Seattle, Tucson, Charlotte, trauma10)

df <- orig_bl_wh %>% 
  select(fatal, White, Black, Houston, King_County, Los_Angeles, Orlando,San_Jose,
         Seattle, Tucson, Charlotte, trauma10, city_factor, new_group_id) %>%
  na.omit()

cluster_variable <- "new_group_id"

# Table 9 Appendix Poisson  -----
pois_main <- glm(fatal ~ White +
                   Houston + King_County + Los_Angeles + Orlando + 
                   San_Jose + Seattle  + Tucson + trauma10,
                 data = df, 
                 family = poisson(link = "log"))

RR_main <- exp(coef(pois_main)["White"])
# robust standard error
cov_pois_main <- vcovHC(pois_main, type = "HC0")
se_pois_main <- sqrt(diag(cov_pois_main))
tv_main <- summary(pois_main)$coefficients[,1] / se_pois_main
df_main <- summary(pois_main)$df[2]

pv_main <- 2*pt(abs(tv_main),df = df_main, lower.tail = FALSE)
coef_map_list2 <- list("White" = "White",
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

confounder_models <- map(var_list, function(x) estimate.poisson.rhoU2(data=df,confounder=x))
rhoU_white <- map_df(confounder_models, function(x) coef(x)["White"])
rhoU_white$RR <- exp(rhoU_white$White)
rhoU_white$confounder <- unlist(var_list)
round(rhoU_white$White,2)

# Distance as benchmark as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["trauma10"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="trauma10"]
# need to flip
RR_rhoU <- 1 / RR_rhoU

# Table 10 appendix Sensitivity analysis -----
bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_main / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),3)


# LA as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["Los_Angeles"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="Los_Angeles"]

bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_main / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),2)

# Seattle as Benchmark for both RR
RR_UY <- exp(coef(pois_main)["Seattle"])
RR_rhoU <- rhoU_white$RR[rhoU_white$confounder=="Seattle"]

bf <- bias.factor(RR_UY = RR_UY, RR_rhoU = RR_rhoU)
(RR_adj <- RR_main / bf)
1 - (1/RR_adj)
round(1 - (1/RR_adj),2)

# Bootstrap Confidence Interval-------
set.seed(546)
pois_fe2 <- fepois(fatal ~ White + trauma10 + city_factor,
                   data = df,
                   se = "cluster",
                   cluster = cluster_variable)

# TO RUN BOOTSTRAP UNCOMMENT THIS CODE --------
# results <- boot(data = df,
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

