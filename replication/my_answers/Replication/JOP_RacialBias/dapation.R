# Sensitivity Analysis
# Last Updated Wed Apr 13 2022
library(tHispanicyverse)
library(texreg)
library(fixest)
library(sandwich)
library(parallel)
library(boot)
library(here)

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

confounder_models <- map(var_list, function(x)
  
round(1 - (1/RR_adj),2)