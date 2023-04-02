# this creates some indicator variables and other data sets needed
# Last updated Apr 13, 2022
library(tidyverse)
library(here)
### load data -------
ois <- readRDS(here('JOP_racialbias/data','ois.RDS'))

#create binary fatal variable
ois$fatal <- ifelse(ois$civilian_fatality=='Fatal',1,0)
ois$fatal <- ifelse(ois$civilian_fatality %in% c('Undetermined','Unknown'),NA,ois$fatal)
#order race variable
ois$civilian_race_factor <- factor(ois$civilian_race_new, 
                                   levels = c("White", "Black", "Hispanic","Asian/AI/AN/PI"))

## all cities and include year variable -------
orig <- ois %>% 
  filter(!is.na(fatal)) %>%
  #filter(civilian_race_factor %in% c("White","Black")) %>%
  select(city_clean, civilian_race_factor,fatal, year,closesttraumamiles,new_group_id)

# Make city dummies
orig <- orig %>%
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
orig <- orig %>%
  mutate(White = ifelse(civilian_race_factor == 'White', 1, 0),
         Black = ifelse(civilian_race_factor == 'Black', 1, 0),
         Hispanic = ifelse(civilian_race_factor == 'Hispanic', 1, 0),
         Asian = ifelse(civilian_race_factor == 'Asian/AI/AN/PI', 1, 0))

# re-scale distance variable
orig$trauma10 <- orig$closesttraumamiles / 10

orig$city_factor <- factor(orig$city_clean,
                           levels = c("Charlotte","Houston","King County",
                                      "Los Angeles","Orlando",
                                      "San Jose","Seattle","Tucson"))
orig$city_factor <- relevel(orig$city_factor, ref="Charlotte")
levels(orig$city_factor)
# Make subsets of data -----
orig_bl_wh <- orig %>% filter(civilian_race_factor == "White" |
                                civilian_race_factor == "Black")

orig_hisp_wh <- orig %>% filter(civilian_race_factor == "White" |
                                  civilian_race_factor == "Hispanic")

orig_asian_wh <- orig %>% filter(civilian_race_factor == "White" |
                                   civilian_race_factor == "Asian/AI/AN/PI")

save(orig,orig_bl_wh,orig_hisp_wh,orig_asian_wh,ois,
     file = here("JOP_RacialBias/data","ois_data_for_models.RData"))

