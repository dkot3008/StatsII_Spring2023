# JOP R and R
# Descriptives
# Last Updated Wed Apr 13, 2022
library(tidyverse)
library(texreg)
library(sandwich)
library(patchwork)
library(lubridate)
library(here)

### load data -------
load(here('JOP_RacialBias/data','ois_data_for_models.RData'))

# Table 1 summary by race and fatality
table(orig$fatal,orig$civilian_race_factor,exclude=NULL)
round(prop.table(table(orig$fatal,orig$civilian_race_factor),margin = 2),2)

chisq <- chisq.test(orig$fatal,orig$civilian_race_factor)
round(chisq$statistic,3)
chisq$p.value

# Figure 2 OIS per month by cities
facetSettings <-
  theme(panel.grid.major = element_line("lightgray",0.5),
        panel.grid.minor = element_line("lightgray",0.25))


ois %>% filter(!is.na(civilian_race_factor)) %>%
  filter(city_clean != "San Antonio") %>%
  filter(Date >"2009-12-01") %>%
  group_by(month = floor_date(Date, 'month'), city_clean) %>%
  count() %>%
  ggplot(aes(x = month, y = log(n+1))) + #
  stat_smooth() +
  geom_point() +
  facet_wrap(~ city_clean,as.table = FALSE) +
  labs(x='Month-Year', y='Number of OIS', 
       title='Officer-Involved Shootings Per Month') +
  facetSettings +
  scale_y_continuous(breaks=c(log(1),log(11),log(21),
                              log(101), log(201),
                              log(401)),
                     labels = c("0", "10","20", "100",
                                "200","400")) +
  theme_bw() 
ggsave(here('figs','ObsPerMonth-fig1.tiff'),
       width = 9,height = 6,dpi=300)

