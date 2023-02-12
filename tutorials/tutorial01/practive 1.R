library(tidyverse)
library(stargazer)
data <- f375b5ad_03ce_4672_83f4_363b88de9f1c_Data
data <- data %>% 
  select(-(starts_with("Time")),-('Country Code'))
names(data) <- sub(" \\[.*", "",names(data)) 

glimpse(data)
help(ggplot)

data%>% 
ggplot(aes(`GDP per capita (current US$)`,
           as.numeric(`Ease of doing business rank (1=most business-friendly regulations)`)))+
  geom_point()+
    geom_smooth(method="loess")

data%>% 
  ggplot(aes(`GDP per capita (current US$)`,
            `Tax revenue (% of GDP)`))+
  geom_line()+
  geom_point()+
  geom_smooth(method="loess")

typeof(data$`Tax revenue (% of GDP)`)
taxgdp <- as.numeric(data$`Tax revenue (% of GDP)`)
gdpcap <- as.numeric(data$`GDP per capita (current US$)`)
ease <- as.numeric(data$`Ease of doing business rank (1=most business-friendly regulations)`)

data%>% 
  ggplot(aes(ease,gdpcap))+
  #geom_line()+
  geom_point()+
  geom_smooth(method="loess")

data%>% 
  ggplot(aes(gdpcap,taxgdp))+
#  geom_line()+
  geom_point()+
  geom_smooth(method="loess")

summary(lm(gdpcap~ease))
summary(lm(taxgdp ~gdpcap))

data$ease[2] <- data
rbind
  