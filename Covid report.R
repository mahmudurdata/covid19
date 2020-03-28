# https://github.com/RamiKrispin/coronavirus
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus) 
library(tidyverse)

data("coronavirus") 

#write.csv(coronavirus, "coronavirus_28Mar.csv", row.names = F)

alldat <- coronavirus

aus_dat <- alldat %>%
  mutate(type = as.factor(type)) %>% 
  select (country = Country.Region,
          date, 
          cases, 
          type) %>% 
  filter (country %in% c("Australia",
                         "US", 
                         "Italy", 
                         "China",
                         "Korea, South")) %>% 
  filter(type == "confirmed") %>% 
  group_by(date, country) %>%
  select(-type) %>% 
  summarise(Daily_Cases=sum(cases)) %>% 
  spread(country, Daily_Cases) 
head(aus_dat)

all_group_dat <- aus_dat %>% 
  group_by(date) %>% 
  ungroup() %>% 
  mutate(Australia = cumsum(Australia),
         US = cumsum(US),
         Italy = cumsum(Italy),
         China = cumsum(China),
         SouthKorea = cumsum(`Korea, South`))%>% 
  select(-`Korea, South`) %>% 
  gather (country, cases, -date)
head(all_group_dat)

#plot  
ggplot(all_group_dat, aes(date, log(cases))) + 
  geom_line( aes(color = country)) +
  theme_bw()

  
  us_dat <- alldat %>% select (state = Province.State,
                                country = Country.Region,
                                date, 
                                cases, 
                                type) %>% 
    filter (country == "US") %>% 
    filter(type == "confirmed") %>% 
    group_by(date)%>%
    summarise(Daily_Cases=sum(cases))%>%
    ungroup()%>%
    mutate(Agg_Cases=cumsum(Daily_Cases))
  