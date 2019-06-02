library(tidycensus)
library(tidyverse)
library(ggmap)
library(tmap)
library(acs)
library(viridis)
library(here)

key <- "e5e70e43116991880bc691ba349e0d4955527403"
census_api_key(key)

variables <- c(population = "B02001_001",
               citizen.resident = "B05001_002",
               noncitizen.resident = "B05001_006",
               migrant.domestic = "B07007_021",
               migrant.fromdiffstate = "B07001_065",
               migrant.overseas = "B07001_081",
               migrant.state = "B07007_016",
               new.since2010 = "B05007_002",
               youth = "B05009_001",
               no.income = "B06010_002",
               income = "B06010_001",
               income0.9999 = "B06010_004",
               income10000.14999 = "B06010_005",
               income15000.24999 = "B06010_006",
               income25000.34999 = "B06010_007",
               income35000.49999 = "B06010_008",
               income50000.64999 = "B06010_009", 
               income65000.74999 = "B06010_010",
               income75000.0 = "B06010_011",
               instate.noincome = "B06010_013",
               otherstate.noincome = "B06010_024",
               same.house.1 = "B07007_006")

waDem <- rbind(
  get_acs(geography = "tract", year = 2010,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2010), 
  get_acs(geography = "tract", year = 2011,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2011),
  get_acs(geography = "tract", year = 2012,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2012),
  get_acs(geography = "tract", year = 2013,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2013),
  get_acs(geography = "tract", year = 2014,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2014),
  get_acs(geography = "tract", year = 2015,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2015),
  get_acs(geography = "tract", year = 2016,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2016),
  get_acs(geography = "tract", year = 2017,
          state = "WA", county = "King", geometry = TRUE,
          variables = variables) %>% mutate(year = 2017)
)

waDem_est <- waDem %>% select(-moe) %>% spread(variable, estimate)
waDem_moe <- waDem %>% select(-estimate) %>% spread(variable, moe)

waDem_est <- waDem_est %>% mutate(inc.prop0.9999 = income0.9999/population, 
                                  inc.prop10000.14999 = income10000.14999/population, 
                                  inc.prop15000.24999 = income15000.24999/population, 
                                  inc.prop35000.49999 = income35000.49999/population,
                                  inc.prop50000.64999 = income50000.64999/population, 
                                  inc.prop65000.74999 = income65000.74999/population, 
                                  inc.prop75000.0 = income75000.0/population,
                                  prop.lowinc = inc.prop0.9999 + inc.prop10000.14999,
                                  prop.newsince2010 = new.since2010/population, 
                                  prop.migrantfromdiffstate = migrant.fromdiffstate/population, 
                                  prop.migrantoverseas = migrant.overseas/population,
                                  prop.migrantinstate = migrant.state/population,
                                  prop.migrant = prop.migrantfromdiffstate + prop.migrantinstate + prop.migrantoverseas,
                                  prop.samehouse1 = same.house.1/population)


ggplot(waDem_est %>% 
         filter(year == 2010), 
       aes(fill = prop.samehouse1)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "Proportion of individuals living in the \n same house as last year in 2010") +
  theme_light()           

ggplot(waDem_est %>% 
         filter(year == 2011), 
       aes(fill = prop.samehouse1)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "Proportion of individuals living in the \n same house as last year in 2011") +
  theme_light()   

ggplot(waDem_est %>% 
         filter(year == 2012), 
       aes(fill = prop.samehouse1)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "Proportion of individuals living in the \n same house as last year in 2012") +
  theme_light()     

ggplot(waDem_est %>% 
         filter(year == 2013), 
       aes(fill = prop.samehouse1)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "Proportion of individuals living in the \n same house as last year in 2013") +
  theme_light()     

ggplot(waDem_est %>% 
         filter(year == 2011), 
       aes(fill = prop.samehouse1)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "Proportion of individuals living in the \n same house as last year in 2011") +
  theme_light()                                       
                                  
                                  
                                  
                                  