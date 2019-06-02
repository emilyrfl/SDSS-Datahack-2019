library(tidycensus)
library(tidyverse)
library(ggmap)
library(tmap)
library(acs)
library(viridis)


key <- "e5e70e43116991880bc691ba349e0d4955527403"
census_api_key(key)

this.year <- 2014

vars <- load_variables(year = 2013,
                       dataset = "acs5",
                       cache = TRUE)

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

varriables2 <- c(median.gross.rent = "B25064_001",
              median.household.income = "B19013_001",
              rent.burden = "B25071_001",
              white = "B03002_003", 
              af.am = "B03002_004",
              hispanic = "B03002_012",
              am.ind = "B03002_005",
              asian = "B03002_006",
              nh.pi = "B03002_007",
              multiple = "B03002_009",
              other = "B03002_008")

waDem2017 <- get_acs(geography = "tract", year = 2017,
                 state = "WA", county = "King", geometry = TRUE,
                 variables = variables)


waDem <- rbind(waDem2010 %>% mutate(Year = 2010),
               waDem2011 %>% mutate(Year = 2011), 
               waDem2012 %>% mutate(Year = 2012), 
               waDem2013 %>% mutate(Year = 2013), 
               waDem2014 %>% mutate(Year = 2014), 
               waDem2015 %>% mutate(Year = 2015), 
               waDem2016 %>% mutate(Year = 2016), 
               waDem2017 %>% mutate(Year = 2017))

waDem2 <- waDem %>% select(-moe) %>% spread(variable, estimate)


ggplot(waDem2 %>% 
         mutate(pct.poor = (income0.9999 + income10000.14999)/population) %>% 
         filter(Year == 2010), 
       aes(fill = pct.poor)) +
  geom_sf() + scale_fill_viridis(option = "magma")  +
  labs(title = "2012 < 10,000") +
  theme_light()




  mutate(checkTot = white+af.am+hispanic+am.ind+ # looks good!
           asian+nh.pi+multiple+other) %>%
  mutate(pct.white = white/checkTot,
         pct.af.am = af.am/checkTot,
         pct.hispanic = hispanic/checkTot,
         pct.am.ind = am.ind/checkTot,
         pct.asian = asian/checkTot,
         pct.nh.pi = nh.pi/checkTot,
         pct.multiple = multiple/checkTot, 
         pct.other = other/checkTot, 
         year = this.year)

head(waPct)

sea.counties <- geo.make(state="WA", county="King", tract="*")

ggplot(waDem2013) +
  geom_polygon(data = )


