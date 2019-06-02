library(tidycensus)
library(rgdal)
library(tidyverse)
library(here)
library(viridis)
library(gganimate)
library(spselect)
library(MASS)
library(glmnet)
library(sf)

`%nin%` <- Negate(`%in%`)

key <- "e5e70e43116991880bc691ba349e0d4955527403"
census_api_key(key)
options(tigris_use_cache = TRUE)

variables <- c(population = "B02001_001",
               citizen.resident = "B05001_002",
               noncitizen.resident = "B05001_006",
               migrant.domestic = "B07007_021",
               migrant.fromdiffstate = "B07001_065",
               migrant.overseas = "B07001_081",
               migrant.state = "B07007_016",
               new.since2010 = "B05007_002",
               youth = "B05009_001",
               age18to24 = "B06001_004",
               age25to34 = "B06001_005", 
               age35to44 = "B06001_006", 
               age45to54 = "B06001_007", 
               age55to59 = "B06001_008",
               age60to61 = "B06001_009",
               age62to64 = "B06001_010",
               age65to74 = "B06001_011",
               age75more = "B06001_012",
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

waDem <- get_acs(geography = "tract", year = 2010,
                 state = "WA", county = "King", geometry = TRUE,
                 variables = variables) %>% mutate(Year = 2010)

waDem_extra <- get_acs(geography = "tract", year = 2010, 
                       state = "WA", county = "King", geometry = TRUE, 
                       variables = "DP03_0001PE") %>% mutate(Year = 2010)




for(i in 1:7) {
  waDemTemp <- get_acs(geography = "tract", year = (2010 + i),
                       state = "WA", county = "King", geometry = TRUE,
                       variables = variables)
  waDemTemp <- waDemTemp %>% mutate(Year = (2010 + i))
  waDem <- rbind(waDem,  waDemTemp)
  
  waDem_extra <- rbind(waDem_extra, get_acs(geography = "tract", year = (2010 + i), 
                                            state = "WA", county = "King", geometry = TRUE, 
                                            variables = "DP03_0001PE") %>% mutate(Year = (2010 + i)))
}


waDem_extra$variable <- "pct.employed"
waDem_extra2 <- waDem_extra %>% dplyr::select(-moe) %>% spread(variable, estimate) 
waDem_extra2 <- waDem_extra2 %>% filter(NAME != "Census Tract 9901, King County, Washington")


waDem_est <- waDem %>% dplyr::select(-moe) %>% spread(variable, estimate)
waDem_moe <- waDem %>% dplyr::select(-estimate) %>% spread(variable, moe)

waDem_est <- waDem_est %>% mutate(
  pct.citizen = citizen.resident/population, 
  pct.noncitizen =  noncitizen.resident/population,
  pct.migrant.domestic = migrant.domestic/population, 
  pct.migtant.overseas = migrant.overseas/population,
  pct.migrant.instate = migrant.state/population,
  pct.migrant = pct.migrant.instate + pct.migrant.domestic + pct.migtant.overseas,
  pct.same.house.1 = 1 - same.house.1/population,
  pct.youth = youth/population,
  pct.noincome = no.income/population,
  pct.0.9999 = income0.9999/population,
  pct.10000.14999 = income10000.14999/population,
  pct.15000.24999 = income15000.24999/population,
  pct.25000.34999 = income25000.34999/population,
  pct.35000.49999 = income35000.49999/population,
  pct.50000.64999 = income50000.64999/population,
  pct.65000.74999 = income65000.74999/population,
  pct.75000.0 = income75000.0/population, 
  pct.abovepov = 1 - (pct.0.9999 + pct.10000.14999),
  pct.low = pct.abovepov + pct.15000.24999,
  pct.lowandmid = pct.abovepov + pct.25000.34999 + pct.35000.49999, 
  pct.age18to24 = age18to24/population, 
  pct.age25to34 = age25to34/population,
  pct.age35to44 = age35to44/population,
  pct.age45to54 = age45to54/population,
  pct.ageabove55 = (age55to59 + age60to61 + age62to64 + age65to74 + age75more)/population) %>%
  filter(NAME != "Census Tract 9901, King County, Washington")

waPct_est <- as.data.frame(waDem_est)

waPct_est <- waPct_est %>% filter(NAME != "Census Tract 9901, King County, Washington")

## Low and Middle Income
{
ggplot(waDem_est %>% 
         filter(Year == 2010), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Percent Low and Middle Income 2010") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2011, NAME != "Census Tract 9901, King County, Washington"), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) + 
  labs(title = "Percent Low and Middle Income 2011") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2012, NAME != "Census Tract 9901, King County, Washington"), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) + 
  labs(title = "Percent Low and Middle Income 2012") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2013), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Percent Low and Middle Income 2013") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2014), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Percent Low and Middle Income 2014") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2015), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Perent Low and Middle Income 2015") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2016), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Percent Low and Middle Income 2016") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1, 0.9))

ggplot(waDem_est %>% 
         filter(Year == 2017), 
       aes(fill = pct.lowandmid)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "Perent Low and Middle Income 2017") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0.1,0.9))
}

{
  ggplot(waDem_est %>% 
           filter(Year == 2011), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2011") + theme_minimal() + theme(axis.line=element_blank(),
                                                   axis.text.x=element_blank(),
                                                   axis.text.y=element_blank(),
                                                   axis.ticks=element_blank(),
                                                   axis.title.x=element_blank(),
                                                   axis.title.y=element_blank(),
                                                   panel.grid.minor=element_blank(),
                                                   panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  
  ggplot(waDem_est %>% 
           filter(Year == 2012), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2012") + theme_minimal() + theme(axis.line=element_blank(),
                                                   axis.text.x=element_blank(),
                                                   axis.text.y=element_blank(),
                                                   axis.ticks=element_blank(),
                                                   axis.title.x=element_blank(),
                                                   axis.title.y=element_blank(),
                                                   panel.grid.minor=element_blank(),
                                                   panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  ggplot(waDem_est %>% 
           filter(Year == 2013), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2013") + theme_minimal() + theme(axis.line=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),
                                                axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank(),
                                                panel.grid.minor=element_blank(),
                                                panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  
  ggplot(waDem_est %>% 
           filter(Year == 2014), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2014") + theme_minimal() + theme(axis.line=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),
                                                axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank(),
                                                panel.grid.minor=element_blank(),
                                                panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  
  ggplot(waDem_est %>% 
           filter(Year == 2015), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2015") + theme_minimal() + theme(axis.line=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),
                                                axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank(),
                                                panel.grid.minor=element_blank(),
                                                panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  
  ggplot(waDem_est %>% 
           filter(Year == 2016), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2016") + theme_minimal() + theme(axis.line=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),
                                                axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank(),
                                                panel.grid.minor=element_blank(),
                                                panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
  
  ggplot(waDem_est %>% 
           filter(Year == 2017), 
         aes(fill = pct.same.house.1)) +
    geom_sf(color = "white", lwd = 0.05) +
    labs(title = "Percent Same House 2017") + theme_minimal() + theme(axis.line=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),
                                                axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank(),
                                                panel.grid.minor=element_blank(),
                                                panel.grid.major = element_line(colour = "white")) + 
    scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                         limits=c(0, 0.8))
}


ggplot(waDem_est %>% 
         filter(Year == 2017, NAME %nin% c("Census Tract 328, King County, Washington", 
                                           "Census Tract 327.04, King County, Washington", 
                                           "Census Tract 327.03, King County, Washington",
                                           "Census Tract 327.02, King County, Washington", 
                                           "Census Tract 327.01, King County, Washington", 
                                           "Census Tract 326.02, King County, Washington",
                                           "Census Tract 327.01, King County, Washington",
                                           "Census Tract 315.02, King County, Washington", 
                                           "Census Tract 315.01, King County, Washington")), 
       aes(fill = pct.migtant.overseas)) +
  geom_sf(color = "white", lwd = 0.05) +
  labs(title = "2017") + theme_minimal() + theme(axis.line=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks=element_blank(),
                                              axis.title.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              panel.grid.minor=element_blank(),
                                              panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), na.value = "transparent",
                       limits=c(0, 0.2))





mymap <- ggplot(waDem_est, 
                aes(fill = pct.abovepov)) + 
  geom_sf(col = "white", 
          lwd = 0.05, 
          position = "identity") + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), 
                       na.value = "transparent", 
                       limits = c(.25, .95)) +
  coord_sf() +
  labs(title = "Percent Above Povertyline in {current_frame}") +
  transition_manual(Year)

mymap <- ggplot(waDem_est, 
                aes(fill = pct.abovepov)) + 
  geom_sf(col = "white", 
          lwd = 0.05, 
          position = "identity") + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), 
                       na.value = "transparent", 
                       limits = c(.25, .95)) +
  coord_sf() +
  labs(title = "Percent Above Povertyline in {frame_time}") +
  transition_time(Year)

gganimate::animate(mymap)



ggplot(waDem_est %>% filter(Year == 2014), 
       aes(fill = pct.abovepov)) + 
  geom_sf(col = "white", 
          lwd = 0.05, 
          position = "identity") + 
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major = element_line(colour = "white")) + 
  scale_fill_gradientn(colours = plasma(256), 
                       na.value = "transparent", 
                       limits = c(.25, .95)) +
  coord_sf() +
  labs(title = "Percent Above Povertyline in 2014") 




## dark as highest


eviction <- read_csv(here("Datasets", "evictions.csv")) %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))

foreclosure <- read_csv(here("Datasets", "forecloseWATract.csv"))

waDem_est2 <- merge(waDem_est, eviction,  
                    by.x = c("GEOID", "Year"), 
                    by.y = c("GEOID", "year"))
waDem_est3 <- merge(waDem_est2, foreclosure, 
                    by.x = c("GEOID", "Year"), 
                    by.y = )

lasso.ss(waDem_est2$`eviction-rate`, data.frame(waDem_est[,-c(21, 22, 23, 24)]))

waDem_est2_2 <- merge(waDem_est2, data.frame(waDem_extra2), 
                      by.x = c("GEOID", "Year"), 
                      by.y = c("GEOID", "Year"))

waDem_est2_2 <- waDem_est2 %>% st_set_geometry(NULL)

waDem_est_predictors <- data.frame(waDem_est2_2)[complete.cases(data.frame(waDem_est2_2)), ]
waDem_est_y <- waDem_est_predictors$eviction.rate

waDem_est_predictors <- (waDem_est_predictors[,35:81])[,-c(26, 27, 28, 30, 44, 45, 47)] %>% as.matrix()
waDem_est_predictors <- matrix(mapply(waDem_est_predictors, FUN = as.numeric), 
                               ncol = 40)

lasso.fit <- cv.glmnet(x = waDem_est_predictors[,-40], 
                    y = waDem_est_y, 
                    family = "gaussian")

lasso.fit$lambda.min
lasso.fit$lambda.1se

coef(lasso.fit, lasso.fit$lambda.1se)
### picks out intercept, 8, 17, 25, 28, 29, 32


## pct youth, pct 75000.0, median property value, pct af am, pct hispanic, pct nh pi

coef(lasso.fit, lasso.fit$lambda.min)

names.dat <- colnames(waDem_est_predictors)

data(X)
data(y)
a.lst <- list(NULL)
a.lst[[1]] <- 1
dim(a.lst[[1]]) <- c(1,1)
dimnames(a.lst[[1]]) <- list(NULL, names.X[1])
a.lst[[2]] <- diag(2)
dimnames(a.lst[[2]]) <- list(NULL, names.X[c(2,3)])
a.lst[[3]] <- diag(2)
dimnames(a.lst[[3]]) <- list(NULL, names.X[c(4,5)])


ss <- c("ind", "ss1", "ss2")
mod_lasso.ss <- lasso.ss(y, X, ss, a.lst, S.v, C.v, c("black", "red", "green"))

S.v <- c(1,2,2)
C.v <- rep(0,length(a.lst))


lasso.ss(y = waDem_est_y, 
         X = waDem_est_predictors, 
         ss = NA,
         
         C.v <- rep(0,length(a.lst)))






