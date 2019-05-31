set.seed(1221223)
rm(list = ls())
##Mapping
library(rgeos)
library(maptools)
library("ggplot2")
library(broom) ## for converting a map to a data frame
library(INLA)
library(spdep)
## coloring the spplot
library(colorspace)

## load the evictions data
## load in the evictions data

evictions <- read.csv("../sdss2019_data_hack/Datasets/evictions.csv") %>% 
  filter(year==2013 & parent.location=="King County, Washington")  %>% 
  arrange(GEOID) %>% 
  mutate(id=1:n()) %>% 
  #mutate(id=as.character(c)) %>% 
  mutate(evictions_calc = evictions/population)
glimpse(evictions)

## recooments this struggling with changing the id
#king_shp <- rgdal::readOGR("king_shape/king_spatial.shp")


king_shp <- maptools::readShapePoly( "king_shape/king_spatial.shp",
                                       IDvar="GEOID",
                                     proj4string=CRS("+proj=longlat +ellps=clrk66"))

king_shp <- geo_join(spatial_data = king_shp, evictions, by = "GEOID") 
length(unique(king_shp$id))
## save a shape file to be used with inla
temp <- spdep::poly2nb(king_shp )
glimpse(temp)
nb2INLA("king_shape/king_Arc.graph", temp)
king_adj <- "king_shape/king_Arc.graph"





###defining the priors
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3

#'         \\\_Model_1_\\\         #  
###############MOdel 1 #######################################################    
## spatial unstructured
formulaUH0 <- eviction.rate ~ rent.burden +
  f(GEOID, model = "iid",prior="normal",param=c(0, 0.001) , initial = 1)

resultUH0 <- inla(formulaUH0,family="nbinomial",
                  data=evictions, control.compute=list(dic=TRUE,cpo=TRUE),#E=log(nagem) ,
                  control.predictor = list(compute = TRUE))


##summary in 3 decimal places
summary(resultUH0)
exp(resultUH0$summary.fixed)
pdresultUH0 <- resultUH0$dic$p.eff



###############MOdel 1B#######################################################  
### spatial model structured and unstrustured  without 
### to comapare with Winbugs
formulaUHB <-eviction.rate ~ rent.burden +
  f(id, model = "bym"  ,graph=king_adj , scale.model=TRUE,
    hyper=list(prec.unstruct=list(prior="loggamma",param=c(0.0111,0.001)),
               prec.spatial=list(prior="loggamma",param=c(0.0011,0.001)))) 


resultUHB <- inla(formulaUHB,family="nbinomial",
                  data=evictions, control.compute=list(dic=TRUE,cpo=TRUE)#,E=log(nagem)
                  ,control.predictor(compute=TRUE))
summary(resultUHB)
pdresultUHB <- resultUHB$dic$p.eff 
exp(resultUHB$summary.fixed)



csi <- resultUHB$marginals.random$id[1:398]

## then apply the exponential transformation and 
# calculate the posterior mean for each of   them using the lapply function.
zeta <- lapply(csi,function(x) inla.emarginal(exp,x))
##define the cut offs for your risk ratio
zeta_cutoff <- c(0.5, 1, 1.5 ,2,2.5,3, 3.8)

#Transform zeta in categorical variable
cat_zeta <- cut(unlist(zeta),breaks=zeta_cutoff,
                include.lowest=TRUE )

#Create a dataframe with all the information needed for the map
maps_cat_zeta <- data.frame(unique(evictions$id), cat_zeta=cat_zeta)

#Add the categorized zeta to the kilifi spatial polygon
## 
data_king <- attr(king_shp, "data")
attr(king_shp, "data") <- merge(data_king, maps_cat_zeta,
                                  by.x="id" , by.y="unique.evictions.id.")

## mapping the risk ratio 
#spplot(obj=kilifi_sub, zcol= "cat.zeta", col.regions=gray(seq(0.9,0.1,length=4)), asp=1)
spplot(obj=king_shp, zcol= "cat_zeta",col.regions=diverge_hsv(8), scales=list(draw = TRUE), asp=1)
