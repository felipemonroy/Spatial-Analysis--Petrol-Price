########################################################################
#
#              Loading Packages
#
#######################################################################

library(sp)
library(rgdal)
library(tidyverse)
library(snow)
library(foreach)
library(here)
library(doParallel)
library(maptools)
library(rgeos)
library(readxl)
library(ggpubr)
library(tmap)
library(spdep)
library(raster)
library(gstat)
library(automap)
library(broom)
library(ape)

#######################################################################
#
#              Reading files and maps
#
#######################################################################

stations_data <- readRDS(here("07 Processed Data","merged_station_data.rds"))
nsw_map<- readOGR(dsn="05 Maps Data", layer="SA2_2016_AUST")

#######################################################################
#
#              Modifying map and datasets Greater Sydney
#
#######################################################################
#I do not have enough points for all NSW but I have for Greater Sydney
#Subsetting map
nsw_map_sub<-nsw_map[nsw_map$GCC_NAME16=="Greater Sydney",]

#Subsetting Datasets
stations_data<-stations_data %>% 
            filter(SA2_MAIN16 %in% nsw_map_sub@data$SA2_MAIN16)

#######################################################################
#
#              Mapping to see representativity
#
#######################################################################
ggplot() +  geom_point(colour = "blue", alpha = 0.3,
                       aes(x = lon, y = lat),
                       data = stations_data) +
            geom_point(colour = "red", alpha = 0.3,
                       aes(x = lon, y = lat),
                       data = stations_data[which(!is.na(stations_data$price)),]) +
            geom_polygon(data = nsw_map_sub, 
                         aes(x = long, y = lat, group = group),
                         fill = NA, colour = "black", alpha=0.3)+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude")

#Most points are red, which means that I am considering it in my sample
            
#####################################################
#
#          Density to see representativity
#
######################################################
ggplot() + stat_density2d(data=stations_data,
                          geom="tile", 
                          aes(x=lon,y=lat,fill = ..density..),
                          contour = FALSE)+
                        scale_fill_gradient(low = "white", high = "blue")+
                        geom_polygon(data = nsw_map_sub, 
                                     aes(x = long, y = lat, group = group),
                                     fill = NA, colour = "black", alpha=0.3)

ggplot() + stat_density2d(data=stations_data[which(!is.na(stations_data$price)),],
                          geom="tile", 
                          aes(x=lon,y=lat,fill = ..density..),
                          contour = FALSE)+
            scale_fill_gradient(low = "white", high = "blue")+
            geom_polygon(data = nsw_map_sub, 
                         aes(x = long, y = lat, group = group),
                         fill = NA, colour = "black", alpha=0.3)
            
#Density appears to be distibuted the same way in both maps

#######################################################################
#
#                    Quick EDA for lm
#
#######################################################################
#Per brand
stations_data%>%ggplot(aes(x=brand,y=price)) +
                        geom_boxplot()

#Per brand size
stations_data%>%ggplot(aes(x=brand_size,y=price)) +
            geom_boxplot()

#Per competitive index
stations_data%>%ggplot(aes(x=competition_index,y=price)) +
                        geom_point()+
                        geom_smooth(method='lm')+
                        stat_cor(method="pearson")

#Per number of close stations
stations_data%>%ggplot(aes(x=stations_1mile,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per IER Decile
stations_data%>%ggplot(aes(x=`IER Decile`,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per IER Score
stations_data%>%ggplot(aes(x=`IER Score`,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per population
stations_data%>%ggplot(aes(x=population,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per population density (almost no change)
stations_data%>%ggplot(aes(x=pop_density,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per number of jobs
stations_data%>%ggplot(aes(x=jobs_number,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per job density
stations_data%>%ggplot(aes(x=jobs_density,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per cars per dweling (almost no change)
stations_data%>%ggplot(aes(x=cars_per_dwelings,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per vehicle travels
stations_data%>%ggplot(aes(x=percentage_car_travel,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Per avg travel distance (almost no change)
stations_data%>%ggplot(aes(x=avg_comm_distance,y=price)) +
            geom_point()+
            geom_smooth(method='lm')+
            stat_cor(method="pearson")

#Bests:
#Brand Size

corr_matrix <- cor(stations_data[,c(8,12:19)], use = "complete.obs")
#IRSED is the best correlated with price

corr_matrix <- cor(stations_data[,c(8,13,20:31,33:34)], use = "complete.obs")

#######################################################################
#
#                    lm
#
#######################################################################
library(AICcmodavg)
library(car)
library(kableExtra)
library(lmtest)
library(caret)

lm_model<-lm(price~`IER Decile`+population+percentage_car_travel+cars_per_dwelings+
                         total_cars+total_dwelings+employed_number+avg_comm_distance+
                         jobs_number+pop_density+jobs_density+brand_size+competition_index+
                         stations_1mile+ cars_density,
             data=stations_data)

#Collinearily
vif(lm_model)
#Is less than 5 so there are not collinearity

#Var Imp
varimp<-varImp(lm_model,scale=TRUE)
varimp$feature<-rownames(varimp)
varimp<-varimp %>% arrange((Overall))
varimp$feature<-factor(varimp$feature,levels = as.character(varimp$feature))
ggplot(varimp,aes(x=Overall,y=feature))+
            geom_col()+
            xlab("t value")+
            ylab("Feature")+
            theme_light()+
            geom_vline(xintercept = 1.959976, linetype="dotted", 
                       color = "red", size=1)

summary(lm_model)

########################################
#     Final linear model
########################################

lm_model<-lm(price~brand_size+distinct_brand_1mile+percentage_car_travel+
                         `IER Decile`+cars_per_dwelings+avg_comm_distance+
                         pop_density,
             data=stations_data)

vif(lm_model)

varimp<-varImp(lm_model,scale=TRUE)
varimp$feature<-rownames(varimp)
varimp<-varimp %>% arrange((Overall))
varimp$feature<-factor(varimp$feature,levels = as.character(varimp$feature))
ggplot(varimp,aes(x=Overall,y=feature))+
            geom_col()+
            xlab("t value")+
            ylab("Feature")+
            theme_light()+
            geom_vline(xintercept = 1.959976, linetype="dotted", 
                       color = "red", size=1)

summary(lm_model)


#Creating df with no NA
stations_data_nona<-stations_data %>% filter(!is.na(price),
                                             !is.na(brand_size),
                                             !is.na(distinct_brand_1mile),
                                             !is.na(percentage_car_travel),
                                             !is.na(`IER Decile`),
                                             !is.na(cars_per_dwelings),
                                             !is.na(avg_comm_distance),
                                             !is.na(pop_density))
stations_data_nona$residuals<-as.numeric(lm_model$residuals)

saveRDS(stations_data_nona,here("09 Modelling Files","input_data.rds"))

#####################################################
#
#       Testing for spatial autocorrelation (Maps)
#
######################################################
############################
#Points of residuales
############################
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = residuals),
                       data = stations_data_nona)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

###########################
#Interpolation
###########################
#Creating grid
#https://rpubs.com/huanfaChen/grid_from_polygon
grid <- raster(extent(nsw_map_sub), resolution = c(0.01,0.01), 
               crs = proj4string(nsw_map_sub))
gridPolygon <- rasterToPolygons(grid)
#plot(nsw_map_sub)
#plot(gridPolygon, add=T)
intersectGridClipped <- raster::intersect(gridPolygon, nsw_map_sub)
#plot(intersectGridClipped)
#Transform coordinates
intersectGridClipped<-spTransform(intersectGridClipped, 
                                  CRS("+proj=utm +zone=10 +datum=WGS84"))

#Transforming points
stations_coord<-stations_data_nona
coordinates(stations_coord)<- ~ lon + lat
class(stations_coord)
bbox(stations_coord)
#Transforming coordinates
stations_coord@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
stations_coord<-spTransform(stations_coord, CRS("+proj=utm +zone=10 +datum=WGS84"))

#Plot variogram
lzn.vgm <- variogram(residuals~1, stations_coord)
lzn.fit <- fit.variogram(lzn.vgm, model=vgm("Sph"))
lzn.fit
plot(lzn.vgm,lzn.fit)

#Interpolation
lzn.kriged = autoKrige(formula=residuals~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_residuals.rds"))
lzn.kriged <- readRDS(here("09 Modelling Files","lzn_kriged_residuals.rds"))

lzn.kriged<-spTransform(lzn.kriged$krige_output, 
                  CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

tidy_spdf = tidy(lzn.kriged)
tidy_var = lzn.kriged@data
tidy_var$id = as.character(1:nrow(tidy_var))

plotData = left_join(tidy_spdf,tidy_var,by='id')

ggplot()+geom_polygon(data=plotData, aes(fill=var1.pred,x=long,y=lat,group=group))+
            scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
            geom_polygon(data = nsw_map_sub, 
                         aes(x = long, y = lat, group = group),
                         fill = NA, colour = "black", alpha=0.3)

#####################################################
#
#       Testing for spatial autocorrelation
#
######################################################
#https://cran.r-project.org/web/packages/lctools/lctools.pdf
library(spgwr)
library(lctools)

stations_data_nona<-read_rds(here("09 Modelling Files","input_data.rds")) %>%
            rename(IRSED_Decile=`IRSED Decile`,
                   IER_Decile=`IER Decile`)
coordinates(stations_data_nona)<- ~ lon + lat
stations_data_nona@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

set.seed(20)
GWRbandwidth <- gwr.sel(price~brand_size+distinct_brand_1mile+percentage_car_travel+
                                    IER_Decile+cars_per_dwelings+avg_comm_distance+
                                    pop_density, 
                        data=stations_data_nona,adapt=T)
#Global Moran
globalmoran<-moransI(data.frame(x=stations_data_nona$lon,y=stations_data_nona$lat), 
                      GWRbandwidth, 
                      as.vector(stations_data_nona$price), 
                      WType='Binary')
#Local Moran
localmoran<-l.moransI(data.frame(x=stations_data_nona$lon,y=stations_data_nona$lat), 
          GWRbandwidth, 
          as.vector(stations_data_nona$price), 
          WType='Binary', 
          scatter.plot = TRUE, 
          family = "adaptive")

#Df for plotting
temp<-data.frame(cbind(stations_data_nona$ID,stations_data_nona$lon,stations_data_nona$lat))
names(temp)<-c("ID","lon","lat")
temp<-cbind(temp,localmoran[,2:9])

#Addding high high low low
temp<-temp %>% mutate(quad_sig=case_when(
            p.value<=0.05 & Xi>0 & wXj>0 ~ "high-high",
            p.value<=0.05 & Xi<=0 & wXj<=0 ~ "low-low",
            p.value<=0.05 & Xi>0 & wXj<=0 ~ "high-low",
            p.value<=0.05 & Xi<=0 & wXj>0 ~ "low-high",
            TRUE ~ "non-significant"
))

#Plot points
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = p.value),
                       data = temp)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = quad_sig),
                       data = temp)+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

#Plot areas
#Transforming points
stations_coord<-temp
coordinates(stations_coord)<- ~ lon + lat
class(stations_coord)
bbox(stations_coord)
#Transforming coordinates
stations_coord@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
stations_coord<-spTransform(stations_coord, CRS("+proj=utm +zone=10 +datum=WGS84"))


#Plot variogram
lzn.vgm <- variogram(p.value~1, stations_coord)
lzn.fit <- fit.variogram(lzn.vgm, model=vgm("Sph"))
lzn.fit
plot(lzn.vgm,lzn.fit)

#Interpolation
lzn.kriged = autoKrige(formula=p.value~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_p_value.rds"))
lzn.kriged <- readRDS(here("09 Modelling Files","lzn_kriged_p_value.rds"))

lzn.kriged<-spTransform(lzn.kriged$krige_output, 
                        CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
tidy_spdf = tidy(lzn.kriged)
tidy_var = lzn.kriged@data
tidy_var$id = as.character(1:nrow(tidy_var))
plotData = left_join(tidy_spdf,tidy_var,by='id')

ggplot()+geom_polygon(data=plotData, aes(fill=var1.pred,x=long,y=lat,group=group))+
            scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred")+
            geom_polygon(data = nsw_map_sub, 
                         aes(x = long, y = lat, group = group),
                         fill = NA, colour = "black", alpha=0.3)

#####################################################
#
#       Other Autocorrelation Tests
#
######################################################
#Testing autocorrelation
dists <- as.matrix(dist(cbind(stations_data_nona$lon, stations_data_nona$lat)))
dists.inv <- 1/dists
diag(dists.inv) <- 0
Moran.I(stations_data_nona$residuals, dists.inv)

#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
#Based on these results, we can reject the null hypothesis that there 
#is zero spatial autocorrelation present in the variable residuals 
#at alpha = .05.

####################

xy<-cbind(stations_data_nona$lon,stations_data_nona$lat)
test<-knn2nb(knearneigh(xy,4))
temp<-nb2listw(test)

moran.test(stations_data_nona$residuals, temp)
local <- localmoran(x = stations_data_nona$residuals, 
                    listw = nb2listw(test, style = "W"))

temp<-cbind(stations_data_nona,data.frame(local))