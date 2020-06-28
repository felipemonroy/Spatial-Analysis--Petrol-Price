library(spgwr)
library(here)
library(tidyverse)
library(sp)
library(rgdal)
library(automap)
library(broom)
library(raster)

stations_data_nona<-read_rds(here("09 Modelling Files","input_data.rds")) %>%
            rename(IRSED_Decile=`IRSED Decile`,
                   IER_Decile=`IER Decile`)
coordinates(stations_data_nona)<- ~ lon + lat
stations_data_nona@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

set.seed(20)
GWRbandwidth <- gwr.sel(price~brand_size+distinct_brand_1mile+IER_Decile+
                                    cars_per_dwelings+
                                    avg_comm_distance+
                                    percentage_car_travel+
                                    pop_density, 
                        data=stations_data_nona,adapt=T)

gwr.model = spgwr::gwr(price~brand_size+distinct_brand_1mile+IER_Decile+
                            cars_per_dwelings+
                            avg_comm_distance+
                            percentage_car_travel+
                            pop_density,
                data = stations_data_nona, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

#####################################################
#
#     Interpreting results
#
#####################################################
nsw_map<- readOGR(dsn="05 Maps Data", layer="SA2_2016_AUST")
nsw_map_sub<-nsw_map[nsw_map$GCC_NAME16=="Greater Sydney",]

results <-as.data.frame(gwr.model$SDF)

#####################################################
#
#     distinct_brand_1mile
#
#####################################################

#Plot points (competition index)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = distinct_brand_1mile),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

#Creating Grid
grid <- raster(extent(nsw_map_sub), resolution = c(0.01,0.01), 
               crs = proj4string(nsw_map_sub))
gridPolygon <- rasterToPolygons(grid)
intersectGridClipped <- raster::intersect(gridPolygon, nsw_map_sub)
intersectGridClipped<-spTransform(intersectGridClipped, 
                                  CRS("+proj=utm +zone=10 +datum=WGS84"))

#Transforming points
stations_coord<-results
coordinates(stations_coord)<- ~ lon + lat
class(stations_coord)
bbox(stations_coord)
#Transforming coordinates
stations_coord@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
stations_coord<-spTransform(stations_coord, CRS("+proj=utm +zone=10 +datum=WGS84"))

lzn.kriged = autoKrige(formula=distinct_brand_1mile~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_competition.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_competition.rds"))

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
#     IER_Decile
#
#####################################################
#Plot points (IER_Decile)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = IER_Decile),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=IER_Decile~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_IER.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_IER.rds"))

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
#     Brand_size
#
#####################################################
#Plot points (Brand size)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = brand_sizesmall),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=brand_sizesmall~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_brandsize.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_brandsize.rds"))

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
#     cars_per_dwelings
#
#####################################################
#Plot points (cars_per_dwelings)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = cars_per_dwelings),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=cars_per_dwelings~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_carsperdw.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_carsperdw.rds"))

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
#     avg_comm_distance
#
#####################################################
#Plot points (avg_comm_distance)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = avg_comm_distance),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=avg_comm_distance~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_avgdist.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_avgdist.rds"))

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
#     percentage_car_travel
#
#####################################################
#Plot points (percentage_car_travel)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = percentage_car_travel),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=percentage_car_travel~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_perccartravel.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_perccartravel.rds"))

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
#     pop_density
#
#####################################################
#Plot points (pop_density)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = pop_density),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=pop_density~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_popdensity.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_popdensity.rds"))

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
#     R2
#
#####################################################
#Plot points (localR2)
ggplot() + geom_polygon(data = nsw_map_sub, 
                        aes(x = long, y = lat, group = group),
                        fill = NA, colour = "black", alpha=0.3) + 
            geom_point(alpha = 0.7,
                       aes(x = lon, y = lat,color = localR2),
                       data = results)+
            scale_color_gradientn(colours = rainbow(5))+
            theme_bw() +
            xlab("Longitude") +
            ylab("Latitude") + 
            coord_quickmap()

lzn.kriged = autoKrige(formula=localR2~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_localr2.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_localr2.rds"))

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
#   Competition effect (cents)
#
#####################################################
competition_index_value<-stations_data_nona$competition_index
competition_index_coeff<-stations_coord$competition_index

competition_df<-cbind(competition_index_value,competition_index_coeff)
names(competition_df)<-c("value","coefficient")
competition_df<- competition_df %>% mutate(effect=value*coeff)

suplim<-quantile(competition_df$coefficient,0.975)
inflim<-quantile(competition_df$coefficient,0.025)
ggplot(competition_df,aes(x=value,y=coefficient))+geom_point()+
            geom_smooth(method='lm')+
            geom_hline(yintercept = inflim)+
            geom_hline(yintercept = suplim)+
            xlab("Competition Index")+
            ylab("Coefficient")

suplim<-quantile(competition_df$effect,0.975)
inflim<-quantile(competition_df$effect,0.025)
ggplot(competition_df,aes(x=value,y=coefficient))+geom_point()+
            geom_smooth(method='lm')+
            geom_hline(yintercept = inflim)+
            geom_hline(yintercept = suplim)+
            xlab("Competition Index")+
            ylab("Effect on Price")

#Interpolation effect
stations_coord<-results
stations_coord$competition_effect<-competition_df$effect
coordinates(stations_coord)<- ~ lon + lat
class(stations_coord)
bbox(stations_coord)
#Transforming coordinates
stations_coord@proj4string@projargs<-"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
stations_coord<-spTransform(stations_coord, CRS("+proj=utm +zone=10 +datum=WGS84"))
lzn.kriged = autoKrige(formula=effect~1,
                       input_data=stations_coord, 
                       new_data=intersectGridClipped)

saveRDS(lzn.kriged,here("09 Modelling Files","lzn_kriged_compeffect.rds"))
lzn.kriged<-readRDS(here("09 Modelling Files","lzn_kriged_compeffect.rds"))

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
