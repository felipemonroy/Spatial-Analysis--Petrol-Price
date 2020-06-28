library(sp)
library(rgdal)
library(tidyverse)
library(snow)
library(foreach)
library(here)
library(doParallel)

level1_map<- readOGR(dsn="05 Maps Data", layer="SA1_2016_AUST")

sum_data <- readRDS(here("04 Tidy Data","petrol_station_data.rds"))

rev_geo<-function(lat,long){
            points<-SpatialPoints(matrix(c(long,lat),ncol=2,nrow=1))
            proj4string(points) <- proj4string(level1_map)
            info<-over(points,level1_map)
            return(info)
}

cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)

map_info<-foreach(i=1:nrow(sum_data), 
                  .packages = c("sp", "rgdal"), .combine=rbind) %dopar% {
                              rev_geo(as.numeric(sum_data[i,"lat"]),
                                      as.numeric(sum_data[i,"lon"]))
                  }

stopCluster(cl)

stations_SA<-cbind(sum_data[,"ID"],map_info)

saveRDS(stations_SA,here("04 Tidy Data","petrolstations_SA.rds"))
