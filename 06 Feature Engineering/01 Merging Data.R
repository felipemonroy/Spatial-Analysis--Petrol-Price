library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(zoo)
library(ggmap)
library(geosphere)

################################################################################
#
#                      Adding Brand Size
#
################################################################################
sum_data<- readRDS(here("04 Tidy Data","petrol_station_data.rds"))

bigstationsnames<-c("7-Eleven", "Caltex Woolworths", "Liberty", "Shell", "BP",
                    "Coles Express", "Metro Fuel", "United", "Caltex", "Costco",
                    "Puma Energy")
bigstationsnames<-tolower(bigstationsnames)

brands<-sum_data %>% group_by(brand) %>%
            summarise() %>% ungroup() %>%
            mutate(brand_size=ifelse(brand %in% bigstationsnames,
                                     "big","small"))
rm(bigstationsnames)
################################################################################
#
#                      Adding Distance Index
#
################################################################################
#Creating distance matrix
distance_matrix<-distm(as.matrix(sum_data[,c("lon","lat")]), 
                       fun = distHaversine)
#To mile
distance_matrix<-distance_matrix*0.621371/1000

distance_matrix<-data.frame(distance_matrix)
names(distance_matrix)<-sum_data$ID
distance_matrix$ID_1<-sum_data$ID

distance_df<-distance_matrix %>%
            gather(key="ID_2",value="distance",-ID_1) %>%
            filter(!ID_1==ID_2,
                   distance<=1) %>%
            mutate(sq_sim=1-distance^2) %>%
            group_by(ID_1) %>%
            summarise(sim_index=sum(sq_sim),
                      count=n()) %>% ungroup() %>%
            mutate(scaled_sim_index=sim_index/max(sim_index)) %>%
            rename(ID=ID_1) %>%
            dplyr::select(ID,competition_index=scaled_sim_index)

competition_index<-sum_data[,"ID"] %>% left_join(distance_df) %>%
            ungroup %>%
            dplyr::select(ID,competition_index) %>%
            mutate(competition_index=ifelse(is.na(competition_index),0,
                                            competition_index))

################################################################################
#
#                      Stations Between 1 mile
#
################################################################################
total_stations_1m<-distance_matrix %>%
            gather(key="ID_2",value="distance",-ID_1) %>%
            filter(!ID_1==ID_2,
                   distance<=1) %>%
            group_by(ID_1) %>%
            summarise(stations_1mile=n()) %>%
            rename(ID=ID_1)

same_stations_1m<-distance_matrix %>%
            gather(key="ID_2",value="distance",-ID_1) %>%
            filter(!ID_1==ID_2,
                   distance<=1) %>%
            left_join(sum_data[,c("ID","brand")],by=c("ID_1"="ID")) %>%
            rename(brand_1=brand) %>%
            mutate(ID_2=as.numeric(ID_2)) %>%
            left_join(sum_data[,c("ID","brand")],by=c("ID_2"="ID")) %>%
            rename(brand_2=brand) %>%
            mutate(brand_1=case_when(
                        brand_1=="caltex woolworths"~"caltex",
                        brand_1=="coles express"~"shell",
                        TRUE~brand_1
            )) %>%
            mutate(brand_2=case_when(
                        brand_2=="caltex woolworths"~"caltex",
                        brand_2=="coles express"~"shell",
                        TRUE~brand_2
            )) %>%
            filter(brand_1==brand_2) %>%
            group_by(ID_1) %>%
            summarise(same_brand_1mile=n()) %>%
            rename(ID=ID_1)

stations_1mile<-sum_data[,"ID"] %>% left_join(total_stations_1m) %>%
            left_join(same_stations_1m) %>%
            ungroup() %>%
            mutate(stations_1mile=ifelse(is.na(stations_1mile),0,
                                         stations_1mile),
                   same_brand_1mile=ifelse(is.na(same_brand_1mile),0,
                                           same_brand_1mile),
                   distinct_brand_1mile=stations_1mile-same_brand_1mile)

################################################################################
#
#                      Merging Data
#
################################################################################
#Reading Census Data
SA2_data<- readRDS(here("04 Tidy Data","SA2_data.rds"))

#Creating two new variables SA2_data
SA2_data<- SA2_data %>% mutate(pop_density=population/AREASQKM16,
                               jobs_density=jobs_number/AREASQKM16,
                               cars_density=total_cars/AREASQKM16,
                               house_density=total_dwelings/AREASQKM16)

#Reading SA per station
petrolstations_SA<- readRDS(here("04 Tidy Data","petrolstations_SA.rds")) %>%
            dplyr::select(ID,SA2_MAIN16) %>%
            mutate(SA2_MAIN16=as.numeric(SA2_MAIN16))

#Merging Data
merged_station_data<-sum_data %>% left_join(petrolstations_SA) %>% #Adding SA
            left_join(SA2_data) %>%   #Adding SA2 data
            left_join(brands) %>% #Adding brand size
            left_join(competition_index) %>% #Adding competition_index
            left_join(stations_1mile) #Adding stations in 1 mile

saveRDS(merged_station_data,here("07 Processed Data","merged_station_data.rds"))
saveRDS(SA2_data,here("07 Processed Data","SA2_data.rds"))

