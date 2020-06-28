library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggmap)
library(here)
library(geosphere)
library(readxl)
library(sp)
library(rgdal)
library(snow)
library(foreach)
library(doParallel)
library(maptools)
library(rgeos)
library(ggpubr)
library(tmap)
library(spdep)
library(raster)
library(gstat)
library(automap)
library(broom)
library(ape)

#####################################################################
#
#          Summarizing Petrol Station Data
#
#####################################################################
Geocoded_petrol_data<- readRDS(here("04 Tidy Data","Geocoded_petrol_data.rds"))

#Filtering only E10 (has more data) and putting only 1 obs per day
Geocoded_petrol_data <- Geocoded_petrol_data %>% ungroup() %>%
            mutate(date=ymd_hms(date)) %>%
            mutate(date=date(date)) %>%
            filter(fuel=="E10") %>%
            group_by_at(vars(-price)) %>%
            summarise(price=mean(price)) %>%
            mutate(week_num=week(date))

#Checking which day has more data
temp<-Geocoded_petrol_data %>% group_by(date,ID) %>%
            summarise() %>% group_by(date) %>%
            summarise(count=n())
max(temp$count)
temp[which(temp$count==max(temp$count)),"date"]
#2019-11-29 has more stations

#Getting prices
temp<-Geocoded_petrol_data %>% filter(date==ymd("2019/11/29")) %>% ungroup() %>%
            dplyr::select(ID,price)  %>%
            distinct()

#Summarizing data +/- 15 days
sum_data<-Geocoded_petrol_data %>% filter(date>=ymd("2019/11/29")-15,
                                             date<=ymd("2019/12/14")+15) %>% 
            ungroup() %>%
            group_by(ID,brand,station_name,suburb,postcode,lon,lat) %>%
            summarise() %>%
            left_join(temp)
#Deleting duplicated values
sum_data<-sum_data[which(!duplicated(sum_data$ID)),]
sum(duplicated(sum_data$ID))

saveRDS(sum_data,here("04 Tidy Data","petrol_station_data.rds"))

#####################################################################
#
#          Tidying Socio Economic Index Data
#
#####################################################################
sum_data<-readRDS(here("04 Tidy Data","petrol_station_data.rds"))

#I am going to only use SA2 data. SA1 data has too much NAs

SEIFA_SA2<-read_xls(here("02 Raw Bureau Data","2033055001 - sa2 indexes.xls"),
                    sheet=2,skip=5)
names(SEIFA_SA2)<-c("SA2_MAIN16","SA2_name","IRSED Score", "IRSED Decile",
                    "IRSEAD Score", "IRSEAD Decile", "IER Score",
                    "IER Decile", "IEO Score", "IEO Decile", "population")

SEIFA_SA2<-SEIFA_SA2 %>% filter(!is.na(SA2_MAIN16))
temp<-SEIFA_SA2$SA2_name
SEIFA_SA2<-SEIFA_SA2 %>% mutate_if(is.character,as.numeric)
SEIFA_SA2$SA2_name<-temp

#####################################################################
#
#          Tyding other Census information
#
#####################################################################

############################
#   Mode of transportation
############################
travel_data_p1<-read_csv(here("02 Raw Bureau Data","2016Census_W22D_NSW_SA2_travel.csv"))
travel_data_p2<-read_csv(here("02 Raw Bureau Data","2016Census_W22E_NSW_SA2_travel.csv"))

travel_data<-travel_data_p1 %>% left_join(travel_data_p2) %>%
            dplyr::select(SA2_MAINCODE_2016,P_M1_CarAsDriv_Tot,P_Tot_Tot) %>%
            mutate(percentage_car_travel=P_M1_CarAsDriv_Tot/P_Tot_Tot) %>%
            dplyr::select(SA2_MAINCODE_2016,percentage_car_travel) %>%
            rename(SA2_MAIN16=SA2_MAINCODE_2016)
rm(travel_data_p1)
rm(travel_data_p2)

############################
#   Number of vehicles
############################
vehicles<-read_csv(here("02 Raw Bureau Data","2016Census_G30_NSW_SA2_vehicles.csv"))

vehicles<- vehicles %>% 
            mutate(total_cars=Num_MVs_per_dweling_1_MVs*1+Num_MVs_per_dweling_2_MVs*2+
                               Num_MVs_per_dweling_3_MVs*3+Num_MVs_per_dweling_4mo_MVs*4) %>%
            mutate(cars_per_dwelings=total_cars/Total_dwelings) %>%
            dplyr::select(SA2_MAINCODE_2016,cars_per_dwelings,total_cars,
                          total_dwelings=Total_dwelings) %>%
            rename(SA2_MAIN16=SA2_MAINCODE_2016)

############################
#   Average Distance
############################
distance<-read_xls(here("02 Raw Bureau Data","2071055001 - commuting distance from usual residence.xls"),
                   skip=5, sheet=9)

distance<- distance %>% filter(!is.na(`Statistical Area Level 2 code`)) %>%
            dplyr::select(SA2_MAINCODE_2016=`Statistical Area Level 2 code`,
                          employed_number=`Number of employed people living in region (no.)`,
                          avg_comm_distance=`Average commuting distance (kms)`) %>%
            rename(SA2_MAIN16=SA2_MAINCODE_2016)

############################
#   Jobs
############################
jobs<-read_xls(here("02 Raw Bureau Data","61600ds0003_2015-16_jobs.xls"),
               skip=6, sheet=7)

jobs<-jobs %>% mutate(SA2=as.numeric(SA2)) %>%
            filter(!is.na(SA2))
jobs<-jobs[,c(1,22)]
names(jobs)<-c("SA2_MAIN16","jobs_number")
jobs$jobs_number<-as.numeric(jobs$jobs_number)*1000

#####################################################################
#
#          Area per SA2
#
#####################################################################
nsw_map<- readOGR(dsn="05 Maps Data", layer="SA2_2016_AUST")

map_data<-nsw_map@data %>% 
            dplyr::select(SA2_MAIN16,AREASQKM16) %>%
            mutate(SA2_MAIN16=as.numeric(SA2_MAIN16))

#####################################################################
#
#          Merging SA2 data
#
#####################################################################
SA2_data<-map_data %>% left_join(SEIFA_SA2) %>% left_join(travel_data) %>%
            left_join(vehicles) %>% left_join(distance) %>% left_join(jobs)

saveRDS(SA2_data,here("04 Tidy Data","SA2_data.rds"))
