#######################################################################
#
#              Loading Packages
#
#######################################################################
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggmap)
library(here)
library(geosphere)

#######################################################################
#
#              Reading data
#
#######################################################################
#It creates the vector with all the files names
file.ls <- list.files(path='01 Raw Petrol Data/',pattern='\\.xlsx$')

#New list to store the information of each file
petrolprices<-list()

#The next for loop read each file and seach the first true row (not all files have the same)
for(i in 1:length(file.ls)){
        #Set the sheet to 1
        desired_sheet <- 1
        #Read all the file without skiping rows
        temp_read <- readxl::read_xlsx(paste("01 Raw Petrol Data/",file.ls[i],sep=""),sheet = desired_sheet)
        #Set skip row to NULL
        skip_rows <- NULL
        #String that I am searching in the first column
        search_string <- "ServiceStationName"
        #Number of rows where I am going to seach
        max_rows_to_search <- 10
        
        #THe while loop read the information in the first column until find search_string
        while (length(skip_rows) == 0){
                if(names(temp_read[1])==search_string){
                        skip_rows<-0
                        break
                }
                skip_rows <- which(stringr::str_detect(temp_read[1:max_rows_to_search,1][[1]],search_string)) - 0
        }
        
        #Read the file again skiping the rows with no information
        temp_read <- as_tibble(readxl::read_excel(
                paste("01 Raw Petrol Data/",file.ls[i],sep=""),
                sheet = desired_sheet,
                skip = skip_rows
        ))
        #Stores the values in the list
        petrolprices[[i]]<-temp_read
}

#Convert dates as character to date
petrolprices<-lapply(petrolprices, function(x){
        if(class(x$PriceUpdatedDate)=="character"){
                x$PriceUpdatedDate<-dmy_hms(x$PriceUpdatedDate)
        }
        else{
                x
                
        };return(x)
        
})

#Append all list elements into one dataframe
petrolprices<-do.call(rbind, lapply(petrolprices, as.data.frame))
petrolprices<-petrolprices %>% filter(!is.na(Price))

#######################################################################
#
#              Data Tyding
#
#######################################################################
#change na to the last observation
petrolprices_t<-petrolprices
petrolprices_t[,c(1,2,3,4,5)]<-na.locf(petrolprices[,c(1,2,3,4,5)])

#####################
# Geocoding address
#####################
stations<-petrolprices_t %>% select(ServiceStationName,Address,Suburb,Postcode,Brand) %>%
        distinct()

register_google(key = "GoogleAPIKeyHere")

#Getting coordinates from address and location name
coordinates<-ggmap::geocode(stations$Address, output = "more", source = "google")
coordinates2<-ggmap::geocode(stations$ServiceStationName, output = "more", source = "google")

#Checking address with incorrect information
id<-is.na(coordinates$address) | !str_detect(coordinates$address,"nsw")

#Creating final dataset with both coordinates
coordinates_final<-coordinates
coordinates_final[id,]<-coordinates2[id,]

#Combinating with address
stations_final<-cbind(stations,coordinates_final) %>%
        select(-type,-north,-south,-east,-west,-loctype)

#####################
# Manual Changes
#####################
#Loading changes
corrections<-read_xlsx(here("03 Data Tidying","Address_corrections.xlsx"))

stations_final<-stations_final %>%left_join(corrections, by=c("Address"))%>%
        mutate(address=ifelse(is.na(New_Address),address,New_Address)) %>%
        mutate(lat=ifelse(is.na(lat.y),lat.x,lat.y)) %>%
        mutate(lon=ifelse(is.na(lon.y),lon.x,lon.y)) %>%
        select(station_name=ServiceStationName, address=Address, suburb=Suburb,
               postcode=Postcode,brand=Brand,lon,lat) %>%
        mutate(station_name=tolower(station_name),
               address=tolower(address),
               suburb=tolower(suburb),
               brand=tolower(brand))

stations_final$ID<-seq.int(nrow(stations_final))

#######################################
# Too close stations (are the same?)
#######################################
#Creating distance matrix
distance_matrix<-distm(as.matrix(stations_final[,c("lon","lat")]), fun = distHaversine)

distance_matrix_2<-distance_matrix
distance_matrix_2<-data.frame(distance_matrix_2)
distance_matrix_2$ID_1<-seq.int(nrow(distance_matrix))

distance_matrix_2<-distance_matrix_2 %>%
        gather(key="ID_2",value="distance",-ID_1) %>%
        mutate(ID_2=parse_number(ID_2)) %>%
        filter(!ID_1==ID_2) %>%
        rowwise() %>%
        mutate(ID=paste(min(ID_1,ID_2),max(ID_1,ID_2),sep="_")) %>%
        distinct(ID, .keep_all=TRUE) 

#DF with all the duplicated stations
aux_matrix <- distance_matrix_2 %>% ungroup() %>%
        left_join(stations_final[,c("ID","brand")],by=c("ID_1"="ID")) %>%
        left_join(stations_final[,c("ID","brand")],by=c("ID_2"="ID")) %>%
        filter(brand.x==brand.y,distance<20)
#Changing ID of the same stations
stations_nodup<-stations_final %>% 
        left_join(aux_matrix[,c("ID_1","ID_2")],by=c("ID"="ID_1")) %>%
        mutate(ID=ifelse(is.na(ID_2),ID,ID_2)) %>%
        select(-ID_2) %>%
        select(ID,everything())

#Manual Changes
stations_nodup<-stations_nodup %>%
        mutate(ID=case_when(
                ID==2082~20,
                ID==2163~852,
                ID==2198~964,
                ID==1656~1654,
                ID==2402~1654,
                ID==2195~1810,
                TRUE ~ ID
        ))

#DF with too close stations with different brand
#Maybe they changed their names
aux_matrix2 <- distance_matrix_2 %>% ungroup() %>%
        left_join(stations_final[,c("ID","brand")],by=c("ID_1"="ID")) %>%
        left_join(stations_final[,c("ID","brand")],by=c("ID_2"="ID")) %>%
        filter(!brand.x==brand.y,distance<20)
#I am not going to change this stations

#Creating final tables to make the join
station_list<-stations_nodup %>%
        select(ID,brand,address)
        
coordinate_list<-stations_nodup %>%
        select(-brand,-address) %>% group_by(ID) %>%
        summarise_all(first)

#######################################################################
#
#              Joining Data
#
#######################################################################
mergeddata<-petrolprices_t %>%
        select(address=Address, brand=Brand, fuel=FuelCode,
               date=PriceUpdatedDate,price=Price) %>%
        mutate(address=tolower(address), brand=tolower(brand)) %>%
        left_join(station_list) %>% left_join(coordinate_list)

#E10 is the one with more data so I am going to use it
table(mergeddata$fuel)

saveRDS(mergeddata,here("04 Tidy Data","Geocoded_petrol_data.rds"))

saveRDS(stations_nodup,here("04 Tidy Data","Stations_list_all.rds"))
