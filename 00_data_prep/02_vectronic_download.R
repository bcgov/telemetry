# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# Description: Script to download Lotek data directly from the Website
# Author : Bevan Ernst (Bevan.Ernst@gov.bc.ca)


# Vectronic script to download collar data
# one you need to download all the files as csv to a common folder and run it on that.



library(sf)
library(tidyverse)




file_location="W:/wlap/kam/Workarea/BErnst/Caribou/Collars/Revy_Collars/Jan_10"

OUTPUT_FILE_ALL="W:/wlap/kam/Workarea/BErnst/Caribou/Collars/Revy_Collars/Jan_10/ALL_COLLARS.shp"

OUTPUT_FILE_RECENT="W:/wlap/kam/Workarea/BErnst/Caribou/Collars/Revy_Collars/Jan_10/ALL_COLLARS_RECENT.shp"


START_DATE = Sys.Date()-7
END_DATE = Sys.Date()


####bring in all files####



FILE_LIST <- list.files(file_location, pattern =c( "GPS") , full.names = T)

###combine all files into a single data set####

ALL_COLLARS <- FILE_LIST %>% map(read_csv, ) %>% reduce(bind_rows) %>% rename_all(make.names) %>% rename(Longitude=Longitude...U.00B0.., Latitude= Latitude...U.00B0..) %>% select(-(starts_with("Sat_")),-(starts_with("C.N_")) )
str(ALL_COLLARS)


####make it all spatial###
ALL_COLLARS_SF <- st_as_sf(ALL_COLLARS %>% filter(!is.na(Longitude)) , coords= c("Longitude", "Latitude"), crs=4326, remove = F) %>% mutate(Year=format(LMT_Date, "%Y"), Month= format(LMT_Date, "%b"), Month_Year=format(LMT_Date, "%b-%Y"), Day=format(LMT_Date, "%d") ) %>% MAKE_ALBERS()



####subset to date range####

RECENT_COLLARS_SF <- ALL_COLLARS_SF %>% filter(LMT_Date>START_DATE ,LMT_Date<END_DATE )




####write output####
st_write(ALL_COLLARS_SF, OUTPUT_FILE_ALL,delete_dsn = T )



st_write(RECENT_COLLARS_SF, OUTPUT_FILE_RECENT, delete_dsn = T)

#####same thing but convert to albers, useful for doing geoprocessing with gov layers in R.  Note that although the spztial refernce right it comes up as different name than bc albers####




MAKE_ALBERS <- function(SF_OBJECT){st_transform(x = SF_OBJECT,crs= "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" )%>% `st_crs<-`(3005)}

MAKE_ALBERS()


FILE_LIST <- list.files(file_location, pattern =c( "GPS") , full.names = T)


ALL_COLLARS <- FILE_LIST %>% map(read_csv, ) %>% reduce(bind_rows) %>% rename_all(make.names) %>% rename(Longitude=Longitude...U.00B0.., Latitude= Latitude...U.00B0..) %>% select(-(starts_with("Sat_")),-(starts_with("C.N_")) )
str(ALL_COLLARS)

ALL_COLLARS_SF <- st_as_sf(ALL_COLLARS %>% filter(!is.na(Longitude)) , coords= c("Longitude", "Latitude"), crs=4326, remove = F) %>% mutate(Year=format(LMT_Date, "%Y"), Month= format(LMT_Date, "%b"), Month_Year=format(LMT_Date, "%b-%Y"), Day=format(LMT_Date, "%d") ) %>% MAKE_ALBERS()






