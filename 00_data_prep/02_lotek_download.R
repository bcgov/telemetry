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

## CURRENTLY NOT FUNCTIONING

# The Lotek one downloads directly form their website, CURRENTLY NOT FUNCTIONING
# The lotek one has recently started to not function, it has started to fall
# down on opening the website to download.  It doesn’t seem to be taking the
# argument of the driver version to use, which has led to it throwing an error
# about mismatch between browser and driver version.
# I haven’t been able to determine if the issue is in the Collarscraper package,
# the underlying selenium driver it is using, or quirks of drivers and browsers on managed gov machines.




####only need to run the first time, coudl take a bit inlcuding the dependencies#####
install.packages("tidyverse", dependencies = T)
install.packages("devtools", dependencies = T)
devtools::install_github("rgzn/CollarScraper", dependencies = T)
install.packages("janitor", dependencies = T)
install.packages("sf", dependencies = T)


#####run this section everytime###
library(tidyverse)
library(CollarScraper)
library(janitor)
library(sf)
library(ggmap)
library(bcdata)


##set user name password and working directory should be only chang in input needed, run a all remaingin code , note working directory needs \ replaced with /

username="USERNAME"
password="PASSWORD"
setwd("W:/wlap/kam/Workarea/BErnst/Caribou/Collars/NT_COLLAR_DOWNLOADS")


# Create scraper:
myLotek = lotekScraper$new(username = username, password = password, headless = FALSE, driver_version=last (binman::list_versions( "chromedriver")$win32))

# Start browser:
myLotek$start()

# Submit login data:
myLotek$login()

# Get a data frame with available collars added filter to avoid blank collars:
collarsDF = myLotek$get_collar_df() %>% filter(GPS>5)

# Set a start and end date for requested data right now set to 4 years back watch out for time messing with import as requesting outside of existing dates give null file, which screws up later steps:
end = Sys.Date()
begin = end - 1460

# download a single collar:
collar2 = collarsDF[2,]
myFilename = myLotek$dl_collar(collar_id = collar2$ID,
                               start_date = begin,
                               end_date = end,
                               format = "Spread Sheet (CSV)")

# download each collar and return a list of downloaded files
#    use default arguments for dl_collar method
downloadedFiles = lapply(collarsDF$ID, function(x) myLotek$dl_collar(x, format = "Spread Sheet (CSV)"))


###wait a couple seconds for browser to finish

Sys.sleep(7)


# stop browser:
myLotek$close()




####make data frames####

COLLARS_LIST <- map(downloadedFiles, read_csv)




###combine list of data frames to single convert serial data time to date and time

COMBINED_COLLARS<- COLLARS_LIST %>% reduce(bind_rows) %>% mutate(LOCAL_DATE_TIME=excel_numeric_to_date(`Date & Time [Local]`,include_time = T)) %>% mutate( Time=format(LOCAL_DATE_TIME, '%H:%M:%S'))



write_csv(COMBINED_COLLARS, paste0("Combined_Collar_Data", format(Sys.time(),'_%Y_%m_%d'),".csv"))



####turn into an SF spatial object
COMBINED_COLLARS_SF <- st_as_sf(COMBINED_COLLARS%>% filter(!is.na(Latitude)), coords=c( "Longitude","Latitude"), crs=4326, remove = F)  %>% mutate(Year=format(LOCAL_DATE_TIME, "%Y"), Month= format(LOCAL_DATE_TIME, "%b"), Month_Year=format(LOCAL_DATE_TIME, "%b-%Y"), Day=format(LOCAL_DATE_TIME, "%d") )



bcdc_search("Snowmobile")




plot(COMBINED_COLLARS_SF[2])

#export to shapefile with date###
st_write(COMBINED_COLLARS_SF, paste0("Combined_Collar_Data", format(Sys.time(),'_%Y_%m_%d'),".shp") ,  driver = "ESRI Shapefile", delete_dsn = T)



####recent#####
Sys.Date()-7

LAST_WEEK_COLLARS <-COMBINED_COLLARS_SF %>% filter(LOCAL_DATE_TIME>=Sys.Date()-7)

LAST_MONTH_COLLARS <-COMBINED_COLLARS_SF %>% filter(LOCAL_DATE_TIME>=Sys.Date()-30)


st_write(LAST_WEEK_COLLARS, paste0("Last_Week_Collar_Data", format(Sys.time(),'_%Y_%m_%d'),".shp") ,  driver = "ESRI Shapefile", delete_dsn = T)



st_write(LAST_MONTH_COLLARS, paste0("Last_Month_Collar_Data", format(Sys.time(),'_%Y_%m_%d'),".shp") ,  driver = "ESRI Shapefile", delete_dsn = T)


LAST_WEEK_COLLARS$

####map###


SNOWMO<- bcdc_get_data("e6e31638-afe9-4355-aa4d-9853e53de55c")


GH <- SNOWMO %>% filter(str_detect(.$SNOWMOBILE_AREA_NAME, "Groundhog"), AREA_MANAGEMENT_CLASS =="Open under SMA" )

GH_BUFF <-  GH %>% st_buffer(dist = 5000)


GH_MAP <- get_map(location = unname(st_bbox(st_transform( GH_BUFF, 4326))), zoom = 11)

ggmap(GH_MAP, )+geom_sf( data=st_transform( GH, 4326), inherit.aes = F, fill=NA, aes(colour="Groundhog Riding Area"))+geom_sf(data = st_transform(LAST_WEEK_COLLARS, 4326), inherit.aes = F, aes(colour=factor(`Device ID`)))

