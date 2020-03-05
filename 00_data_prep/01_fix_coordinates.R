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


# writen by Bevan Ernst (Bevan.Ernst@gov.bc.ca)

# script for taking the excel sheet they sent,
# cleaning it up (there are cells with multiple co-ordinates in a single cell,
# ome with ‘ included in decimal degrees some with spaces, etc…),
# turning it into a spatial object and export to shapefile.
# If it is of use to you, you would just need to change the filepaths to work on your machine.




library(sf)
library(tidyverse)
library(readxl)



##bring in

UPDATE <- read_xlsx("H:/Caribou/Predator_Management/CWC_Updates/TweedsmuirDailyReport2020 Feb 6-March 1.xlsx",sheet = "Wolf Removal Data")%>% mutate(ORIG_ID=rownames(.))


###clean it up

UPDATE_CLEAN <- UPDATE %>%
  mutate(`LONGS (decimal degrees)`=if_else(condition = str_detect(`LONGS (decimal degrees)`, pattern = "126.18619\r\n126.1924\r\n126.187.89"), true = "126.18619\r\n126.1924\r\n126.18789", false = `LONGS (decimal degrees)`) ) %>% ##this entry so problematic coudl not do through logic
  mutate(`LONGS (decimal degrees)`=if_else(condition = str_detect(`LONGS (decimal degrees)`,"'") ,true = str_replace_all(`LONGS (decimal degrees)`, pattern = "\\.", replacement = ""), false = `LONGS (decimal degrees)`)) %>% ###gets rid of . in entries with' and .
  mutate(`LONGS (decimal degrees)`=if_else(condition = str_detect(`LONGS (decimal degrees)`,"'") ,true = str_replace_all(`LONGS (decimal degrees)`, "\\'", "\\."), false = `LONGS (decimal degrees)`)) %>% ###replace ' with .
  mutate(`LATS. (decimal degrees)`=if_else(condition = str_detect(`LATS. (decimal degrees)`,"'") ,true = str_replace_all(`LATS. (decimal degrees)`, pattern = "\\.", replacement = ""), false = `LATS. (decimal degrees)`)) %>% ###gets rid of . in entries with' and .
  mutate(`LATS. (decimal degrees)`=if_else(condition = str_detect(`LATS. (decimal degrees)`,"'") ,true = str_replace_all(`LATS. (decimal degrees)`, "\\'", "\\."), false = `LATS. (decimal degrees)`)) %>% ###replace ' with .
  mutate(`LONGS (decimal degrees)`=  str_replace_all(`LONGS (decimal degrees)`, pattern="\\. ", replacement = "\\.")) %>% ###remove spaces in middle of number
  mutate(`LATS. (decimal degrees)`=  str_replace_all(`LATS. (decimal degrees)`, pattern="\\. ", replacement = "\\.")) %>%###remove spaces in middle of number
  separate_rows(`LATS. (decimal degrees)`, `LONGS (decimal degrees)`, sep = "\r\n") %>% ###seperates mulitple values in cell
  mutate(`LATS. (decimal degrees)`=as.numeric(`LATS. (decimal degrees)`),`LONGS (decimal degrees)`=as.numeric(`LONGS (decimal degrees)`)*-1 ) %>% ###makes LAT and long numeric
  group_by(ORIG_ID) %>% mutate(COUNT=n()) %>% ungroup() %>% ##create coutn to divide wolf numbers between split rows
  mutate(`WOLVES REMOVED`=`WOLVES REMOVED`/COUNT, `WOLVES COLLARED`=`WOLVES COLLARED`/COUNT)##recalc wolf numbers



UPDATE_SF <- st_as_sf(UPDATE_CLEAN, coords = c("LONGS (decimal degrees)", "LATS. (decimal degrees)" ), crs=4326)


####check numbers agree, this shoudl equal zero, if not won't export witout specifially selecting only that row
{
  if(last(UPDATE_SF$`Year total`)- sum(UPDATE_SF$`WOLVES REMOVED`, na.rm = T) !=0) stop("Mismatch between totals. DID NOT EXPORT")


  ##export out###
  st_write(UPDATE_SF, paste0("W:/wlap/kam/Workarea/BErnst/Caribou/Predators/Pred_Man_2020/TWEED_UPDATE_", Sys.Date(),".shp"), delete_dsn = T)

}


