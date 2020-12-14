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
        
##set user name password and working directory should be only chang in input needed, run a all remaingin code , note working directory needs \ replaced with /

username="Kamloops"
password="Tarandus"
setwd("W:/wlap/kam/Workarea/BErnst/Caribou/Collars/NT_COLLAR_DOWNLOADS")


# Create scraper:
myLotek = lotekScraper$new(username = username, password = password, headless = FALSE, driver_version=first (binman::list_versions( "chromedriver")$win32))

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
COMBINED_COLLARS_SF <- st_as_sf(COMBINED_COLLARS%>% filter(!is.na(Latitude)), coords=c( "Longitude","Latitude"), crs=4326, remove = F)

plot(COMBINED_COLLARS_SF[2])

#export to shapefile with date###
st_write(COMBINED_COLLARS_SF, paste0("Combined_Collar_Data", format(Sys.time(),'_%Y_%m_%d'),".shp") ,  driver = "ESRI Shapefile")




