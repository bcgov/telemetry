# Copyright 2019 Province of British Columbia
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


# script to run KDE for caribou data (written by Gen Perkins) April 16th 2010.

# used both href kde and and lscv as they provide very different results


library(lubridate)
library(dplyr)
library(readxl)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(stringr)

data_path <- file.path("data")


# import all sheets into single file with name of year
indata <- read_xlsx(file.path(data_path, "KlinseZaAll_200320.xlsx"),
                    sheet = 2) %>%
                      rename(x = easting, y = northing )


# format date

counts.per.year = indata %>%
  group_by(yrbiol) %>%
  summarise(count = n())

# format date and filter for last 5 years and add season
#
# early winter : nov 1 - jan 14th
# late winter : Jan 15 - march 31
# spring : April 1 - May 14


indata <- indata %>%
  mutate(day = as.numeric(str_sub(date_time, 1,2)),
        month = str_sub(date_time, 3, 5)) %>%
  filter(yrbiol > 2014) %>%
  mutate(season = case_when(
            month == "APR" ~ "spring",
            month == "MAY" & day <15 ~ "spring",
            month %in% c("FEB", "MAR") ~ "late winter",
            month == "JAN" & day > 14 ~ "late winter",
            month %in% c("NOV", "DEC") ~ "early winter",
            month == "JAN" & day < 15 ~ "early winter"))

# filter seasons of interest

indata <- indata %>%
  filter(! is.na(season))


# filter animal 1d with > 50 counts

animal.ids <- indata %>%
  group_by(animal_id) %>%
  summarise(count = n()) %>%
  filter(count > 50) %>%
  dplyr:: select(animal_id) %>%
  pull()


indata <- indata %>%
  filter(animal_id %in% animal.ids) %>%
  mutate(animal_id = as.factor(animal_id))


# kernal density estimates

seasons = as.list(unique(indata$season))


for (s in seasons) {

  tdata <- indata %>%
    filter(season == s) %>%
    droplevels() %>%
    dplyr::select(x,y, animal_id) %>%
    distinct()

  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdata) <- c("x", "y")
  proj4string(tdata) <- CRS( "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" )
  tdfgeo <- spTransform(tdata, CRS("+init=epsg:3005")) # Transform to UTM


  # run KDE using href as the
  kde  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

  ver95 <- getverticeshr( kde, 95)
  ver95.sf <- st_as_sf( ver95 )

  st_write(ver95.sf, file.path ("out", paste0("KlinseZa_KDE95_",s, "_href.shp")))


  # run KDE using href as the
  kde_lscv  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

  ver95_lscv<- getverticeshr( kde_lscv, 95)
  ver95_lscv.sf <- st_as_sf( ver95_lscv)

  st_write(ver95_lscv.sf , file.path ("out", paste0("KlinseZa_KDE95_",s, "_lsvc.shp")))


}


# FOr the othe herd that are all the same format


files <- list.files(data_path, pattern = "20191231.xlsx$")

for( f in files){

  #f = files[1]
  fname = gsub("_20191231.xlsx", "", f)

# import all sheets into single file with name of year
indata <- read_xlsx(file.path(data_path, f)) %>%
  rename(x = AlbersX, y = AlbersY) %>%
  dplyr::select(Animal_ID, x, y, Year,	Month,	Day) %>%
  rename_all(.funs = tolower) %>%
  dplyr::filter(year > max(indata$year)-5) %>%
  mutate(season = case_when(
    month == 4 ~ "spring",
    month == 5 & day <15 ~ "spring",
    month %in% c(2, 3) ~ "late winter",
    month == 1 & day > 14 ~ "late winter",
    month %in% c(11, 12) ~ "early winter",
    month == 1 & day < 15 ~ "early winter")) %>%
  filter(!is.na(season))


# filter animal 1d with > 50 counts

animal.ids <- indata %>%
  group_by(animal_id) %>%
  summarise(count = n()) %>%
  filter(count > 50) %>%
  dplyr:: select(animal_id) %>%
  pull()


indata <- indata %>%
  filter(animal_id %in% animal.ids) %>%
  mutate(animal_id = as.factor(animal_id))


# kernal density estimates

seasons = as.list(unique(indata$season))


for (s in seasons) {

 # s = seasons[[1]]

  tdata <- indata %>%
    filter(season == s) %>%
    droplevels() %>%
    dplyr::select(x,y, animal_id) %>%
    distinct()

  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(tdata) <- c("x", "y")
  proj4string(tdata) <- CRS( "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs " )
  tdfgeo <- spTransform(tdata, CRS("+init=epsg:3005")) # Transform to UTM


  # run KDE using href as the
  kde  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

  ver95 <- getverticeshr( kde, 95)
  ver95.sf <- st_as_sf( ver95 )

  st_write(ver95.sf, file.path ("out", paste0(fname, "_KDE95_",s, "_href.shp")), overwrite = TRUE)


  # run KDE using href as the
  kde_lscv  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

  ver95_lscv<- getverticeshr( kde_lscv, 95)
  ver95_lscv.sf <- st_as_sf( ver95_lscv)

  st_write(ver95_lscv.sf , file.path ("out", paste0( fname, "_KDE95_",s, "_lsvc.shp")), overwrite = TRUE)


 }

}


