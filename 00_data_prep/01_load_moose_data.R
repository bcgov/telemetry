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


library(dplyr)
library(readxl)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)

data_path <- file.path("data")


# import all sheets into single file with name of year

import_sheets <- function(sheetname, year){
  mdata.0 <- read_xlsx(file.path(data_path, "Entiako_For_MCPs.xlsx"),
                       sheet = sheetname) %>%
              mutate(year = year)
  }


mdata.13 <- import_sheets("2013-14", 201314)
mdata.14 <- import_sheets("2014-15", 201415)
mdata.15 <- import_sheets("2015-16", 201516)
mdata.16 <- import_sheets("2016-17", 201617)
mdata.17 <- import_sheets("2017-18", 201718)


moose <- bind_rows(mdata.13, mdata.14, mdata.15, mdata.16, mdata.17)

# check the distribution of points
ggplot(moose, aes(Y,X)) +
  geom_point()

ggplot(moose, aes(Y,X)) +
  geom_point() +
  facet_wrap(~year)

# remove outliers # largely in 2018. # some change in notes
moose <- moose %>%
  filter(X < 53.75)


# Create a SpatialPointsDataFrame by defining the coordinates
moose.sp <- moose[, c("X", "Y", "year")]
coordinates(moose.sp) <- c("Y", "X")
proj4string(moose.sp) <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs" )

moose.sp <- spTransform(moose.sp, CRS("+init=epsg:3005")) # Transform to UTM

# check moose distribution
#mapview::mapview(moose.sp)

# Calculate MCPs for each year
moose.mcp <- mcp(moose.sp, percent = 100)
moose.mcp.95 <- mcp(moose.sp, percent = 95)

# Plot
plot(moose.sp, col = as.factor(moose.sp$year), pch = 16)
plot(moose.mcp, col = alpha(1:5, 0.5), add = TRUE)
plot(moose.mcp.95, col = alpha(1:5, 0.5), add = TRUE)

# convert to sf object
library(sf)
moose.mcp <- st_as_sf(moose.mcp)
moose.mcp.95 <- st_as_sf(moose.mcp.95)

# write out to shapefile

st_write(moose.mcp, file.path("out", "mmcp100.shp"))
st_write(moose.mcp.95, file.path("out", "mmcp95.shp"))


plot(moose.mcp)


# Part 2:  Moose Data Check 2020 --------------------------------------------------

library(dplyr)
library(readxl)
library(adehabitatHR)
library(sp)
library(ggplot2)
library(sf)
library(plotKML)

data_path <- file.path("data")

# import data
  mdata.0 <- read_xlsx(file.path(data_path, "Copy of 2018 - 2019 Compiled Data.xlsx"),
                      sheet = "Cleaned")

  moose <- rename(mdata.0, X = `Latitude [°]`, Y =`Longitude [°]` ) %>%
    mutate(id = case_when(
      CollarID == 14446 ~ "ENT131401",
      CollarID == 17770 ~ "ENT131402",
      CollarID == 19795 ~ "ENT131403",
      CollarID == 19822 ~ "ENT131404",
      CollarID == 20211 ~ "ENT131405",
      CollarID == 20219 ~ "ENT131406",
      CollarID == 24445 ~ "ENT131407",
      CollarID == 24447 ~ "ENT131408",
      CollarID == 24448 ~ "ENT131409",
      CollarID == 29374 ~ "ENT131410",
    ))

  # check the distribution of points
  ggplot(moose, aes(Y, X)) +
    geom_point()

  ggplot(moose, aes(Y,X)) +
    geom_point() +
    facet_wrap(~CollarID)


  # Create a SpatialPointsDataFrame by defining the coordinates
  moose.sp <- moose[, c("X", "Y", "CollarID")]
  coordinates(moose.sp) <- c("Y", "X")
  proj4string(moose.sp) <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs" )
  mapview::mapview(moose.sf)


  moose.sf <- spTransform(moose.sp, CRS("+init=epsg:3005")) # Transform to UTM
  mapview::mapview(moose.sf)

#  output a kml for easy investigation
  plotKML::kml(moose.sp,
               file.name    = "moose2020.kml",
               points_names = moose$CollarID,
               colour    = "#FF0000",
               alpha     = 0.6,
               size      = 1,
               shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")



  write.csv(moose, file.path("data", "moose_2020.csv"))



# Part 3 Date format  ------------------------------------------------------------

  # Run this part of the script to fix date format, add day, month, year columns and consolidate the files into a single
  # xlsx file which is written out

  library(dplyr)
  library(readxl)
  library(ggplot2)
  library(lubridate)
  library(stringr)

  data_path <- file.path("data")

  data_path <- "I:/ES/General/Wildlife/WILDLIFE SPECIES/Moose/Telemetry/PMU Tweedsmuir/Entiako-Tweedsmuir Study Area/Collars/Data Request compilation/2014-2015/Date correction/"

  gps.files <- list.files(file.path(data_path))
  gps.files <- as.list(gps.files[str_detect(gps.files, "GPS_Collar")] )

  mdata.out <- lapply(gps.files, function(x){

  # import date
  mdata.0 <- read.csv(file.path(data_path, x))

  # convert date to dmy
  mdata.0 %>%
    mutate(date =  as.POSIXct(LMT_Date, format = "%m/%d/%Y"),
           year = year(date),
           month = month(date),
           day = day(date))
  })


  # Part 4:  Moose summarised 2014-2014 --------------------------------------------------
#checking locations to remove extraneous fixes,
# replacing the collar serial numbers with the pseudonym

library(dplyr)
library(readxl)
library(ggplot2)
library(sf)
library(sp)
library (mapview)

data_path <- file.path("data")

mdata.0 <- read_xlsx(file.path(data_path, "2014 - 2015 Compiled Data.xlsx"),
                         sheet = "2014-15 Web+Collar Compiled")


moose <- rename(mdata.0, X = `Latitude [°]`, Y =`Longitude [°]` )


id_key <- tribble(~ CollarID, ~ id,
                 14220, "ENT181901",
                 14326, "ENT181902",
                 14434, "ENT181903",
                 14435, "ENT181904",
                 14440, "ENT181905",
                 14441, "ENT181906",
                 14444, "ENT181907",
                 14446, "ENT181908",
                 14449, "ENT181909",
                 14772, "ENT181910"
        )

moose <- moose %>%
  left_join(id_key)

# check the distribution of points
ggplot(moose, aes(Y, X)) +
  geom_point()

ggplot(moose, aes(Y,X)) +
  geom_point() +
  facet_wrap(~CollarID)


write.csv(moose, file.path("data", "moose_1314.csv"))


# Create a SpatialPointsDataFrame by defining the coordinates
#moose.sp <- moose[, c("X", "Y", "id")]
#coordinates(moose.sp) <- c("Y", "X")
#proj4string(moose.sp) <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs" )


moose.sf <- st_as_sf(moose, coords = c("X", "Y"), crs = 4326)
moose.sf <- spTransform(moose.sp, CRS("+init=epsg:3005")) # Transform to UTM
mapview::mapview(moose.sf)



#  output a kml for easy investigation
plotKML::kml(moose.sp,
             file.name    = "moose2020.kml",
             points_names = moose$CollarID,
             colour    = "#FF0000",
             alpha     = 0.6,
             size      = 1,
             shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")



write.csv(moose, file.path("data", "moose_2020.csv"))

