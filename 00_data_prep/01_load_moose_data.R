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


mdata.13 <- import_sheets("2013-14", 2013)
mdata.14 <- import_sheets("2014-15", 2014)
mdata.15 <- import_sheets("2015-16", 2015)
mdata.16 <- import_sheets("2016-17", 2016)
mdata.17 <- import_sheets("2017-18", 2018)


moose <- bind_rows(mdata.13, mdata.14, mdata.15, mdata.16, mdata.17)

# check the distribution of points
ggplot(moose, aes(Y,X)) +
  geom_point()

ggplot(moose, aes(Y,X)) +
  geom_point() +
  facet_wrap(~year)

# remove outliers # largely in 2018.
moose <- moose %>%
  filter(X < 53.75)


# Create a SpatialPointsDataFrame by defining the coordinates
moose.sp <- moose[, c("X", "Y", "year")]
coordinates(moose.sp) <- c("Y", "X")
proj4string(moose.sp) <- CRS("+proj=longlat +datum=WGS84 +units=m +no_defs" )

moose.sp <- spTransform(moose.sp, CRS("+init=epsg:3005")) # Transform to UTM

# check moose distribution
#mapview::mapview(mgeo)

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

