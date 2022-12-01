library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)

# GEOGRAPHY
# Get Raleigh police districts/beats
download.file("https://services.arcgis.com/v400IkDOw1ad7Yad/arcgis/rest/services/Raleigh_Police_Districts/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
              "data/source/geo/raleigh_police_districts.geojson")

# Read in geojson and then transform to sf format
districts_geo <- st_read("data/source/geo/raleigh_police_districts.geojson") %>% st_transform(3857)

districts_geo <- districts_geo %>% 
  group_by(DISTRICT) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "NC",
                        county = c("Wake"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Calculate the estimated population of police BEATS geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
districts_withpop <- st_interpolate_aw(blocks, districts_geo, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
districts_withpop <- st_drop_geometry(districts_withpop)
# Binds that new population column to the table
districts_geo <- cbind(districts_geo,districts_withpop)
# Cleans up unneeded calculation file
# rm(districts_withpop, blocks)

# Check total population assigned/estimated across all precincts
sum(districts_geo$population) # tally is 453321 

# Round the population figure; rounded to nearest thousand
districts_geo$population <- round(districts_geo$population,-3)
# Prep for tracker use
districts_geo <- districts_geo %>% st_transform(4326)
districts_geo <- st_make_valid(districts_geo) %>% janitor::clean_names()

# Quick define of the areas 
districts_geo$placename <- case_when(districts_geo$district == "DTD"~ "Downtown Raleigh",
                                     districts_geo$district == "NED"~ "Northeast Raleigh",
                                     districts_geo$district == "NOD"~ "North Raleigh",
                                     districts_geo$district == "NWD"~ "Northwest Raleigh",
                                     districts_geo$district == "SED"~ "Southeast Raleigh",
                                     districts_geo$district == "SWD"~ "Southwest Raleigh",
                                     TRUE ~ "Unknown")

# Quick define of the areas 
districts_geo$district_name <- case_when(districts_geo$district == "DTD"~ "Downtown",
                                     districts_geo$district == "NED"~ "Northeast",
                                     districts_geo$district == "NOD"~ "North",
                                     districts_geo$district == "NWD"~ "Northwest",
                                     districts_geo$district == "SED"~ "Southeast",
                                     districts_geo$district == "SWD"~ "Southwest",
                                     TRUE ~ "Unknown")

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/raleigh_districts.geojson")
st_write(districts_geo,"data/output/geo/raleigh_districts.geojson")
saveRDS(districts_geo,"scripts/rds/raleigh_districts.rds")

# BEAT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,20000,40000,50000,60000,100000, Inf)
poppal <- colorBin("YlOrRd", districts_geo$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts_geo$district_name,districts_geo$placename,prettyNum(districts_geo$population, big.mark = ","))

raleigh_districts_map <- leaflet(districts_geo) %>%
  setView(-78.65, 35.79, zoom = 11.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
raleigh_districts_map