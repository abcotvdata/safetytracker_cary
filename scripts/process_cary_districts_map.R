library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)

# GEOGRAPHY
# Get Raleigh police districts/beats
download.file("https://maps.townofcary.org/arcgis/rest/services/PublicSafety/LawOperations/MapServer/2/query?where=0%3D0&text=&objectIds=&time=&timeRelation=esriTimeRelationOverlaps&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&sqlFormat=none&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson",
              "data/source/geo/cary_police_beats.geojson")

# Read in geojson and then transform to sf format
districts_geo <- st_read("data/source/geo/cary_police_beats.geojson") %>% st_transform(3857)

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "NC",
                        county = c("Wake","Chatham"),
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

# rename some columns
districts_geo$district_cpd <- districts_geo$district
districts_geo$district <- districts_geo$beat_num
districts_geo$beat_num <- NULL

# Quick define of the areas 
districts_geo$placename <- case_when(districts_geo$district == "111"~ "Harrison Park, Northwoods and Weston Parkway",
                                     districts_geo$district == "112"~ "Southeast Cary, South Hills Shopping Center and Aurella",
                                     districts_geo$district == "113"~ "Central Cary, Town Hall Campus and Harrison Pointe Shopping Center",
                                     districts_geo$district == "114"~ "Maynard Loop, Maynard Crossing and Weatherfield Industrial Park",
                                     districts_geo$district == "115"~ "Central Cary, Ashley Village, Scottish Hills and MacGregor Downs",
                                     districts_geo$district == "116"~ "Regency Park, Waverly Place, MacGregor Park and Crescent Commons",
                                     districts_geo$district == "117"~ "Bond Park, Park Village, Brookstone and Parkway Pointe",
                                     districts_geo$district == "118"~ "Southeast Cary, Walnut Plaza and Crossroads",
                                     districts_geo$district == "119"~ "Carolina Preserve, Cary Park and Panther Creek HS",
                                     districts_geo$district == "120"~ "Cary Towne Center, Cary Village Square and Chatham Square",
                                     districts_geo$district == "121"~ "Carpenter Village, Preston and Green Hope HS",
                                     districts_geo$district == "122"~ "Cary Park Town Center, Southwest Cary and Cary Park",
                                     TRUE ~ "Unknown")

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/cary_districts.geojson")
st_write(districts_geo,"data/output/geo/cary_districts.geojson")
saveRDS(districts_geo,"scripts/rds/cary_districts.rds")

# BEAT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,5000,8000,10000,15000,50000, Inf)
poppal <- colorBin("YlOrRd", districts_geo$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts_geo$district,districts_geo$placename,prettyNum(districts_geo$population, big.mark = ","))

cary_districts_map <- leaflet(districts_geo) %>%
  setView(-78.815, 35.7915, zoom = 11.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "Stamen.TonerLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
cary_districts_map





