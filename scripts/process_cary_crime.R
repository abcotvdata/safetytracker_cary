library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

# starting to lay out crime data sourcing
# https://data.townofcary.org/explore/dataset/cpd-incidents/download/?format=csv&timezone=America/New_York&lang=en&use_labels_for_header=true&csv_separator=%2C

download.file("https://data.townofcary.org/explore/dataset/cpd-incidents/download/?format=csv&timezone=America/New_York&lang=en&use_labels_for_header=true&csv_separator=%2C",
              "data/source/cary_crime.csv")

cary_crime <- read_csv("data/source/cary_crime.csv") %>% janitor::clean_names()

cary_crime <- cary_crime %>% rename("category"="crime_category")
cary_crime$category <- str_to_title(cary_crime$category)
  
# Rebuild date fields in formats we need
cary_crime$date <- ymd(cary_crime$begin_date_of_occurrence)
cary_crime$hour <- substr(cary_crime$begin_time_of_occurrence,1,2)
cary_crime$month <- lubridate::floor_date(as.Date(cary_crime$date),"month")

cary_crime$type <- case_when(cary_crime$category %in% 
                                  c("Murder","Aggravated Assault","Sexual Assault","Robbery") ~ "Violent",
                                cary_crime$category %in% 
                                  c("Motor Vehicle Theft","Burglary","Larceny") ~ "Property",
                                TRUE ~ "Other/Unknown")
# select and rename columns
# cary_crime <- cary_crime %>% select(1,3,4,6:9,16:24)
cary_crime <- cary_crime %>% rename("description"="crime_type")
# filter date down to match Raleigh, 2015 and later
cary_crime <- cary_crime %>% filter(date>"2014-12-31")

# rename some columns
cary_crime$district_cpd <- cary_crime$district
cary_crime$district <- as.character(cary_crime$beat_number)
cary_crime$beat_number <- NULL

# create separate table of crimes from the last full 12 months
cary_crime_last12 <- cary_crime %>% filter(cary_crime$date > max(cary_crime$date)-365)

### CITYWIDE CRIME 
### TOTALS AND OUTPUT

# Set variable of city population
# likely needs added to the tracker itself
cary_population <- 176987

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- cary_crime %>%
  group_by(category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_detailed_last12 <- cary_crime_last12 %>%
  group_by(category,description) %>%
  summarise(last12mos = n())
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("category","description"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
rm(citywide_detailed_last12)
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total19+citywide_detailed$total20+citywide_detailed$total21
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to21 <- round(citywide_detailed$total21/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_21tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total21*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/cary_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/cary_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/cary_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/cary_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/cary_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- cary_crime %>%
  group_by(category,description,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
#citywide_detailed_monthly <- citywide_detailed_monthly %>%
#  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
#citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")


# Calculate of each category of offense CITYWIDE
citywide_category <- cary_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_category_last12 <- cary_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total19+citywide_category$total20+citywide_category$total21
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to21 <- round(citywide_category$total21/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_21tolast12 <- round(citywide_category$last12mos/citywide_category$total21*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/cary_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/cary_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/cary_population*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/cary_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/cary_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- cary_crime %>%
  group_by(category,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
#citywide_category_monthly <- citywide_category_monthly %>%
#  arrange(category,month) %>%
#  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
#citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)

# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
# citywide_category_monthly %>% filter(category=="Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Motor Vehicle Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Larceny") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category=="Aggravated Assault") %>% write_csv("data/output/monthly/assaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Murder") %>% write_csv("data/output/monthly/murders_monthly.csv")


### Some YEARLY tables for charts for our pages
citywide_category %>% select(1:8,10) %>% filter(category=="Murder") %>% write_csv("data/output/yearly/murders_city.csv")
# citywide_category %>% select(1,3:9,11) %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_category %>% select(1:8,10) %>% filter(category=="Motor Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_category %>% select(1:8,10) %>% filter(category=="Larceny") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_category %>% select(1:8,10) %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_category %>% select(1:8,10) %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_category %>% select(1:8,10) %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")


# Calculate of each type of crime CITYWIDE
citywide_type <- cary_crime %>%
  group_by(type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_type_last12 <- cary_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = n())
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total19+citywide_type$total20+citywide_type$total21
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to21 <- round(citywide_type$total21/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_21tolast12 <- round(citywide_type$last12mos/citywide_type$total21*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/cary_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/cary_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/cary_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/cary_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/cary_population*100000,1)

### RALEIGH PD DISTRICT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
districts <- st_read("data/output/geo/cary_districts.geojson")

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_district_description <- crossing(district = unique(cary_crime$district), description = unique(cary_crime$description)) %>% filter(!is.na(district))
list_district_category <- crossing(district = unique(cary_crime$district), category = unique(cary_crime$category)) %>% filter(!is.na(district))
list_district_type <- crossing(district = unique(cary_crime$district), type = unique(cary_crime$type)) %>% filter(!is.na(district))

# Calculate total of each detailed offense type by community area
district_detailed <- cary_crime %>%
  filter(!is.na(district)) %>%
  group_by(district,category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
district_detailed <- district_detailed %>% 
  rename("total15" = "2015",
         "total16" = "2016",
         "total17" = "2017",
         "total18" = "2018",
         "total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
district_detailed_last12 <- cary_crime_last12 %>%
  group_by(district,category,description) %>%
  summarise(last12mos = n())
district_detailed <- left_join(district_detailed,district_detailed_last12,by=c("district","category","description"))
rm(district_detailed_last12)
# add zeros where there were no crimes tallied that year
district_detailed[is.na(district_detailed)] <- 0
# Calculate a total across the 3 prior years
district_detailed$total_prior3years <- district_detailed$total19+district_detailed$total20+district_detailed$total21
district_detailed$avg_prior3years <- round(district_detailed$total_prior3years/3,1)
# calculate increases
district_detailed$inc_19to21 <- round(district_detailed$total21/district_detailed$total19*100-100,1)
district_detailed$inc_19tolast12 <- round(district_detailed$last12mos/district_detailed$total19*100-100,1)
district_detailed$inc_21tolast12 <- round(district_detailed$last12mos/district_detailed$total21*100-100,1)
district_detailed$inc_prior3yearavgtolast12 <- round((district_detailed$last12mos/district_detailed$avg_prior3years)*100-100,0)
# add population for beats
district_detailed <- full_join(districts,district_detailed,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_detailed$rate19 <- round(district_detailed$total19/district_detailed$population*100000,1)
district_detailed$rate20 <- round(district_detailed$total20/district_detailed$population*100000,1)
district_detailed$rate21 <- round(district_detailed$total21/district_detailed$population*100000,1)
district_detailed$rate_last12 <- round(district_detailed$last12mos/district_detailed$population*100000,1)
# calculate a multiyear rate
district_detailed$rate_prior3years <- round(district_detailed$avg_prior3years/district_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_detailed <- district_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_detailed <- district_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each category of offense BY POLICE BEAT
district_category <- cary_crime %>%
  filter(!is.na(district)) %>%
  group_by(district,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
district_category <- left_join(list_district_category,district_category,by=c("district"="district","category"="category"))
# rename the year columns
district_category <- district_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
district_category_last12 <- cary_crime_last12 %>%
  group_by(district,category) %>%
  summarise(last12mos = n())
district_category <- left_join(district_category,district_category_last12,by=c("district","category"))
rm(district_category_last12)
# add zeros where there were no crimes tallied that year
district_category[is.na(district_category)] <- 0
# Calculate a total across the 3 prior years
district_category$total_prior3years <- district_category$total19+district_category$total20+district_category$total21
district_category$avg_prior3years <- round(district_category$total_prior3years/3,1)
# calculate increases
district_category$inc_19to21 <- round(district_category$total21/district_category$total19*100-100,1)
district_category$inc_19tolast12 <- round(district_category$last12mos/district_category$total19*100-100,1)
district_category$inc_21tolast12 <- round(district_category$last12mos/district_category$total21*100-100,1)
district_category$inc_prior3yearavgtolast12 <- round((district_category$last12mos/district_category$avg_prior3years)*100-100,0)
# add population for beats
district_category <- full_join(districts,district_category,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_category$rate19 <- round(district_category$total19/district_category$population*100000,1)
district_category$rate20 <- round(district_category$total20/district_category$population*100000,1)
district_category$rate21 <- round(district_category$total21/district_category$population*100000,1)
district_category$rate_last12 <- round(district_category$last12mos/district_category$population*100000,1)
# calculate a multiyear rate
district_category$rate_prior3years <- round(district_category$avg_prior3years/district_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_category <- district_category %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_category <- district_category %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each type of crime BY POLICE BEAT
district_type <- cary_crime %>%
  filter(!is.na(district)) %>%
  group_by(district,type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every type
district_type <- left_join(list_district_type,district_type,by=c("district"="district","type"="type"))
# rename the year columns
district_type <- district_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
district_type_last12 <- cary_crime_last12 %>%
  group_by(district,type) %>%
  summarise(last12mos = n())
district_type <- left_join(district_type,district_type_last12,by=c("district","type"))
rm(district_type_last12)
# add zeros where there were no crimes tallied that year
district_type[is.na(district_type)] <- 0
# Calculate a total across the 3 prior years
district_type$total_prior3years <- district_type$total19+district_type$total20+district_type$total21
district_type$avg_prior3years <- round(district_type$total_prior3years/3,1)
# calculate increases
district_type$inc_19to21 <- round(district_type$total21/district_type$total19*100-100,1)
district_type$inc_19tolast12 <- round(district_type$last12mos/district_type$total19*100-100,1)
district_type$inc_21tolast12 <- round(district_type$last12mos/district_type$total21*100-100,1)
district_type$inc_prior3yearavgtolast12 <- round((district_type$last12mos/district_type$avg_prior3years)*100-100,0)
# add population for beats
district_type <- full_join(districts,district_type,by=c("district"="district"))
# calculate the beat by beat rates PER 1K people
district_type$rate19 <- round(district_type$total19/district_type$population*100000,1)
district_type$rate20 <- round(district_type$total20/district_type$population*100000,1)
district_type$rate21 <- round(district_type$total21/district_type$population*100000,1)
district_type$rate_last12 <- round(district_type$last12mos/district_type$population*100000,1)
# calculate a multiyear rate
district_type$rate_prior3years <- round(district_type$avg_prior3years/district_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_type <- district_type %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_type <- district_type %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
district_detailed %>% st_drop_geometry() %>% write_csv("data/output/districts/district_detailed.csv")
district_category %>% st_drop_geometry() %>% write_csv("data/output/districts/district_category.csv")
district_type %>% st_drop_geometry() %>% write_csv("data/output/districts/district_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_district <- district_category %>% filter(category=="Murder")
# sexassaults_district <- district_category %>% filter(category=="Sexual Assault")
autothefts_district <- district_category %>% filter(category=="Motor Vehicle Theft")
thefts_district <- district_category %>% filter(category=="Larceny")
burglaries_district <- district_category %>% filter(category=="Burglary")
robberies_district <- district_category %>% filter(category=="Robbery")
assaults_district <- district_category %>% filter(category=="Aggravated Assault")
violence_district <- district_type %>% filter(type=="Violent")
property_district <- district_type %>% filter(type=="Property")

# Create same set of tables for citywide figures
murders_city <- citywide_category %>% filter(category=="Murder")
# sexassaults_city <- citywide_category %>% filter(category=="Sexual Assault")
autothefts_city <- citywide_category %>% filter(category=="Motor Vehicle Theft")
thefts_city <- citywide_category %>% filter(category=="Larceny")
burglaries_city <- citywide_category %>% filter(category=="Burglary")
robberies_city <- citywide_category %>% filter(category=="Robbery")
assaults_city <- citywide_category %>% filter(category=="Aggravated Assault")
violence_city <- citywide_type %>% filter(type=="Violent")
property_city <- citywide_type %>% filter(type=="Property")

# Using hour to identify the hours of day when murders happen
when_murders_happen <- cary_crime %>%
  filter(category=="Murder") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_district %>% st_drop_geometry() %>% write_csv("data/output/districts/murders_district.csv")
#sexassaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/sexassaults_district.csv")
autothefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/autothefts_district.csv")
thefts_district %>% st_drop_geometry() %>% write_csv("data/output/districts/thefts_district.csv")
burglaries_district %>% st_drop_geometry() %>% write_csv("data/output/districts/burglaries_district.csv")
robberies_district %>% st_drop_geometry() %>% write_csv("data/output/districts/robberies_district.csv")
assaults_district %>% st_drop_geometry() %>% write_csv("data/output/districts/assaults_district.csv")
violence_district %>% st_drop_geometry() %>% write_csv("data/output/districts/violence_district.csv")
property_district %>% st_drop_geometry() %>% write_csv("data/output/districts/property_district.csv")

# TEST TEST TEST OF WHETHER RDS WILL WORK FOR TRACKERS IN AUTOMATION
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
#saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(robberies_city,"scripts/rds/retailthefts_city.rds")

saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
#saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")

# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(cary_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# additional table exports for specific charts
when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="NC")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")


