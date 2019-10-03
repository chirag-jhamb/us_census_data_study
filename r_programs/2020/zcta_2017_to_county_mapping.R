#####################################################################
# this program takes the
# 2010 county map, /groups/brooksgrp/maps/united_states/census2010/counties/cnty_2010_20140313.*
#  and subset to study counties
# And intersect it with
# 2017 ZCTAs: /groups/brooksgrp/maps/united_states/census2010/zcta/2017_census_zcta_shapefile/

# zcta2017_to_county.R
# Created by: deepak
# Created on: 12/12/18
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

outDir = paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data/")

# read the county area census 2010 data
counties <- st_read(paste0(groupDir,"/maps/united_states/census2010/counties"),layer="cnty_2010_20140313",stringsAsFactors = F)

# subset for relevant state and counties
counties <- counties[which((counties$COUNTY %in% c("001") & counties$STATE %in% c("11"))|
                        (counties$COUNTY %in% c("009","017","021","031","033") & counties$STATE %in% c("24"))|
                        (counties$COUNTY %in% c("510","013","043","047","059","600","610","061","630",
                                                 "107","683","685","153","157","177","179","187")
                         & counties$STATE %in% c("51"))|
                        (counties$COUNTY %in% c("037") & counties$STATE %in% c("54"))),]

# read the zcta area data
zctas <- st_read(paste0(groupDir,"/maps/united_states/census2010/zcta/2017_census_zcta_shapefile"),
                 layer="cb_2017_us_zcta510_500k",stringsAsFactors = F)

#print(table(st_geometry_type(zctas)))
# calculate the area of the zctas
zctas$zcta_area <- st_area(zctas)

# intersect the county and zcta data
int.counties_zctas <- st_intersection(counties,zctas)

head(int.counties_zctas)

# calculate the intersected area
int.counties_zctas$intersected_area <- st_area(int.counties_zctas)

head(int.counties_zctas)

#print(table(st_geometry_type(int.counties_zctas)))

# find total number and unique number of ZCTAs
nrow(int.counties_zctas)
length(unique(int.counties_zctas$ZCTA5CE10))

int.counties_zctas <- int.counties_zctas %>% mutate('percent_intersected_area'=intersected_area/zcta_area)

head(int.counties_zctas$percent_intersected_area)


int.counties_zctas$percent_intersected_area <- as.numeric(int.counties_zctas$percent_intersected_area)

int.counties_zctas_greater_0.5 <- int.counties_zctas #%>% filter(percent_intersected_area>0)

# check if zcta ids greater than 0.5 unique or not
print(nrow(int.counties_zctas_greater_0.5)==length(unique(int.counties_zctas_greater_0.5$ZCTA5CE10)))

# # sort by ZCTA id and reverse of intersected area
# # ref https://stackoverflow.com/questions/12805964/remove-duplicates-keeping-entry-with-largest-absolute-value
# int.county_zctas <- int.county_zctas[order(int.county_zctas$ZCTA5CE10, -int.county_zctas$intersected_area), ] 
# 
# #print(head(int.county_zctas))
# 
# # take the ZCTA id row with highest value of intersected area (which means the first occurence as we have revese sorted the data on area in previous step)
# int.county_zctas_unique <- int.county_zctas[!duplicated(int.county_zctas$ZCTA5CE10), ]  
# 
# head(int.county_zctas_unique)
# nrow(int.county_zctas_unique)

print(table(st_geometry_type(int.counties_zctas_greater_0.5)))

# select the relevant columns of state, county and zcta
int.counties_zctas_greater_0.5_subset <- int.counties_zctas_greater_0.5 %>% select(STATE,COUNTY,ZCTA5CE10,CENSUSAREA,zcta_area,intersected_area,percent_intersected_area,NAME,LSAD)

# remove the geometry column
st_geometry(int.counties_zctas_greater_0.5_subset) <- NULL

# sort the data for state and county
int.counties_zctas_greater_0.5_subset <- int.counties_zctas_greater_0.5_subset[order(int.counties_zctas_greater_0.5_subset$STATE, 
                                                                                     int.counties_zctas_greater_0.5_subset$COUNTY),]

head(int.counties_zctas_greater_0.5_subset)

colnames(int.counties_zctas_greater_0.5_subset)[3] <- 'ZCTA'

# save the data in output directory
write.csv(int.counties_zctas_greater_0.5_subset, paste0(outDir,dateo,"_state_county_zcta.csv"), row.names = F)

st_geometry(int.counties_zctas_greater_0.5) <- NULL

write.csv(int.counties_zctas_greater_0.5, paste0(outDir,dateo,"_state_county_zcta_data.csv"), row.names = F)



