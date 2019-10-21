# AUthor: Chirag Jhamb
# This code is to report number of block groups & number of census tracts by county for DMV
library(dplyr)
library(sf)
library(tidyr)

dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

output_file <- paste("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191018_meeting/",dateo,"census_tracts_table.csv",sep="")

get_msa_counties <- function(msa_code, cbsa_code){ # function to get list of counties in a msa, given its msa_code. Returns unique list of county FIPS
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  msa_cols <- c("CSA","LSAD","STCOU","NAME","CBSA")
  msa_data <- msa_data[msa_cols]
  msa_counties <- subset(msa_data, LSAD=="County or equivalent" & CBSA==cbsa_code)   #subset as per
  return(as.vector(unique(msa_counties$STCOU)))
}

get_county_names_msa <- function(msa_code, cbsa_code){ # function to get names of counties in a msa, given its msa_code. Returns dataframe having name and code of each county in the msa
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  msa_cols <- c("CSA","LSAD","STCOU","NAME", "CBSA")
  msa_data <- msa_data[msa_cols]
  msa_counties <- subset(msa_data, LSAD=="County or equivalent" & CBSA==cbsa_code)   #subset as per
  return(msa_counties[c("STCOU","NAME")])
}

# read the tract file
US_blck_grp_2010_file <- "/groups/brooksgrp/maps/united_states/census2010/block_groups/nhgis0020_shape/US_blck_grp_2010.dbf"
US_blck_grp_2010 <- st_read(US_blck_grp_2010_file)
# geometry not needed
US_blck_grp_2010$geometry <- NULL
# US_blck_grp_2010$SS <- substr(US_blck_grp_2010$GEOID10,1,2)
# US_blck_grp_2010$CCC <- substr(US_blck_grp_2010$GEOID10,3,5)
US_blck_grp_2010$TTTTT <- substr(US_blck_grp_2010$GEOID10,6,11)   #census_tracts
US_blck_grp_2010$TTTTTB <- substr(US_blck_grp_2010$GEOID10,6,12)   #census_tracts  and block_groups
US_blck_grp_2010$SSCCC <- substr(US_blck_grp_2010$GEOID10,1,5)   #FIP of county
dmv_msa <- 548
cbsa_code <- 47900
dmv_counties <- get_msa_counties(dmv_msa, cbsa_code)   #get list of all counties in dmv msa
dmv_blck_grp <- US_blck_grp_2010[US_blck_grp_2010$SSCCC %in% dmv_counties, ]   #subset the US_blck_grp to only have dmv counties

unique_tract_by_county <- group_by(.data = dmv_blck_grp, SSCCC) %>% summarise(unique_tract = n_distinct(TTTTT))   #  create a new dataframe that is the number of unique tracts per county
unique_block_by_county <- group_by(.data = dmv_blck_grp, SSCCC) %>% summarise(unique_block = n_distinct(TTTTTB))  #  create a new dataframe that is the number of unique block groups per county
# merge county names with the county codes in the created dataframe
county_names <- get_county_names_msa(dmv_msa, cbsa_code)
names(county_names)[names(county_names) == 'STCOU'] <- 'SSCCC'
unique_block_by_county <- merge(unique_block_by_county, county_names, by="SSCCC",all.x = TRUE)
# merge both the created dataframe to get unique block groups and unique tracts in the same dataframe
final_data <- merge(unique_block_by_county, unique_tract_by_county, by = "SSCCC")
# save file
write.csv(final_data, file = output_file)
