###########################################################################
# Takes csv created by the program /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/load_1970_2000_sasdata.py
# transforms and adds data to output format columns
# Reads ACS years and years 1970-2000.

# Writes the data subset to csv for visualisation
# Author: Chirag Jhamb
################################################################################

library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(sf)
library(stringr)
library(tidyr)
# file where 1970-2000 data is
# file created using /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/load_1970_2000_sasdata.py
data_file_1970_2000_census <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/20191211_1970_to_2000_county_census.csv"

# folder where ACS years o/p data is
# files created using /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/acs_county_2010_2017_v02.py
data_folder_acs_years <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_msa"
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")

# function to get list of counties in a msa, given its msa_code. Returns unique list of county FIPS
get_msa_counties <- function(cbsa_code){
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  msa_cols <- c("CSA","LSAD","STCOU","NAME","CBSA")
  msa_data <- msa_data[msa_cols]
  msa_counties <- subset(msa_data, LSAD=="County or equivalent" & CBSA==cbsa_code)   #subset as per MSA
  return(as.vector(unique(msa_counties$STCOU)))
}

# function to get names of counties in a msa, given its msa_code. Returns dataframe having name and code of each county in the msa
get_county_names_msa <- function(cbsa_code){
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  msa_cols <- c("CSA","LSAD","STCOU","NAME", "CBSA")
  msa_data <- msa_data[msa_cols]
  msa_counties <- subset(msa_data, LSAD=="County or equivalent" & CBSA==cbsa_code)   #subset as per MSA county
  return(msa_counties[c("STCOU","NAME")])
}

# function to get names of each msa and its respective CBSA code
get_top_n_msa <- function(top_n = 10){
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  msa_only <- subset(msa_data, LSAD=="Metropolitan Statistical Area")
  msa_only <- msa_only[order(msa_only$CENSUS2010POP, decreasing=TRUE), ]
  top_10_msa <- head(msa_only,top_n)
  return(top_10_msa[c("CBSA","NAME")])
}
# function to get names of counties in a msa, given its msa_code. Returns dataframe having name and code of each county in the msa
get_top_n_msa_county_level <- function(top_n = 10){
  msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
  msa_data <- read.csv(msa_file)
  # create a DataFrame msa_only having code and name of top 10 MSAs by population
  msa_only <- subset(msa_data, LSAD=="Metropolitan Statistical Area")
  msa_only <- msa_only[order(msa_only$CENSUS2010POP, decreasing=TRUE), ]
  top_10_msa <- head(msa_only,top_n)
  top_10_msa_names <- top_10_msa[c("CBSA","NAME")]
  top_10_msa_cbsa <- as.vector(unique(top_10_msa$CBSA))
  # using the code of top 10 MSAs, create a dataframe of all the counties that are there in those MSAs
  top_10_msa_counties <- msa_data[msa_data$CBSA %in% top_10_msa_cbsa,]
  top_10_msa_counties <- subset(top_10_msa_counties, LSAD=="County or equivalent")
  top_10_msa_counties <- top_10_msa_counties[c("CBSA","NAME","LSAD","STCOU")]
  # merge the county dataframe with MSA dataframe, having one row per county and a column mentioning MSA name
  names(top_10_msa_names)[names(top_10_msa_names) == 'NAME'] <- 'msa_name'  # rename before merge
  top_10_msa <- merge(top_10_msa_counties, top_10_msa_names, by="CBSA",all = TRUE)
  names(top_10_msa)[names(top_10_msa) == 'STCOU'] <- 'FIPS'  # standard name across datasets
  names(top_10_msa)[names(top_10_msa) == 'NAME'] <- 'county_name'  # standard name across datasets
  return(top_10_msa)
}


reformat_acs_data <- function(df){
  data <- as.data.frame(t(df))
  # remove unnecessary columns
  data <- data[-c(1,2),]
  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index
  # print the df to check if the format is appropriate
  # rename the columns to avoid column duplicates error
  colnames(data) <- make.unique(names(data))
  #names of columns in data frame
  cols <- colnames(data)
  # character variables
  cols.char <- c("FILEID","FILETYPE","STATE","GEO_NAME","GEO_ID")
  #numeric variables
  cols_num <- cols[!cols %in% cols.char]
  # write a function to convert the required columns to numeric
  make_num <- function(x)
  {
    return(as.numeric(as.character(x)))
  }
  cols_num_unique <- cols_num
  subset_cols <- c(cols_num_unique,cols.char)
  data_sub <- data[subset_cols]
  # make all the required columns numeric
  data_sub[cols_num_unique] <- lapply(data_sub[cols_num_unique],make_num)
  # create column state country code
  data_sub["state_county_code"] <- rownames(data_sub)
  # split the column GEO NAME to extract county and state name
  data_sub <- as.data.frame(cSplit(data_sub,c("GEO_NAME"),',',drop = F))
  # split the column country code to get state and county codes
  data <- as.data.frame(cSplit(data_sub,c("state_county_code"),'_',drop = F,type.convert = FALSE))
  # rename the splitted columns
  names(data)[names(data)=="GEO_NAME_1"] <- "county_name"
  names(data)[names(data)=="GEO_NAME_2"] <- "state_name"
  names(data)[names(data)=="state_county_code_1"] <- "state_code"
  names(data)[names(data)=="state_county_code_2"] <- "county_code"
  data$FILETYPE <- as.character(data$FILETYPE)
  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))
  data$FIPS <- paste(data$STATE_FIPS,data$county_code, sep="")
  return(data)
}

# sum race variables on ACS years data to create household type columns
reformat_acs_household_type_subset <- function(data) {
  colnames(data)[which(colnames(data) == "B11005_2_households_with_one_or_more_people_under_18_years")] <- 'household_with_kids'
  colnames(data)[which(colnames(data) == "B11005_12_family_households")] <- 'family_household_without_kids'
  colnames(data)[which(colnames(data) == "B11005_17_nonfamily_households")] <- 'non_family_household_without_kids'
  return(data)
}

# sum B11016 variables to create houlsehold size
reformat_acs_household_size_subset <- function(data) {
  data <- data %>% mutate('household_size_1'=select(.,matches('B11016_10_1-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_2'=select(.,matches('B11016_3_2-person_household|B11016_11_2-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_3_to_4'=select(.,matches('B11016_12_3-person_household|B11016_13_4-person_household|B11016_5_4-person_household|B11016_4_3-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_more_than_4'=select(.,matches('B11016_14_5-person_household|B11016_15_6-person_household|B11016_16_7-or-more_person_household|B11016_6_5-person_household|B11016_7_6-person_household|B11016_8_7-or-more_person_household')) %>% apply(1, sum, na.rm=TRUE))
  return(data)
}

# sum age variables on ACS years data to create age columns (also the total_population column)
reformat_acs_age_subset <- function(data) {
  data <- data %>% mutate('less_than_18'=
                     select(.,matches('under_5_years|5_to_9_years|10_to_14_years|15_to_17_years$')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('18_to_29'=
      select(.,matches('18_and_19_years|20_years|21_years|22_to_24_years|25_to_29_years$')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('30_to_44'=
      select(.,matches('30_to_34_years|35_to_39_years|40_to_44_years$')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('45_to_59'=
                   select(.,matches('45_to_49_years|50_to_54_years|55_to_59_years$')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('above_59'=
                select(.,matches('60_and_61_years|62_to_64_years|65_and_66_years|67_to_69_years|70_to_74_years|75_to_79_years|80_to_84_years|85_years_and_over$')) %>% apply(1, sum, na.rm=TRUE))
  colnames(data)[which(colnames(data) == "B01001_1_total")] <- 'total_population'
  return(data)
}

# sum race variables on ACS years data to create race columns
reformat_acs_race_subset <- function(data) {
  data <- data %>% mutate(year=substr(FILETYPE,1,4))
  data <- data %>% mutate('white_alone'=
                     select(.,matches('B03002_3_white_alone|B03002_13_white_alone$')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('AA_alone'=
      select(.,matches('B03002_4_black_or_african_american_alone|B03002_14_black_or_african_american_alone$')) %>% apply(1, sum, na.rm=TRUE))
  colnames(data)[which(colnames(data) == "B03002_12_hispanic_or_latino")] <- 'hispanic_or_latino'
  return(data)
}



# takes dataframe from 1970 to 2000 data, divides it into DFs by decade and then rbind to stack them up and return
reformat_1970_2000_data <- function(data){
  # year 1970:
  data_1970 <- subset(data, year=="1970")
  # age 1970 columns:
  # male: 'm17_1' onwards and then female 'm17_28' onwards
# 1 to 10 and 28 to 37
# data_1970 <- transform(data = data_1970, lt18_v2 = sum(m17_1-m17_10) + sum(m17_28-m17_30))
  data_1970$less_than_18 <- rowSums(data_1970[,c("m17_1","m17_2","m17_3","m17_4","m17_5","m17_6","m17_7","m17_8","m17_9","m17_10","m17_28","m17_29","m17_30","m17_31","m17_32","m17_33","m17_34","m17_35","m17_36","m17_37")])
  # 11 to 16 and 38 to 43
  data_1970['18_to_29'] <- rowSums(data_1970[,c("m17_11","m17_12","m17_13","m17_14","m17_15","m17_16","m17_38","m17_39","m17_40","m17_41","m17_42","m17_43")])
  # 17 to 19 and 44 to 46
  data_1970['30_to_44'] <- rowSums(data_1970[,c("m17_17","m17_18","m17_19","m17_44","m17_45","m17_46")])
# 20 to 22 and 47 to 49
  data_1970['45_to_59'] <- rowSums(data_1970[,c("m17_20","m17_21","m17_22","m17_47","m17_48","m17_49")])
# 23 to 27 and 50 to 54
  data_1970['above_59'] <- rowSums(data_1970[,c("m17_23","m17_24","m17_25","m17_26","m17_27","m17_50","m17_51","m17_52","m17_53","m17_54")])
  data_1970$total_population <- rowSums(data_1970[,c("less_than_18","18_to_29","30_to_44","45_to_59","above_59")])

# year 1980:
  data_1980 <- subset(data, year=="1980")
  # age 1980 columns:
  # 1 to 11 & 27 to 37
  data_1980['less_than_18'] <- rowSums(data_1980[,c("t15_1","t15_2","t15_3","t15_4","t15_5","t15_6","t15_7","t15_8","t15_9","t15_10","t15_11","t15_27","t15_28","t15_29","t15_30","t15_31","t15_32","t15_33","t15_34","t15_35","t15_36","t15_37")])
  # 12 to 17 & 38 to 43
  data_1980['18_to_29'] <- rowSums(data_1980[,c("t15_12","t15_13","t15_14","t15_15","t15_16","t15_17","t15_38","t15_39","t15_40","t15_41","t15_42","t15_43")])
  # 18 and 19 & 44 and 45
  data_1980['30_to_44'] <- rowSums(data_1980[,c("t15_18","t15_19","t15_44","t15_45")])
  # 20 and 21 & 46 and 47
  data_1980['45_to_59'] <- rowSums(data_1980[,c("t15_20","t15_21","t15_46","t15_47")])
  # 22 to 26 & 48 to 52
  data_1980['above_59'] <- rowSums(data_1980[,c("t15_22","t15_23","t15_24","t15_25","t15_26","t15_48","t15_49","t15_50","t15_51","t15_52")])

  # race 1980 columns
  data_1980['white_alone'] <- rowSums(data_1980[,c("t12_1","t14_2")])
  data_1980['AA_alone'] <- rowSums(data_1980[,c("t12_2","t14_3")])
  names(data_1980)[names(data_1980) == 't14_1'] <- 'hispanic_or_latino'  # rename since only one column, need atleast two columns to apply the sum function
  # data_1980['hispanic_or_latino'] <- rowSums(data_1980[,c("t14_1")])
  # data_1980['total_population'] <- rowSums(data_1980[,c("less_than_18","18_to_29","30_to_44","45_to_59","above_59")])
  print("Running t3_1 for total population")
  names(data_1980)[names(data_1980) == 't3_1'] <- 'total_population'  # rename since only one column, need atleast two columns to apply the sum function
  # household type 1980 columns
  data_1980['household_with_kids'] <- rowSums(data_1980[,c("t20_1","t20_3","t20_5")])
  data_1980['family_household_without_kids'] <- rowSums(data_1980[,c("t20_2","t20_4","t20_6")])
  # data_1980['non_family_household_without_kids'] <-
  names(data_1980)[names(data_1980) == 't20_7'] <- 'non_family_household_without_kids'  # rename since only one column, need atleast two columns to apply the sum function
  #household size columns:
  names(data_1980)[names(data_1980) == 't18_1'] <- 'household_size_1'
  names(data_1980)[names(data_1980) == 't18_2'] <- 'household_size_2'
  data_1980['household_size_3_to_4'] <- rowSums(data_1980[,c("t18_4","t18_3")])
  data_1980['household_size_more_than_4'] <- rowSums(data_1980[,c("t18_5","t18_6")])
  # year 1990:
  data_1990 <- subset(data, year=="1990")
  names(data_1990)[names(data_1990) == 'P1_1'] <- 'total_population'  # rename since only one column
  # age 1990 columns:
  data_1990['less_than_18'] <- rowSums(data_1990[,c("P13_1","P13_2","P13_3","P13_4","P13_5","P13_6","P13_7","P13_8","P13_9","P13_10","P13_11","P13_12")])
  data_1990['18_to_29'] <- rowSums(data_1990[,c("P13_13","P13_14","P13_15","P13_16","P13_17","P13_18")])
  data_1990['30_to_44'] <- rowSums(data_1990[,c("P13_19","P13_20","P13_21")])
  data_1990['45_to_59'] <- rowSums(data_1990[,c("P13_22","P13_23","P13_24")])
  data_1990['above_59'] <- rowSums(data_1990[,c("P13_25","P13_26","P13_27","P13_28","P13_29","P13_30","P13_31")])

  # race 1990 columns
  data_1990['white_alone'] <- rowSums(data_1990[,c("P12_11","P12_21")])
  data_1990['AA_alone'] <- rowSums(data_1990[,c("P12_12","P12_22")])
  data_1990['hispanic_or_latino'] <- rowSums(data_1990[,c("P12_15","P12_21","P12_22","P12_23","P12_24","P12_25")])

  # household type 1990 columns
  data_1990['household_with_kids'] <- rowSums(data_1990[,c("P19_1","P19_3","P19_5")])
  data_1990['family_household_without_kids'] <- rowSums(data_1990[,c("P19_2","P19_4","P19_6")])
  names(data_1990)[names(data_1990) == 'P19_7'] <- 'non_family_household_without_kids'
  #household size columns:
  names(data_1990)[names(data_1990) == 'P16_1'] <- 'household_size_1'
  names(data_1990)[names(data_1990) == 'P16_2'] <- 'household_size_2'
  data_1990['household_size_3_to_4'] <- rowSums(data_1990[,c("P16_3","P16_4")])
  data_1990['household_size_more_than_4'] <- rowSums(data_1990[,c("P16_5","P16_6","P16_7")])
  # year 2000:
  data_2000 <- subset(data, year=="2000")
  names(data_2000)[names(data_2000) == 'P7_1'] <- 'total_population'  # rename since only one column
  # age 2000 columns:
  # less_than_18: 3 to 20 and 42 to 59
  data_2000['less_than_18'] <- rowSums(data_2000[,c("P8_3","P8_4","P8_5","P8_6","P8_7","P8_8","P8_9","P8_10","P8_11","P8_12","P8_13","P8_14","P8_15","P8_16","P8_17","P8_18","P8_19","P8_20","P8_42","P8_43","P8_44","P8_45","P8_46","P8_47","P8_48","P8_49","P8_50","P8_51","P8_52","P8_53","P8_54","P8_55","P8_56","P8_57","P8_58","P8_59")])
  # 18_to_29: 21 to 26 and 60 to 65
  data_2000['18_to_29'] <- rowSums(data_2000[,c("P8_21","P8_22","P8_23","P8_24","P8_25","P8_26","P8_60","P8_61","P8_62","P8_63","P8_64","P8_65")])
  # 30_to_44: 27 to 29 and 66 to 68
  data_2000['30_to_44'] <- rowSums(data_2000[,c("P8_27","P8_28","P8_29","P8_66","P8_67","P8_68")])
  # 45_to_59: 30 to 32 and 69 to 71
  data_2000['45_to_59'] <- rowSums(data_2000[,c("P8_30","P8_31","P8_32","P8_69","P8_70","P8_71")])
  # above_59: 33 to 40 and 72 to 79
  data_2000['above_59'] <- rowSums(data_2000[,c("P8_33","P8_34","P8_35","P8_36","P8_37","P8_38","P8_39","P8_40","P8_72","P8_73","P8_74","P8_75","P8_76","P8_77","P8_78","P8_79")])

  # data_2000['pop_share'] <- rowSums(data_2000[,c("less_than_18","18_to_29","30_to_44","45_to_59","above_59")])
  # data_2000['pop_share'] <- data_2000['pop_share']/data_2000['total_population']
  # print("Age Share data_2000:")
  # print(head(data_2000['pop_share']))
  # race 2000 columns
  data_2000['white_alone'] <- rowSums(data_2000[,c("P7_3","P7_11")])
  data_2000['AA_alone'] <- rowSums(data_2000[,c("P7_4","P7_12")])
  names(data_2000)[names(data_2000) == 'P7_10'] <- 'hispanic_or_latino'  # rename since only one column
  names(data_2000)[names(data_2000) == 'P7_1'] <- 'total_population'  # rename since only one column


  # household type 2000 columns
  data_2000['household_with_kids'] <- rowSums(data_2000[,c("P10_8","P10_12","P10_15")])
  data_2000['family_household_without_kids'] <- rowSums(data_2000[,c("P10_9","P10_13","P10_16")])
  data_2000['non_family_household_without_kids'] <- rowSums(data_2000[,c("P10_2","P10_17")])
  #household size columns:
  names(data_2000)[names(data_2000) == 'P14_10'] <- 'household_size_1'  # rename since only one column
  data_2000['household_size_2'] <- rowSums(data_2000[,c("P14_11","P14_3")])
  data_2000['household_size_3_to_4'] <- rowSums(data_2000[,c("P14_12","P14_13","P14_4","P14_5")])
  data_2000['household_size_more_than_4'] <- rowSums(data_2000[,c("P14_6","P14_7","P14_8","P14_14","P14_15","P14_16")])

  print("1970-2000 data columns merged, subsetting and concatinating")
  columns_subset <- c('FIPS', 'year','less_than_18', '18_to_29', '30_to_44', '45_to_59', 'above_59', "total_population")
  data_1970 <- data_1970[columns_subset]  # limited data for 1970
  # add more columns/categories for years after 1970:
  # columns_subset <- c(columns_subset, "household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4",   "white_alone", "AA_alone", "hispanic_or_latino")
  columns_subset <- c(columns_subset, "household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4", "household_with_kids", "family_household_without_kids", "non_family_household_without_kids", "white_alone", "AA_alone", "hispanic_or_latino")
  # get the required columns
  data_1980 <- data_1980[columns_subset]
  data_1990 <- data_1990[columns_subset]
  data_2000 <- data_2000[columns_subset]
  # bind all years data and return
  print("1970-2000 data binding..")

  data_1970_to_2000 <- rbindlist(list(data_2000, data_1990, data_1980, data_1970), fill=TRUE,use.names=TRUE)  # set fill=true to fill empty columns of data_1970
  house_cols <- c("household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4")
  race_cols <- c("white_alone","AA_alone","hispanic_or_latino", "total_population")
  age_cols <- c("less_than_18", "18_to_29","30_to_44","45_to_59","above_59")
  household_type_cols <- c("household_with_kids", "family_household_without_kids", "non_family_household_without_kids")
  data_1980['pop_share'] <- rowSums(data_1980[,race_cols])
  data_1990['pop_share'] <- rowSums(data_1990[,race_cols])
  data_2000['pop_share'] <- rowSums(data_2000[,race_cols])
  print("Race Share 1980:")
  data_1980['pop_share'] <- data_1980['pop_share']/data_1980['total_population']
  print(head(data_1980['pop_share']))
  print("Race Share 1990:")
  data_1990['pop_share'] <- data_1990['pop_share']/data_1990['total_population']
  print(head(data_1990['pop_share']))
  print("Race Share 2000:")
  data_2000['pop_share'] <- data_2000['pop_share']/data_2000['total_population']
  print(head(data_2000['pop_share']))

  data_1980['pop_share'] <- rowSums(data_1980[,age_cols])
  data_1990['pop_share'] <- rowSums(data_1990[,age_cols])
  data_2000['pop_share'] <- rowSums(data_2000[,age_cols])
  print("Age Share 1980:")
  data_1980['pop_share'] <- data_1980['pop_share']/data_1980['total_population']
  print(head(data_1980['pop_share']))
  for (i in age_cols){
    data_1980[i] <- data_1980[i]/data_1980['total_population']
    data_1990[i] <- data_1990[i]/data_1990['total_population']
  }
  print(head(data_1980[age_cols]))
  
  print("Age Share 1990:")
  print(head(data_1990[age_cols]))
  # data_1990['pop_share'] <- data_1990['pop_share']/data_1990['total_population']
  # print(head(data_1990['pop_share']))
  print("Age Share 2000:")
  data_2000['pop_share'] <- data_2000['pop_share']/data_2000['total_population']
  print(head(data_2000['pop_share']))

  data_1980['pop_share'] <- rowSums(data_1980[,house_cols])
  data_1990['pop_share'] <- rowSums(data_1990[,house_cols])
  data_2000['pop_share'] <- rowSums(data_2000[,house_cols])
  print("House Share 1980:")
  data_1980['pop_share'] <- data_1980['pop_share']/data_1980['total_population']
  print(head(data_1980['pop_share']))
  print("House Share 1990:")
  data_1990['pop_share'] <- data_1990['pop_share']/data_1990['total_population']
  print(head(data_1990['pop_share']))
  print("House Share 2000:")
  data_2000['pop_share'] <- data_2000['pop_share']/data_2000['total_population']
  print(head(data_2000['pop_share']))

  data_1980['pop_share'] <- rowSums(data_1980[,household_type_cols])
  data_1990['pop_share'] <- rowSums(data_1990[,household_type_cols])
  data_2000['pop_share'] <- rowSums(data_2000[,household_type_cols])
  print("household_type_cols Share 1980:")
  data_1980['pop_share'] <- data_1980['pop_share']/data_1980['total_population']
  print(head(data_1980['pop_share']))
  print("household_type_cols Share 1990:")
  data_1990['pop_share'] <- data_1990['pop_share']/data_1990['total_population']
  print(head(data_1990['pop_share']))
  print("household_type_cols Share 2000:")
  data_2000['pop_share'] <- data_2000['pop_share']/data_2000['total_population']
  print(head(data_2000['pop_share']))

  return(data_1970_to_2000)
}

top_10_msa <- get_top_n_msa_county_level()   # get top 10 MSA's county data
top_10_msa_county_codes <- as.vector(top_10_msa$FIPS)  # list of counties that will be needed
print("Got top MSAs")
# set name of columsns which will be used for subsetting later:
house_cols <- c("household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4")
race_cols <- c("white_alone","AA_alone","hispanic_or_latino", "total_population")
age_cols <- c("less_than_18", "18_to_29","30_to_44","45_to_59","above_59")
household_type_cols <- c("household_with_kids", "family_household_without_kids", "non_family_household_without_kids")

# these columns will get added during group by:
to_add_cols <- c(age_cols, race_cols, house_cols,household_type_cols)
#other columns:
all_cols <- c(to_add_cols, "FIPS", "year")
# all_cols <- c(to_add_cols, "FIPS", "year", "MSA")

# read 1970 to 2000 data:
data_1970_to_2000 <- read.csv(data_file_1970_2000_census)
# get only the counties which are in the top 10 MSAs
print("Subsetting data_1970_to_2000")
data_1970_to_2000 <- data_1970_to_2000[data_1970_to_2000$FIPS %in% as.character(top_10_msa_county_codes),]
print(dim(data_1970_to_2000))
#convert data to required columns:
print("Reformatting data_1970_to_2000")
data_1970_to_2000 <- reformat_1970_2000_data(data_1970_to_2000)
# add MSA name to data:
print("Adding MSAs to FIPS of data_1970_to_2000")
top_10_msa$FIPS <- as.character(top_10_msa$FIPS)
data_1970_to_2000$FIPS <- as.character(data_1970_to_2000$FIPS)
data_1970_to_2000 <- merge(data_1970_to_2000, top_10_msa, by="FIPS")
print(dim(data_1970_to_2000))
print(names(data_1970_to_2000))
print("Subsetting data_1970_to_2000")
# data_1970_to_2000 <- data_1970_to_2000[all_cols]
# get list of all files in acs years, present in the path data_folder_acs_years. Stack them together
acs_files <- list.files(path = data_folder_acs_years, pattern = "20191202*", full.names = TRUE)
print("Getting list of ACS files")
dfn <- data.frame()
# read acs files and stack them together:
for (i in acs_files){
  acs_data <- read.csv(i)
  # reformat to required transopse shape
  acs_data <- reformat_acs_data(acs_data)
  print(dim(acs_data))
  acs_data$MSA <- strsplit(i,"_")[[1]][16]  # take msa from the filename
  dfn <- dplyr::bind_rows(dfn, acs_data)
}

data_acs_years <- dfn
# get sum of columns for race
# B11005
print("reformating ACS race subset")
data_acs_years <- reformat_acs_race_subset(data_acs_years)
# get sum of columns for age
print(dim(data_acs_years))
print("reformating ACS age subset")
data_acs_years <- reformat_acs_age_subset(data_acs_years)
print(dim(data_acs_years))
# get sum of columns for household size
print("reformating HH size subset")
data_acs_years <- reformat_acs_household_size_subset(data_acs_years)
print(dim(data_acs_years))
print("reformating HH TYPE subset")
data_acs_years <- reformat_acs_household_type_subset(data_acs_years)
print(dim(data_acs_years))
# get rid of variables not relevant to the function:
data_acs_years <- data_acs_years[all_cols]
print(dim(data_acs_years))

print("Combining data_acs_years and data_1970_to_2000")
all_years_df <- rbindlist(list(data_acs_years, data_1970_to_2000),fill=TRUE)
print(dim(all_years_df))
# subset dmv level data for merging later with MSA level data:
dmv_subset <- all_years_df[all_years_df$FIPS %in% as.character(get_msa_counties(47900)),]
dmv_subset$level <- "county_level"

# cols_to_be_added <- c('less_than_18', '18_to_29', '30_to_44', '45_to_59', 'above_59', "total_population","household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4", "household_with_kids", "family_household_without_kids", "non_family_household_without_kids", "white_alone", "AA_alone", "hispanic_or_latino")
# cols_to_be_added <- c('less_than_18', '18_to_29', '30_to_44', '45_to_59', 'above_59', "total_population","household_size_1", "household_size_2", "household_size_3_to_4", "household_size_more_than_4", "white_alone", "AA_alone", "hispanic_or_latino")
# sum_by <- c("MSA","year")
# slct <- c(sum_by,cols_to_be_added)
# added_msa <- data_1970_to_2000 %>%group_by(.dots=sum_by) %>%summarise_each(funs(sum))
print(head(all_years_df))
print("Creating MSA level dataset")
all_years_df$CBSA <- NULL
all_years_df$county_name <- NULL
all_years_df$LSAD  <- NULL
all_years_df$msa_name <- NULL
all_years_df <- merge(all_years_df, top_10_msa, by="FIPS")
print(names(all_years_df))
print(unique(all_years_df$CBSA))
print(unique(all_years_df$year))
# create sum of total population by MSA level
total_pop_df <- all_years_df %>% group_by(CBSA, year) %>% summarise(total_population = sum(total_population))
# loop through each column to be added and groupby to create it's MSA level data, merge with total_pop_df data:
print(dim(total_pop_df))
for (k in to_add_cols){
  df_temp <- all_years_df %>% group_by(CBSA, year) %>% summarise(tempo = sum(!!sym(k)))
  colnames(df_temp)[which(colnames(df_temp) == "tempo")] <- k
  total_pop_df <- merge(total_pop_df, df_temp, all.x=TRUE)
  # print(names(total_pop_df))
  print(dim(total_pop_df))
  print("\n")
}

total_pop_df$level <- "msa_level"
print("total population df complete")
print(names(total_pop_df))
print(dim(total_pop_df))
# final_df <- merge(final_df,top_10_msa,by="FIPS",all.x=TRUE)
top_n_msa_name <- get_top_n_msa()
total_pop_df <- merge(total_pop_df, top_n_msa_name, by="CBSA")

print(head(total_pop_df))
# print(names(total_pop_df))
print("Stacking total_pop_df and dmv_subset to create final_df..")
#delete the columns which are not needed for MSAs:
final_df <- rbindlist(list(total_pop_df, dmv_subset),fill=TRUE)
# delete msa_name variable (redundant)
final_df$msa_name <- NULL
print(head(final_df))
filename <- paste("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/",dateo,"_dataset_all_years.csv",sep="")
print("Saving file..")
write.csv(final_df, filename)
print(filename)
