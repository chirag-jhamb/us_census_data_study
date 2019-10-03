#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2016_v02.py
# to merge it together
# March 14, 2019
#
# acs_county_merge_data_v01.R
##############################################################################
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(stringr)

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/")


file_date <- "20190303"

# load the data
acs_cnt_2007_2011 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2007_2011_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2008_2012 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2008_2012_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2009_2013 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2009_2013_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2010_2014 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2010_2014_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2011_2015 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2011_2015_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2013_2017 <- read.csv(paste0(data_dir,file_date,"_cnty_acs_2013_2017_absolute_values.csv"),stringsAsFactors = F)



# write a function to reformat the data to bring it to county level and subset it for requried columns
reformat_data <- function(df) {
  
  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))
  
  # remove unnecessary columns
  data <- data[-c(1,2),]
  
  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index
  
  # print the df to check if the format is appropriate
  #print(head(data))
  
  # rename the columns to avoid column duplicates error
  #colnames(data) <- make.unique(names(data))
  
  #names of columns in data frame
  cols <- colnames(data)
  
  # character variables
  cols.char <- c("FILEID","FILETYPE","STATE","GEO_NAME","GEO_ID")
  
  #numeric variables
  cols.num <- cols[!cols %in% cols.char]
  
  
  
  # write a function to convert the required columns to numeric
  make_num <- function(x)
  {
    return(as.numeric(as.character(x)))
  }
  
  # make all the required columns numeric
  data[cols.num] <- lapply(data[cols.num],make_num)
  
  # print the dataframe to check the data types
  #print(str(data))
  
  # create column state country code
  data["state_county_code"] <- rownames(data)
  
  # split the column GEO NAME to extract county and state name
  data <- as.data.frame(cSplit(data,c("GEO_NAME"),',',drop = F))
  
  # split the column country code to get state and county codes
  data <- as.data.frame(cSplit(data,c("state_county_code"),'_',drop = F,type.convert = FALSE))
  
  # rename the splitted columns
  names(data)[names(data)=="GEO_NAME_1"] <- "county_name"
  names(data)[names(data)=="GEO_NAME_2"] <- "state_name"
  names(data)[names(data)=="state_county_code_1"] <- "state_code"
  names(data)[names(data)=="state_county_code_2"] <- "countyfips"
  
  data$FILETYPE <- as.character(data$FILETYPE)
  
  # get the year column
  data <- data %>% mutate(estimate_year=substr(FILETYPE,1,4)) 
  
  colnames(data)[which(colnames(data) == "STATE_FIPS")] <- 'statefips'
  
  # reorder to make year as first column in dataset
  data <- data %>% select(estimate_year, statefips, countyfips, everything())
  
  # select column with household income
  data_median_col <- data %>% select(contains("B19013_1"))
  
  # get name of household income column
  median_col_name <- colnames(data_median_col)
  
  print(median_col_name)
  
  # rename the median household income to get consistent name across years
  colnames(data)[which(colnames(data) == median_col_name)] <- 'B19013_1_median_household_income'
  
  
  
  return (data)
  
}

# get the data for all years 2011 to 2016 with relevant columns by calling the function
acs_cnt_2007_2011_reformat <- reformat_data(acs_cnt_2007_2011)
acs_cnt_2008_2012_reformat <- reformat_data(acs_cnt_2008_2012)
acs_cnt_2009_2013_reformat <- reformat_data(acs_cnt_2009_2013)
acs_cnt_2010_2014_reformat <- reformat_data(acs_cnt_2010_2014)
acs_cnt_2011_2015_reformat <- reformat_data(acs_cnt_2011_2015)
acs_cnt_2012_2016_reformat <- reformat_data(acs_cnt_2012_2016)
acs_cnt_2013_2017_reformat <- reformat_data(acs_cnt_2013_2017)

# merge the data row-wise
acs_cnt_merged_data_2011_2017 <- bind_rows(acs_cnt_2007_2011_reformat, acs_cnt_2008_2012_reformat,
                                           acs_cnt_2009_2013_reformat, acs_cnt_2010_2014_reformat,
                                           acs_cnt_2011_2015_reformat,acs_cnt_2012_2016_reformat,
                                           acs_cnt_2013_2017_reformat)



acs_cnt_1910_2010 <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/sas_output/load_dec_census/was_msas_1910_2010_20190115.csv"),
                              stringsAsFactors = F)

colnames(acs_cnt_1910_2010)

# rename the columns
acs_cnt_1910_2010 <- plyr::rename(acs_cnt_1910_2010, c("statefips" = "statefips",
                                                       "countyfips" = "countyfips",
                                                       "year" = "estimate_year",
                                                       "cv1" = "B01003_1_total",
                                                       "cv2" = "foreign-born_population",
                                                       "cv3" = "total_black_pop",
                                                       "cv4" = "total_other_races_pop",
                                                       "cv5" = "num_of_manuf_establishments",
                                                       "cv6" = "avg_num_of_manufacturing_wage_earners",
                                                       "cv7" = "total_manufacturing_wages",
                                                       "cv8" = "males > 25, no yrs of school",
                                                       "cv9" = "males > 25, 1-4 years elem school",
                                                       "cv10" = "males > 25, 5-6 years elem school",
                                                       "cv11" = "males > 25, 7 years elem school",
                                                       "cv12" = "males > 25, 8 years elem school",
                                                       "cv13" = "males > 25, 1-3 years hs",
                                                       "cv14" = "males > 25, 4 years hs",
                                                       "cv15" = "males > 25, 1-3 years college",
                                                       "cv16" = "males > 25, 4 years college",
                                                       "cv17" = "males > 25, 5+ years college",
                                                       "cv18" = "females > 25, no yrs of school",
                                                       "cv19" = "females > 25, 1-4 years elem school",
                                                       "cv20" = "females > 25, 5-6 years elem school",
                                                       "cv21" = "females > 25, 7 years elem school",
                                                       "cv22" = "females > 25, 8 years elem school",
                                                       "cv23" = "females > 25, 1-3 years hs",
                                                       "cv24" = "females > 25, 4 years hs",
                                                       "cv25" = "females > 25, 1-3 years college",
                                                       "cv26" = "females > 25, 4 years college",
                                                       "cv27" = "females > 25, 5+ years college",
                                                       "cv28" = "B25001_1_total",
                                                       "cv29" = "single_family_housing_units",
                                                       "cv30" = "B25077_1_median_value_(dollars)",
                                                       "cv31" = "B19013_1_median_household_income",
                                                       "cv58" = "population_65+",
                                                       "cv59" = "employed, various defns by year",
                                                       "cv60" = "gini_coefficient",
                                                       "cv87" = "land area (sq mi, ccdb, 1950; sq m, 2010)"))

colnames(acs_cnt_1910_2010)

# subset for required years
acs_cnt_1950_2010 <- acs_cnt_1910_2010 %>% filter(estimate_year>1949)

# subset for required columns
acs_cnt_1950_2010_subset <- acs_cnt_1950_2010 %>% select(estimate_year, statefips, countyfips, B01003_1_total, B25001_1_total,
                                                         `B25077_1_median_value_(dollars)`, B19013_1_median_household_income)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
acs_cnt_1950_2010_subset[['countyfips']] <- sapply(acs_cnt_1950_2010_subset[["countyfips"]], padzero)

acs_cnt_1950_2010_subset$estimate_year <- as.character(acs_cnt_1950_2010_subset$estimate_year)

head(acs_cnt_1950_2010_subset)


acs_cnt_merged_data_1950_2017 <- bind_rows(acs_cnt_1950_2010_subset, acs_cnt_merged_data_2011_2017)


acs_cnt_merged_data_1950_2017$B01001_1_total <- acs_cnt_merged_data_1950_2017$B01003_1_total

# save data for all years
write.csv(acs_cnt_merged_data_1950_2017,paste0(out_dir,dateo,"_acs_cnt_merged_data_1950_2017.csv"), row.names = F)
