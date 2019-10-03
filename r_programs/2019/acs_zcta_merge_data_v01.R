#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_zcta_2010_2016_v01.py
# to merge it together
# March 4, 2019
#
# acs_zcta_merge_data_v01.R
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
acs_zcta_2007_2011 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2007_2011.csv"),stringsAsFactors = F)
acs_zcta_2008_2012 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2008_2012.csv"),stringsAsFactors = F)
acs_zcta_2009_2013 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2009_2013.csv"),stringsAsFactors = F)
acs_zcta_2010_2014 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2010_2014.csv"),stringsAsFactors = F)
acs_zcta_2011_2015 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2011_2015.csv"),stringsAsFactors = F)
acs_zcta_2012_2016 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2012_2016.csv"),stringsAsFactors = F)
acs_zcta_2013_2017 <- read.csv(paste0(data_dir,file_date,"_acs_zcta_2013_2017.csv"),stringsAsFactors = F)


# write a function to reformat the data to bring it to county level and subset it for requried columns
reformat_data <- function(df) {
  
  data <- df

  data$FILETYPE <- as.character(data$FILETYPE)

  # get the year column
  data <- data %>% mutate(estimate_year=substr(FILETYPE,1,4))

  # reorder to make year as first column in dataset
  data <- data %>% select(estimate_year, everything())

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
acs_zcta_2007_2011_reformat <- reformat_data(acs_zcta_2007_2011)
acs_zcta_2008_2012_reformat <- reformat_data(acs_zcta_2008_2012)
acs_zcta_2009_2013_reformat <- reformat_data(acs_zcta_2009_2013)
acs_zcta_2010_2014_reformat <- reformat_data(acs_zcta_2010_2014)
acs_zcta_2011_2015_reformat <- reformat_data(acs_zcta_2011_2015)
acs_zcta_2012_2016_reformat <- reformat_data(acs_zcta_2012_2016)
acs_zcta_2013_2017_reformat <- reformat_data(acs_zcta_2013_2017)



# merge the data row-wise
# acs_zcta_merged_data <- bind_rows(acs_zcta_2012_2016_reformat, acs_zcta_2011_2015_reformat, acs_zcta_2010_2014_reformat,
#                                  acs_zcta_2009_2013_reformat, acs_zcta_2008_2012_reformat, acs_zcta_2007_2011_reformat)

acs_zcta_merged_data_2011_2016 <- bind_rows(acs_zcta_2007_2011_reformat, acs_zcta_2008_2012_reformat,
                                           acs_zcta_2009_2013_reformat, acs_zcta_2010_2014_reformat,
                                           acs_zcta_2011_2015_reformat,acs_zcta_2012_2016_reformat,
                                           acs_zcta_2013_2017_reformat)


# save data for all years
write.csv(acs_zcta_merged_data_2011_2016,paste0(out_dir,dateo,"_acs_zcta_merged_data_2011_2017.csv"), row.names = F)
