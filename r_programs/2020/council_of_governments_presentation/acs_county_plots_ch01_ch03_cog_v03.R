#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2017_v02.py
# and tries to make plots from it
# for presentation purposes
#
# March 8, 2019
# March 11, 2019
#  copied from
#  /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2019
#   acs_county_plots_ch01_ch03_v06.R
# March 12, 2019
# april 8, 2019  ## updating for AEI
#
# acs_county_plots_ch01_ch03_cog_v03.R
#
##############################################################################
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(ggrepel)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(stringr)

##############################################################################

rm(list=ls())

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# new output directory for presentation files
out_dir_pres <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/aei_presentation/"

# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
out_dir_intro <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/council_of_governments/")
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/council_of_governments/")
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/council_of_governments/")
out_dir_ch03 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/council_of_governments/")


# load the data
acs_cnt_2007_2011 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2007_2011_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2008_2012 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2008_2012_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2009_2013 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2009_2013_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2010_2014 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2010_2014_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2011_2015 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2011_2015_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2013_2017 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2013_2017_absolute_values.csv"),stringsAsFactors = F)

#df <- acs_cnt_2012_2017

# other universal settings
axis_labs_col <- "#737373"
axis_text_size <- 40

################ write a function to reformat the data to bring it to county level and subset it for requried columns  ################
reformat_subset_data <- function(df) {
  
  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))
  
  # remove unnecessary columns
  data <- data[-c(1,2),]
  
  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index
  
  # print the df to check if the format is appropriate
  print(head(data))
  
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
  print(str(data))
  
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
  names(data)[names(data)=="state_county_code_2"] <- "county_code"
  
  data$FILETYPE <- as.character(data$FILETYPE)
  
  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))
  
  #data %>% select(matches('^B11003_7_|B11003_14_|B11003_20_')) %>% head(2)
  
  
  
  # get the percent of families with children
  data <- data %>% mutate('family_with_no_childern'=select(.,matches('B11003_7_|B11003_14_|B11003_20_')) %>%
                            apply(1, sum, na.rm=TRUE)) %>%
    mutate("percent_families_with_children"=round((B11003_1_total-family_with_no_childern)/B11003_1_total*100,2))
  

  print(unique(data$year))
  
  # get different type of family households (single, 2 to 4 units)
  # because 2015 and 2017 have addintional column of Built 2010 or later we need to put it in else loop
  if(unique(data$year)<2015){
    data <- data %>% mutate('total_single_family_households_since_2000'= 
                     select(.,matches('^B25127_4_|B25127_40_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_single_family_households_before_2000'=
        select(.,matches('^B25127_11_|B25127_18_|B25127_25_|B25127_32_|B25127_47_|B25127_54_|B25127_61_|B25127_68_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_2_to_4_family_households_since_2000'=select(.,matches('^B25127_5_|B25127_41_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
                     select(.,matches('^B25127_12_|B25127_19_|B25127_26_|B25127_33_|B25127_48_|B25127_55_|B25127_62_|B25127_69_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_5_to_19_family_households_since_2000'=select(.,matches('^B25127_6_|B25127_42_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
       select(.,matches('^B25127_13_|B25127_20_|B25127_27_|B25127_34_|B25127_49_|B25127_56_|B25127_63_|B25127_70_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_since_2000'=select(.,matches('^B25127_7_|B25127_43_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
                     select(.,matches('^B25127_14_|B25127_21_|B25127_28_|B25127_35_|B25127_50_|B25127_57_|B25127_64_|B25127_71_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_since_2000'=select(.,matches('^B25127_8_|B25127_44_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
                     select(.,matches('^B25127_15_|B25127_22_|B25127_29_|B25127_36_|B25127_51_|B25127_58_|B25127_65_|B25127_72_')) %>% apply(1, sum, na.rm=TRUE))
    
    
  }else{
    ## for year 2015-2017
    #data %>% select(matches('^1, detached.*attached$|attached.1$|attached.6$|attached.7$')) %>% head(2)
    
    data <- data %>% mutate('total_single_family_households_since_2000'= 
                     select(.,matches('^B25127_4_|B25127_11_|B25127_47_|B25127_54_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_single_family_households_before_2000'=
                     select(.,matches('^B25127_18_|B25127_25_|B25127_32_|B25127_39_|B25127_61_|B25127_68_|B25127_75_|B25127_82_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_2_to_4_family_households_since_2000'=select(.,matches('^B25127_5_|B25127_12_|B25127_48_|B25127_55_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
                     select(.,matches('^B25127_19_|B25127_26_|B25127_33_|B25127_40_|B25127_62_|B25127_69_|B25127_76_|B25127_83_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_5_to_19_family_households_since_2000'=select(.,matches('^B25127_6_|B25127_13_|B25127_49_|B25127_56_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
                     select(.,matches('^B25127_20_|B25127_27_|B25127_34_|B25127_41_|B25127_63_|B25127_70_|B25127_77_|B25127_84_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_since_2000'=select(.,matches('^B25127_7_|B25127_14_|B25127_50_|B25127_57_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
                     select(.,matches('^B25127_21_|B25127_28_|B25127_35_|B25127_42_|B25127_64_|B25127_71_|B25127_78_|B25127_85_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_since_2000'=select(.,matches('^B25127_8_|B25127_15_|B25127_51_|B25127_58_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
                     select(.,matches('^B25127_22_|B25127_29_|B25127_36_|B25127_43_|B25127_65_|B25127_72_|B25127_79_|B25127_86_')) %>% apply(1, sum, na.rm=TRUE))
    
    
  }
  
  
  
  # select column with household income
  data_median_col <- data %>% select(contains("B19013_1"))
  
  # get name of household income column
  median_col_name <- colnames(data_median_col)
  
  print(median_col_name)
  
  # rename the median household income to get consistent name across years
  colnames(data)[which(colnames(data) == median_col_name)] <- 'median_household_income'
  
  # rename other columns to later merge with 1950-2010 data
  colnames(data)[which(colnames(data) == "B01003_1_total")] <- 'total_population'
  colnames(data)[which(colnames(data) == "B25001_1_total")] <- 'total_housing_units'
  colnames(data)[which(colnames(data) == "B25077_1_median_value_(dollars)")] <- 'median_housing_value'
  colnames(data)[which(colnames(data) == "B25003_2_owner_occupied")] <- 'total_owner_occupied'
  colnames(data)[which(colnames(data) == "B25003_3_renter_occupied")] <- 'total_renter_occupied'
  
  colnames(data)[which(colnames(data) == "B25071_1_median_gross_rent_as_a_percentage_of_household_income")] <- 'median_gross_rent_as_a_percentage_of_household_income'
  
  
  # subset for all the relevant columns
  data_subset <- data %>% select(STATE_FIPS, county_code, year, total_population, total_housing_units, median_housing_value,
                                 median_household_income, percent_families_with_children, county_name, state_name, state_code,
                                 total_single_family_households_since_2000,total_single_family_households_before_2000,
                                 total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
                                 total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
                                 total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
                                 total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000,
                                 total_owner_occupied,total_renter_occupied,median_gross_rent_as_a_percentage_of_household_income)
  
 return (data_subset)

}

# get the data for all years 2011 to 2017 with relevant columns by calling the function
acs_cnt_2007_2011_subset <- reformat_subset_data(acs_cnt_2007_2011)
acs_cnt_2008_2012_subset <- reformat_subset_data(acs_cnt_2008_2012)
acs_cnt_2009_2013_subset <- reformat_subset_data(acs_cnt_2009_2013)
acs_cnt_2010_2014_subset <- reformat_subset_data(acs_cnt_2010_2014)
acs_cnt_2011_2015_subset <- reformat_subset_data(acs_cnt_2011_2015)
acs_cnt_2012_2016_subset <- reformat_subset_data(acs_cnt_2012_2016)
acs_cnt_2013_2017_subset <- reformat_subset_data(acs_cnt_2013_2017)

# stack data for all the years together
acs_cnt_2011_2017 <- rbind(acs_cnt_2007_2011_subset,acs_cnt_2008_2012_subset, acs_cnt_2009_2013_subset, acs_cnt_2010_2014_subset,
                           acs_cnt_2011_2015_subset, acs_cnt_2012_2016_subset,acs_cnt_2013_2017_subset)



# load data for years before 2011
# dataset was created by program at following location
# /groups/brooksgrp/center_for_washington_area_studies/sas_programs/load_county_dec_cen/decyrs1910to2010v10.sas


acs_cnt_1910_2010 <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/sas_output/load_dec_census/was_msas_1910_2010_20190115.csv"),
                              stringsAsFactors = F)

colnames(acs_cnt_1910_2010)

# rename the columns
acs_cnt_1910_2010 <- plyr::rename(acs_cnt_1910_2010, c("statefips" = "statefips",
                                                       "countyfips" = "countyfips",
                                                       "year" = "year",
                                                       "cv1" = "total_population",
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
                                                       "cv28" = "total_housing_units",
                                                       "cv29" = "single_family_housing_units",
                                                       "cv30" = "median_housing_value",
                                                       "cv31" = "median_household_income",
                                                       "cv58" = "population_65+",
                                                       "cv59" = "employed, various defns by year",
                                                       "cv60" = "gini_coefficient",
                                                       "cv87" = "land area (sq mi, ccdb, 1950; sq m, 2010)"))

colnames(acs_cnt_1910_2010)

# subset for required years
acs_cnt_1950_2010 <- acs_cnt_1910_2010 %>% filter(year>1949)

# subset for required columns
acs_cnt_1950_2010_subset <- acs_cnt_1950_2010 %>% select(statefips, countyfips, year, total_population, total_housing_units,
                                                         median_housing_value, median_household_income)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
acs_cnt_1950_2010_subset[['countyfips']] <- sapply(acs_cnt_1950_2010_subset[["countyfips"]], padzero)

acs_cnt_1950_2010_subset$year <- as.character(acs_cnt_1950_2010_subset$year)

head(acs_cnt_1950_2010_subset)

# make column names same before stacking dataframes
colnames(acs_cnt_2011_2017)[1:7] <- colnames(acs_cnt_1950_2010_subset)[1:7]

# stack both dataframes to get pop and housing for year 1950-2017
acs_cnt_1950_2017 <- bind_rows(acs_cnt_1950_2010_subset, acs_cnt_2011_2017)

#write.csv(acs_cnt_1950_2017,paste0(out_dir,dateo,"_acs_cnt_merged_data_1950_2017.csv"))

acs_cnt_1950_2017 <- acs_cnt_1950_2017 %>% mutate("area_type"=ifelse((countyfips %in% c("001") & statefips %in% c("11"))|
                                                                       (countyfips %in% c("013","510") & statefips %in% c("51")),"Urban",
                                                                     ifelse((countyfips %in% c("033","031") & statefips %in% c("24"))|
                                                                              (countyfips %in% c("059","600","610") & statefips %in% c("51")),"Suburban",
                                                                            "Exurban")))

# replace NA with values of county, state and state code within groups
#https://stackoverflow.com/questions/31879390/replace-na-with-values-in-another-row-of-same-column-for-each-group-in-r
acs_cnt_1950_2017 <- acs_cnt_1950_2017 %>% group_by(statefips, countyfips) %>% mutate(county_name=unique(county_name[!is.na(county_name)]),
                                                                                      state_name=unique(state_name[!is.na(state_name)]),
                                                                                      state_code=unique(state_code[!is.na(state_code)]))



# convert tibble to dataframe
acs_cnt_1950_2017 <- acs_cnt_1950_2017 %>% mutate(county_name_state_code=str_c(county_name,", ",state_code)) %>% as.data.frame()

######################## Indexing by CPI Data #####################################################

# read the CPI data downloaded from US Bureau of Labor Department Website
cpi_data <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/data/consumer_price_index/2018/","CPI_data.csv"),
                     sep = "\t",stringsAsFactors = F)

# remove unnecessary columns
cpi_data$footnote_codes <- NULL

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# remove trailing white space from series id column
#https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace

cpi_data$series_id <- trim.trailing(cpi_data$series_id)

# filter for the desired series_id
cpi_data_all_areas_old_base <- cpi_data %>% filter(series_id=="CUUR0100AA0") 

# filter for desired period - here we have chosen M13 which is yearly average CPI
cpi_data_all_areas_old_base_avg_cpi <- cpi_data_all_areas_old_base %>% 
                                filter(period=="M13") %>% filter(!(year %in% c("2018"))) %>% 
                                    as.data.frame()

# rename the column 
colnames(cpi_data_all_areas_old_base_avg_cpi)[4] <- "cpi_index"

# take the CPI value of last row i.e. year 2017
cpi_2017 <- cpi_data_all_areas_old_base_avg_cpi$cpi_index[nrow(cpi_data_all_areas_old_base_avg_cpi)]

# index the CPI values by year 2017
cpi_data_all_areas_old_base_avg_cpi <- cpi_data_all_areas_old_base_avg_cpi %>% mutate(cpi_indexed_2017=round(cpi_index/cpi_2017,3))


cpi_data_all_areas_old_base_avg_cpi$year <- as.character(cpi_data_all_areas_old_base_avg_cpi$year)

acs_cnt_1950_2017 <- left_join(acs_cnt_1950_2017,cpi_data_all_areas_old_base_avg_cpi,by="year")

acs_cnt_1950_2017 <- acs_cnt_1950_2017 %>% mutate_at(c("median_housing_value","median_household_income"),funs(round(./cpi_indexed_2017)))


acs_cnt_1950_2017$county_name_state_code <- ifelse(acs_cnt_1950_2017$county_name_state_code=="District of Columbia, DC","Washington, DC",
                                                   acs_cnt_1950_2017$county_name_state_code)

##################################### CHAPTER 1 ##############################################################

###################### P1.G1. Plot Housing Units Area Wise ###################################################

p1g1 <- TRUE

if(p1g1){


} # close -if- for p1g1

###################### P1.G2. Plot Housing Units Area Wise - County Level ###################################################

p1g2 <- TRUE

if(p1g2){

acs_cnt_2017_subset <- acs_cnt_1950_2017 %>%
  filter(year=='2017') %>%
  dplyr::select(county_name_state_code,total_single_family_households_since_2000,total_single_family_households_before_2000,
         total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
         total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
         total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
         total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000,area_type) %>%
  as.data.frame()


#write.csv(acs_cnt_2016_subset,"p1.g2.acs_cnt_2016_subset.csv",row.names = F)
# melt the dataframe
acs_cnt_2017_subset_melt <- melt(acs_cnt_2017_subset, id.var=c("county_name_state_code","area_type"))

# create a column unit_type to assign new or exsting for housing units
acs_cnt_2017_subset_melt <- acs_cnt_2017_subset_melt %>%
  mutate("unit_type" = ifelse(grepl('since_2000$', variable),"New","Existing"))


acs_cnt_2017_area_wise_new_ext_housing_melt <- acs_cnt_2017_subset_melt %>%
  group_by(county_name_state_code,area_type,unit_type) %>%
  summarise("total_housing_units"=sum(value)) %>% as.data.frame()



# filter for new housing units
acs_cnt_2017_area_wise_new_housing_melt <- acs_cnt_2017_area_wise_new_ext_housing_melt %>% filter(unit_type=="New")


df_exurban_city <- acs_cnt_2017_area_wise_new_housing_melt %>% filter(area_type=="Exurban" & str_detect(county_name_state_code, 'city'))

df_exurban_city_agg <- df_exurban_city %>% group_by(area_type, unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_exurban_city_data <- cbind(county_name_state_code="independent cities of\nFredericksburg, Manassas & Manassas Park",df_exurban_city_agg)

df_suburban_city <- acs_cnt_2017_area_wise_new_housing_melt %>% filter(area_type=="Suburban" & str_detect(county_name_state_code, 'city'))

df_suburban_city_agg <- df_suburban_city %>% group_by(area_type, unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_suburban_city_data <- cbind(county_name_state_code="Fairfax & Falls Church cities",df_suburban_city_agg)


df_cnt_2017_area_wise_new_housing_melt <- rbind(acs_cnt_2017_area_wise_new_housing_melt,df_exurban_city_data,df_suburban_city_data)

df_cnt_2017_area_wise_new_housing_melt <- df_cnt_2017_area_wise_new_housing_melt %>%
  filter(!((area_type=="Exurban" | area_type=="Suburban") & str_detect(county_name_state_code, 'city')))


ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  
  
  category.start <- c("#f7fcf5","#deebf7","#6baed6") # Set the top of the colour pallete
  category.end  <- c("#74c476","#2b8cbe","#045a8d") # set the bottom #74c476
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data
df <- df_cnt_2017_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


df <- df %>% arrange(area_type,total_housing_units)

df$group <- factor(df$group, levels = df$group)

exclude_counties <- c("Fairfax & Falls Church cities","Rappahannock County, VA","Clarke County, VA","Warren County, V
A","Culpeper County, VA",
                      "independent cities of\nFredericksburg, Manassas & Manassas Park")




df1 <- df %>% filter(!(county_name_state_code %in% exclude_counties))

d1 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Suburban')
total_suburban <- d1$`sum(total_housing_units)`

d2 <- df %>% filter(county_name_state_code=="Fairfax & Falls Church cities")
total_farifax_falls <- d2$total_housing_units

fairfax_fall_church_arrow <- total_suburban-round(total_farifax_falls/2)


d3 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Exurban')
total_exburban <- d3$`sum(total_housing_units)`


d4 <- df %>% filter(county_name_state_code %in% c("Rappahannock County, VA","Clarke County, VA","Warren County, VA","
Culpeper County, VA"))
total_rappanhock <- sum(d4$total_housing_units)-10

rappanhock_arrow <- total_exburban-round(total_rappanhock/2)

rappanhock_low_seg <- total_exburban-total_rappanhock

d5 <- df %>% filter(county_name_state_code=="independent cities of\nFredericksburg, Manassas & Manassas Park")
total_indep_cities <- d5$total_housing_units

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))

indep_cities_arrow <- rappanhock_low_seg-round(total_indep_cities/2)

indep_cities_text <- indep_cities_arrow-3000

anno_text_size <- 7


# clean up names for presentation
df1$county_name_state_code <- 
  ifelse(df1$county_name_state_code == "Prince George's County, MD",
     "Prince George's C., MD",
     df1$county_name_state_code)
df1$county_name_state_code <- 
  ifelse(df1$county_name_state_code == "Montgomery County, MD",
     "Montgomery Cnty, MD",
     df1$county_name_state_code)
df1$county_name_state_code <- 
  ifelse(df1$county_name_state_code == "Prince William County, VA",
     "Prince William Cnty, VA",
     df1$county_name_state_code)
df1$county_name_state_code <- 
  ifelse(df1$county_name_state_code == "Spotsylvania County, VA",
     "Spotsylvania Cnty, VA",
     df1$county_name_state_code)



### calculate placement of arrows and text end here

# plot the graph
p1 <- ggplot(df, aes(x = area_type, y = total_housing_units, fill =  group)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 4))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=15),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  geom_text(data=df1,aes(label = county_name_state_code), # << move each label down by 1 unit
            position = position_stack(vjust=0.5), #vjust =2,
            color = "white", size = 6) +
  annotate(geom="text", x=0.88, y=fairfax_fall_church_arrow + 10000, label="Fairfax &", 
  	   color="#525252", size=anno_text_size) +
  annotate(geom="text", x=0.88, y=fairfax_fall_church_arrow, label="Falls Church cities", 
  	   color="#525252", size=anno_text_size)

# save the plot
ggsave(paste0(out_dir_pres,"p1_g2_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 11, height = 8, units = c("in"))

df$group <- NULL

write.csv(df,paste0(out_dir_pres,"p1_g2_",dateo,".csv"),row.names = F)

} # close p1.g2 if statement

###################### P1.G3. Plot Housing Units Percent change from 200 to 2017 vs Median Housing Value 2000 ###################################################

p1g3 <- TRUE

if(p1g3){

# filter the data for years 2000 and 2017
acs_cnt_2000_2017_comp <- acs_cnt_1950_2017 %>% filter(year=="2017")

acs_cnt_2000_2017_comp %>% select(matches("*since_2000$"))


acs_cnt_2000_2017_comp <- acs_cnt_2000_2017_comp %>% mutate('2000 & After'=select(.,matches('*since_2000$')) %>% apply(1,sum,na.rm=TRUE))

acs_cnt_2000_2017_comp <- acs_cnt_2000_2017_comp %>% mutate('Before 2000'=select(.,matches('*before_2000$')) %>% apply(1,sum,na.rm=TRUE))

acs_cnt_2000_2017_comp <- acs_cnt_2000_2017_comp %>% mutate('housing_units_pct_change_from_2000'=round((`2000 & After`/`Before 2000`*100),2))

acs_cnt_2000 <- acs_cnt_1950_2017 %>% filter(year=="2000") %>% 
                      mutate(median_housing_value_2000=median_housing_value) %>% select(median_housing_value_2000)


acs_cnt_2000_2017_comp <- cbind(acs_cnt_2000_2017_comp,acs_cnt_2000)

acs_cnt_2000_2017_comp_pct_2000_wo_Loudoun <- subset(acs_cnt_2000_2017_comp, county_name!="Loudoun County")

## decorate lm object with a new class lm_right
lm_right <- function(formula,data,...){
  mod <- lm(formula,data)
  class(mod) <- c('lm_right',class(mod))
  mod
}

## decorate lm object with a new class lm_left
lm_left <- function(formula,data,...){
  mod <- lm(formula,data)
  class(mod) <- c('lm_left',class(mod))
  mod
}

# plot the graph with all conties
p1 <- ggplot(data = acs_cnt_2000_2017_comp , aes(x = median_housing_value_2000, y = housing_units_pct_change_from_2000)) +
  geom_text_repel(aes(label=county_name, colour=factor(area_type)), size=5,fontface = "bold", segment.size = 0)+
  scale_x_continuous(labels = comma,limits= c(0, 450000), breaks = c(seq(0,450000,100000))) +
  labs(x = "median housing value, 2000", y="% change, no. hsng units, 2000 to 2017")+
  scale_color_manual(values = c("Urban" = "#1f78b4","Suburban"="#a6cee3","Exurban"="#b2df8a"))+
  annotate(geom="text", x=330000, y=29, label="all counties", color="#bdbdbd", size=7)+
  annotate(geom="text", x=30000, y=55, label="without loudoun", color="#525252", size=7)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10)))+
  stat_smooth(method="lm_right", fullrange=TRUE,col='#bdbdbd', se=FALSE) +
  stat_smooth(data=acs_cnt_2000_2017_comp_pct_2000_wo_Loudoun,method="lm_left", fullrange=TRUE,col='#525252',se=FALSE) +
  annotate(geom="text", x=425000, y=90, label="Urban", color="#045a8d", size=10)+
  annotate(geom="text", x=425000, y=86, label="Suburban", color="#2b8cbe", size=10)+
  annotate(geom="text", x=425000, y=82, label="Exurban", color="#74c476", size=10)


# save the graph
ggsave(paste0(out_dir_pres,"p1_g3_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 11, height = 8, units = c("in"))

write.csv(acs_cnt_2000_2017_comp,
          paste0(out_dir_pres,"p1_g3_",dateo,".csv"),row.names = F)

} # close p1g3 -if-


##################################### CHAPTER 2 ##############################################################

###################### P2.G1 Plot Housing Units by unit type and count ###################################################

p2g1 <- TRUE

if(p2g1){

# filter for the relevant columns
acs_cnt_2017_new_ext_housing <- acs_cnt_1950_2017 %>%
                  filter(year=='2017') %>%
                  select(county_name_state_code,area_type,total_single_family_households_since_2000,total_single_family_households_before_2000,
                         total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
                         total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
                         total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
                         total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000) %>%
                  as.data.frame()

# melt the dataframe
acs_cnt_2017_new_ext_housing_melt <- melt(acs_cnt_2017_new_ext_housing, id.var=c("county_name_state_code","area_type"))


# create a column unit_type to assign new or exsting for housing units

acs_cnt_2017_new_ext_housing_melt <- acs_cnt_2017_new_ext_housing_melt %>%
                                  mutate(Unit_Type = ifelse(grepl('since_2000$', variable),"2000 & After","Before 2000"))


acs_cnt_2017_new_ext_housing_melt <- acs_cnt_2017_new_ext_housing_melt %>%
  mutate("Units_Count"=ifelse(grepl('*single*', variable),"1 Unit",
                              ifelse(grepl('*2_to_4*', variable),"2 to 4 Units",ifelse(grepl('*5_to_19*', variable),"5 to 19 Units",
                                                                                       ifelse(grepl('*20_to_49*', variable),"20 to 49 Units","More than 50 Units")))))


acs_cnt_2017_new_ext_housing_melt$Units_Count <- factor(acs_cnt_2017_new_ext_housing_melt$Units_Count,
                                                        levels=c("More than 50 Units","20 to 49 Units","5 to 19 Units","2 to 4 Units","1 Unit"))


acs_cnt_2017_new_ext_housing_melt_rel_freq <- acs_cnt_2017_new_ext_housing_melt %>%
  group_by(Unit_Type, Units_Count) %>%
  summarise(sum_units=sum(value)) %>%
  mutate(unit_freq=round(sum_units/sum(sum_units)*100,2))


ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  #unit_col <- brewer.pal(5, "Purples")
  
  #category.start <- c("#edf8fb","#edf8fb") # Set the top of the colour pallete
  category.start <- c("#be6ae2","#be6ae2") # Set the top of the colour pallete
  category.end  <- c("#810f7c","#810f7c") # set the bottom #74c476
  
  
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data
df <- acs_cnt_2017_new_ext_housing_melt_rel_freq
df$group <- paste0(df$Unit_Type, "-", df$Units_Count, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "Unit_Type", "Units_Count")

df$Unit_Type <- factor(df$Unit_Type,levels = c("Before 2000","2000 & After"))

df$group <- factor(df$group, levels = c( "Before 2000-More than 50 Units","Before 2000-20 to 49 Units",  "Before 2000-5 to 19 Units", 
                                         "Before 2000-2 to 4 Units", "Before 2000-1 Unit", "2000 & After-More than 50 Units",
                                         "2000 & After-20 to 49 Units","2000 & After-5 to 19 Units","2000 & After-2 to 4 Units","2000 & After-1 Unit"))

# plot the graph
p1 <- ggplot(df, aes(x = Unit_Type, y = unit_freq, fill = group)) +
  geom_bar(stat = "identity")+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  scale_shape_manual(values = c(16, 21)) +
  coord_flip() +
  labs(x = "", y = "share of housing units", colour = "Parameter")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  geom_text(data = subset(df, Unit_Type=="Before 2000"),
            aes(label = Units_Count), # << move each label down by 1 unit
            position = position_stack(vjust=0.5),
            color = "white", size = 5)

table(df$Units_Count)

col <- "white"
siz <- 10

p2 <- ggplot(df, aes(x = Units_Count, y = unit_freq, fill = Unit_Type)) +
  geom_bar(position = "dodge", stat = "identity")+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  scale_shape_manual(values = c(16, 21)) +
  coord_flip() +
  labs(x = "", y = "", colour = "Parameter")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm")) +
  annotate(geom="text", x=5.22, y=35, label="Built 2000 & after", color=col, size=siz)+
  annotate(geom="text", x=4.78, y=35, label="Built before 2000", color=col, size=siz)


# save the graph
ggsave(paste0(out_dir_pres,"p2_g1_",dateo,".jpg"),
       plot = p2, dpi = 300, width = 16, height = 11, units = c("in"))

write.csv(df,paste0(out_dir_pres,"p2_g1_",dateo,".csv"),row.names = F)
} # close p2g1 graph

###################### P2.G2 Plot Bi directional Bar chart for new housing units and proportion of single family by county ###################################################

p2g2 <- TRUE

if(p2g2){

# subset the data for New housing units
acs_cnt_2017_new_housing_melt <- acs_cnt_2017_new_ext_housing_melt %>% filter(Unit_Type=='2000 & After') %>% as.data.frame()

# fin the total new units for each county
acs_cnt_2017_new_housing_melt <- acs_cnt_2017_new_housing_melt %>%
  group_by(county_name_state_code) %>%
  mutate(total_new_units=sum(value))

# find the single family proportion
acs_cnt_2017_new_housing_melt <- acs_cnt_2017_new_housing_melt %>%
  filter(Units_Count=='1 Unit') %>%
  mutate(single_family_prop=round(value/total_new_units*100,2)) %>%
  as.data.frame()

# susbset for the relevant columns
df <- acs_cnt_2017_new_housing_melt %>%  select(county_name_state_code, area_type, total_new_units, single_family_prop)

df <- df %>% arrange(area_type,total_new_units)


ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  category.start <- c("#74c476","#2b8cbe","#045a8d") # Set the top of the colour pallete
  category.end  <- c("#74c476","#2b8cbe","#045a8d") # set the bottom #74c476
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data


#df <- df_cnt_2017_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")



df$county_name_state_code <- factor(df$county_name_state_code,levels = df$county_name_state_code)

df$group <- factor(df$group, levels = df$group)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))

county_name_levels <- levels(factor(df$county_name_state_code))

df$county_name_state_code <- as.character(df$county_name_state_code)

df$county_name_state_code <-
 ifelse(df$county_name_state_code == "Prince George's County, MD",
         "Prince George's Cnty, MD",
         df$county_name_state_code)

county_name_levels <-ifelse(county_name_levels == "Prince George's County, MD", "Prince George's Cnty, MD",county_name_levels)

df$county_name_state_code <- factor(df$county_name_state_code,levels = county_name_levels)

#p1+coord_flip()
# create a grid with labels for counties
g.mid<-ggplot(df,aes(x=1,y=county_name_state_code))+
  geom_text(aes(label=county_name_state_code),size=10,color="#525252")+
  #geom_segment(aes(x=0.94,xend=0.96,yend=county_name))+
  #geom_segment(aes(x=1.04,xend=1.065,yend=county_name))+
  ggtitle("")+
  ylab(NULL)+
  #  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        legend.position="none",
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        axis.text = element_text(size = 25))
# plot.margin = unit(c(1,-1,1,-1), "mm"))

g.mid


# plot the county vs single family proportion
g1 <- ggplot(data = df, aes(x = county_name_state_code, y = single_family_prop, fill=group)) +
  geom_bar(stat = "identity") +
  ggtitle("Share Single Family") +
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 30),
        legend.position="none",
        plot.title = element_text(size = 30, hjust = 0.5))+
  # plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() +
  coord_flip()

g1
# plot the county vs total new units
g2 <- ggplot(data = df, aes(x = county_name_state_code, y = total_new_units,fill=group)) +
  xlab(NULL)+
  geom_bar(stat = "identity") +
  #scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  scale_y_continuous(labels = c(0,10,20,30,40,50), breaks = c(0,10000,20000,30000,40000,50000),limits = c(0,60000))+
  ggtitle("10,000s of New Housing Units") +
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.text = element_text(size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 30))+
  #plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()



g2

# join the two graphs with the county label graph
library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

gg.mid

#p1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(2.05/9,2.05/9,4.9/9))
p1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(2.05/9,2.75/9,4.2/9))
p1

# save the graph
ggsave(paste0(out_dir_pres,"p2_g2_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

write.csv(df,paste0(out_dir_pres,"p2.g2_",dateo,".csv"),row.names = F)

} # close p2g2 graph

###################### P2.G3 Plot Owner Renter Occupancy Area Wise ###################################################

p2g3 <- FALSE

if(p2g3){

# get total housing units grouped by year and area
acs_cnt_1950_2017_area_wise_owner_renter_occupancy <- acs_cnt_1950_2017 %>%
                                                        filter(year=="2017") %>%
                                                        group_by(area_type) %>%
                                                        summarise("Owner occupied" = sum(total_owner_occupied, na.rm = T),
                                                                  "Renter occupied"=sum(total_renter_occupied, na.rm = T)) %>%
                                                        as.data.frame()


# melt the dataframe
acs_cnt_1950_2017_area_wise_owner_renter_occupancy_melt <- melt(acs_cnt_1950_2017_area_wise_owner_renter_occupancy, id.var="area_type")

df <- acs_cnt_1950_2017_area_wise_owner_renter_occupancy_melt
df$group <- paste0(df$area_type, "-", df$variable, sep = "")

new_levels <- c("Exurban-Owner occupied", "Exurban-Renter occupied", "Suburban-Owner occupied", "Suburban-Renter occupied",
                "Urban-Owner occupied", "Urban-Renter occupied" )


df$group <- factor(df$group,new_levels)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))
#df$county_name <- NULL

# Build the colour pallete
colours <- c("#a1d99b","#74c476","#9ecae1","#6baed6","#3182bd","#1f78b4")



# plot the graph
p1 <- ggplot(df, aes(x = area_type, y = value, fill = group)) +
  geom_bar(stat = "identity", size=10, color="#525252")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  scale_fill_manual("Subject", values=colours)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none")+
  geom_text(aes(label = variable), # << move each label down by 1 unit
            position = position_stack(vjust=0.5), #vjust =2,
            color = "white", size = 10)


p1


# save the plot
ggsave(paste0(out_dir_ch02,"p2.g3_",dateo,"_acs_cnt_1950_2017_Owner_Renter_occupancy_area_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

write.csv(df,paste0(out_dir_ch02,"p2.g3_",dateo,"_acs_cnt_1950_2017_Owner_Renter_occupancy_area_wise.csv"),row.names = F)

} # end p2g3

##################################### CHAPTER 3 ##############################################################

###################### P3.G1 Timeline graph for median housing value and median housing income ###################################################

p3g1 <- TRUE

if(p3g1){

metro_value_income <- read.csv(paste0(data_dir,"metro_value_income.csv"),stringsAsFactors = F)

# # get the weighted mean (weighted by population) median housing value and median housing income for all years
acs_cnt_1950_2017_median_housing_value_income <- acs_cnt_1950_2017 %>%
                                                             group_by(year) %>%
              summarise(median_housing_value_yr = weighted.mean(median_housing_value, total_population, na.rm = T),
                    median_household_income_yr = weighted.mean(median_household_income,total_population, na.rm = T))

# make the year column numeric
acs_cnt_1950_2017_median_housing_value_income$year <- as.numeric(acs_cnt_1950_2017_median_housing_value_income$year)

df <- na.omit(acs_cnt_1950_2017_median_housing_value_income)

#df <- df %>% filter(year %in% c(1980,1990,2000,2011,2017))

df <- df %>% filter(year %in% c(1980,1990,2000))

head(metro_value_income)

colnames(metro_value_income)[6:7] <- c("median_housing_value_yr","median_household_income_yr")

metro_value_income <- metro_value_income %>% select(year,median_housing_value_yr, median_household_income_yr)

df_all_years <- rbind(df,metro_value_income)


col_median_hh <- "#8c96c6"
col_median_val <- "#810f7c"

# plot the graph
p1 <- ggplot(df_all_years, aes(x=year)) +
  geom_line(aes(y=median_housing_value_yr,size=0.1, color="median home value")) +
  geom_line(aes(y=median_household_income_yr,size=0.1, color="median household income")) +
  scale_y_continuous(labels = comma, breaks = c(0,100000,200000,300000,400000,500000),limits = c(0,560000))+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2017), 
                     labels = paste0(c("1980", "1990", "2000", "2010", "2017")))+
  scale_colour_manual(values = c(col_median_val,col_median_hh))+
  labs(y = "", x = "", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  annotate(geom="text", x=1995, y=450000, label="Median home value", color=col_median_val, size=12)+
  annotate(geom="text", x=1998, y=150000, label="Median household income", color=col_median_hh, size=12)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = axis_text_size, colour = axis_labs_col),
        axis.title.x = element_text(size = axis_text_size, colour = axis_labs_col),
        axis.title.y = element_text(size = axis_text_size, colour = axis_labs_col),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))



# save the graph
ggsave(paste0(out_dir_pres,"p3_g1_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

# save the data
write.csv(metro_value_income,paste0(out_dir_pres,"p3_g1_",dateo,".csv"),row.names = F)

}

###################### P3.G2 Bar graph for median housing value and median housing income 2017 at county level ###################################################

p3g2 <- TRUE

if(p3g2){

acs_cnt_2017 <- acs_cnt_1950_2017 %>% filter(year=="2017") %>%
  mutate("median_housing_value_by_median_hh_income"=round(median_housing_value/median_household_income,2))


# provide column names for which we want absolute value plot on county level
col_vec <- c("median_housing_value_by_median_hh_income")


df <- acs_cnt_2017

ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  
  category.start <- c("#74c476","#2b8cbe","#045a8d") # Set the top of the colour pallete
  #category.end  <- c("#00441b","#2b8cbe","#045a8d") # set the bottom #74c476
  category.end  <- c("#74c476","#2b8cbe","#045a8d") # set the bottom #74c476
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data
df <- df %>% arrange(area_type,median_housing_value_by_median_hh_income)

#df <- df_cnt_2017_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


df$county_name_state_code <- factor(df$county_name_state_code,levels = df$county_name_state_code)

df$group <- factor(df$group, levels = df$group)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))


p <- ggplot(df,aes(x = county_name_state_code, y = median_housing_value_by_median_hh_income,fill = group)) +
  geom_bar(stat = "identity")+
  scale_fill_manual("Subject", values=colours)+
  #geom_text(aes(label=median_housing_value_by_median_hh_income), hjust=-0.1, color="#525252", size=5)+
  labs(x = "", y = "", colour = "Parameter")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        legend.position = "none",
        title = element_text(size = 12.5),
        #axis.line.y = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 40),
        panel.grid = element_blank(), panel.border = element_blank())

# make the barplot horizontal
p1 <- p + coord_flip()

p1

ggsave(paste0(out_dir_pres,"p3_g2_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

write.csv(df,paste0(out_dir_pres,"p3.g2_",dateo,".csv"),row.names = F)

} # end of p3g2


###################### P3.B1 Bar graph for renters burden area and race wise ######################

p3b1 <- FALSE

if(p3b1){

renters_burden <- read.csv(paste0(data_dir,"rent_burdens.csv"),stringsAsFactors = F)

renters_burden$area_type <- factor(renters_burden$area_type, levels = c("Urban","Suburban","Exurban"))

renters_burden_melt <- melt(renters_burden,id.vars = "area_type")


ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  
  
  category.start <- c("#74c476","#2b8cbe","#045a8d") # Set the top of the colour pallete
  category.end  <- c("#74c476","#2b8cbe","#045a8d") # set the bottom #74c476
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data
df <- renters_burden_melt
df$group <- paste0(df$area_type, "-", df$variable, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "variable")


df <- df %>% arrange(area_type)

#df$group <- factor(df$group, levels = df$group)

#df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))
#https://stackoverflow.com/questions/38931194/warning-when-defining-factor-duplicated-levels-in-factors-are-deprecated

df$variable <- factor(df$variable, levels = rev(unique(df$variable)))

p <- ggplot(df, aes(x = variable, y = value, fill =  group)) +
  geom_bar(position = "dodge", stat = "identity")+
  #scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 4))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        #axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=15),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))#+
 
#p1
#+
  # geom_text(data=df1,aes(label = county_name_state_code), # << move each label down by 1 unit
  #           position = position_stack(vjust=0.5), #vjust =2,
  #           color = "white", size = 6)


siz <- 8
col <- "white"


p1 <- p+coord_flip()+ annotate(geom="text", x=4, y=0.2, label="Suburban", color=col, size=siz)+
  annotate(geom="text", x=4.3, y=0.2, label="Urban", color=col, size=siz)+
  annotate(geom="text", x=3.7, y=0.2, label="Exurban", color=col, size=siz)

p1
# geom_text(data = subset(df, variable=="White"), #angle=90,
#           aes(label = area_type), # << move each label down by 1 unit
#           position = position_dodge(0.9), hjust=5, #vjust=-0.5,
#           color = "white", size = 8)
  

# save the plot
ggsave(paste0(out_dir_ch03,"p3.b1_",dateo,"_acs_cnt_1950_2017_renters_burden_area_race_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

df$group <- NULL

write.csv(df,paste0(out_dir_ch03,"p3.b1_",dateo,"_acs_cnt_1950_2017_renters_burden_area_race_wise.csv"),row.names = F)
} # end of p3b1

###################### P3.G3 Bar graph for median gross rent as a percentage of household income ######################

p3g3 <- TRUE

if(p3g3){
acs_cnt_2017 <- acs_cnt_1950_2017 %>% filter(year=="2017")


# provide column names for which we want absolute value plot on county level
col_vec <- c("median_gross_rent_as_a_percentage_of_household_income")


df <- acs_cnt_2017

ColourPalleteMulti <- function(df, group, subgroup){
  
  # Find how many colour categories to create and the number of colours in each
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~" )), df, function(x) length(unique(x)))
  print(categories)
  
  
  
  category.start <- c("#74c476","#2b8cbe","#045a8d") # Set the top of the colour pallete
  #category.end  <- c("#00441b","#2b8cbe","#045a8d") # set the bottom #74c476
  category.end  <- c("#74c476","#2b8cbe","#045a8d") # set the bottom #74c476
  
  print(category.start)
  print(category.end)
  
  # Build Colour pallette
  colours <- unlist(lapply(1:nrow(categories),
                           function(i){
                             colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])}))
  
  print(colours)
  return(colours)
}

# Create data
df <- df %>% arrange(area_type,median_gross_rent_as_a_percentage_of_household_income)

#df <- df_cnt_2017_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


df$county_name_state_code <- factor(df$county_name_state_code,levels = df$county_name_state_code)

df$group <- factor(df$group, levels = df$group)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))


p <- ggplot(df,aes(x = county_name_state_code, y = median_gross_rent_as_a_percentage_of_household_income, fill = group)) +
  geom_bar(stat = "identity")+
  scale_fill_manual("Subject", values=colours)+
  #geom_text(aes(label=median_housing_value_by_median_hh_income), hjust=-0.1, color="#525252", size=5)+
  labs(x = "", y = "", colour = "Parameter")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        legend.position = "none",
        title = element_text(size = 12.5),
        #axis.line.y = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 12.5),
        panel.grid = element_blank(), panel.border = element_blank())

# make the barplot horizontal
p1 <- p + coord_flip()

p1

ggsave(paste0(out_dir_pres,"p3_g3_",dateo,".jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

write.csv(df,paste0(out_dir_pres,"p3_g3_",dateo,".csv"),row.names = F)


} # end of p3g3
