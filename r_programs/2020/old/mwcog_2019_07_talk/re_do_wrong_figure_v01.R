#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2017_v02.py
# and tries to fix one wrong graph for the two-page summary
#
# july 9, 2019
#
# re_do_wrong_figure_v01.R
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
library(reshape2)
library(gridExtra)

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# change output directory for this
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/mwcog_2019_07_talk/")


# data and output directories
#data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
#out_dir_intro <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/introduction/")
#out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch01/")
#out_dir_ch03 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch03/")
#out_dir_appendix <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/appendix/")


# load the data
acs_cnt_2007_2011 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2007_2011_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2008_2012 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2008_2012_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2009_2013 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2009_2013_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2010_2014 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2010_2014_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2011_2015 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2011_2015_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2013_2017 <- read.csv(paste0(data_dir,"20190303_cnty_acs_2013_2017_absolute_values.csv"),stringsAsFactors = F)


#df <- acs_cnt_2013_2017

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
  
  #data %>% dplyr::select(matches('^B11003_7_|B11003_14_|B11003_20_')) %>% head(2)
  
  
  
  # get the percent of families with children
  data <- data %>% mutate('family_with_no_childern'=dplyr::select(.,matches('B11003_7_|B11003_14_|B11003_20_')) %>%
                            apply(1, sum, na.rm=TRUE)) %>%
    mutate("percent_families_with_children"=round((B11003_1_total-family_with_no_childern)/B11003_1_total*100,2))
  

  print(unique(data$year))
  
  # get different type of family households (single, 2 to 4 units)
  # because 2015 and 2017 have addintional column of Built 2010 or later we need to put it in else loop
  if(unique(data$year)<2015){
    data <- data %>% mutate('total_single_family_households_since_2000'= 
                     dplyr::select(.,matches('^B25127_4_|B25127_40_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_single_family_households_before_2000'=
        dplyr::select(.,matches('^B25127_11_|B25127_18_|B25127_25_|B25127_32_|B25127_47_|B25127_54_|B25127_61_|B25127_68_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_2_to_4_family_households_since_2000'=dplyr::select(.,matches('^B25127_5_|B25127_41_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_12_|B25127_19_|B25127_26_|B25127_33_|B25127_48_|B25127_55_|B25127_62_|B25127_69_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_5_to_19_family_households_since_2000'=dplyr::select(.,matches('^B25127_6_|B25127_42_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
       dplyr::select(.,matches('^B25127_13_|B25127_20_|B25127_27_|B25127_34_|B25127_49_|B25127_56_|B25127_63_|B25127_70_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_since_2000'=dplyr::select(.,matches('^B25127_7_|B25127_43_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_14_|B25127_21_|B25127_28_|B25127_35_|B25127_50_|B25127_57_|B25127_64_|B25127_71_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_since_2000'=dplyr::select(.,matches('^B25127_8_|B25127_44_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_15_|B25127_22_|B25127_29_|B25127_36_|B25127_51_|B25127_58_|B25127_65_|B25127_72_')) %>% apply(1, sum, na.rm=TRUE))
    
    
  }else{
    ## for year 2015-2017
    #data %>% dplyr::select(matches('^1, detached.*attached$|attached.1$|attached.6$|attached.7$')) %>% head(2)
    
    data <- data %>% mutate('total_single_family_households_since_2000'= 
                     dplyr::select(.,matches('^B25127_4_|B25127_11_|B25127_47_|B25127_54_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_single_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_18_|B25127_25_|B25127_32_|B25127_39_|B25127_61_|B25127_68_|B25127_75_|B25127_82_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_2_to_4_family_households_since_2000'=dplyr::select(.,matches('^B25127_5_|B25127_12_|B25127_48_|B25127_55_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_2_to_4_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_19_|B25127_26_|B25127_33_|B25127_40_|B25127_62_|B25127_69_|B25127_76_|B25127_83_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_5_to_19_family_households_since_2000'=dplyr::select(.,matches('^B25127_6_|B25127_13_|B25127_49_|B25127_56_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    data <- data %>% mutate('total_5_to_19_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_20_|B25127_27_|B25127_34_|B25127_41_|B25127_63_|B25127_70_|B25127_77_|B25127_84_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_since_2000'=dplyr::select(.,matches('^B25127_7_|B25127_14_|B25127_50_|B25127_57_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_20_to_49_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_21_|B25127_28_|B25127_35_|B25127_42_|B25127_64_|B25127_71_|B25127_78_|B25127_85_')) %>% apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_since_2000'=dplyr::select(.,matches('^B25127_8_|B25127_15_|B25127_51_|B25127_58_')) %>% 
                              apply(1, sum, na.rm=TRUE))
    
    
    data <- data %>% mutate('total_50_or_more_family_households_before_2000'=
                     dplyr::select(.,matches('^B25127_22_|B25127_29_|B25127_36_|B25127_43_|B25127_65_|B25127_72_|B25127_79_|B25127_86_')) %>% apply(1, sum, na.rm=TRUE))
    
    
  }
  
  
  
  # dplyr::select column with household income
  data_median_col <- data %>% dplyr::select(contains("B19013_1"))
  
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
  data_subset <- data %>% dplyr::select(STATE_FIPS, county_code, year, total_population, total_housing_units, median_housing_value,
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
acs_cnt_1950_2010_subset <- acs_cnt_1950_2010 %>% dplyr::select(statefips, countyfips, year, total_population, total_housing_units,
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


#############################################################################################################
axis_labs_col <- "#737373"
axis_text_size <- 50
#############################################################################################################


##################################### CHAPTER 2 ##############################################################


###################### P2.G2 Plot Bi directional Bar chart for new housing units and proportion of single family by county ###################################################

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
g.mid<-ggplot(df,aes(x=0.5,y=county_name_state_code))+
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
        axis.text = element_text(size = axis_text_size, colour = axis_labs_col),
 plot.margin = unit(c(6,-1,1,-1), "mm"))

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
        axis.text = element_text(size = 35, colour = axis_labs_col),
        legend.position="none",
        plot.title = element_text(size = 28, hjust = 0.5))+
  # plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() +
  coord_flip()


# plot the county vs total new units
g2 <- ggplot(data = df, aes(x = county_name_state_code, y = total_new_units,fill=group)) +
  xlab(NULL)+
  geom_bar(stat = "identity") +
  #scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  scale_y_continuous(labels = c(0,10,20,30,40,50), breaks = c(0,10000,20000,30000,40000,50000),limits = c(0,60000))+
  ggtitle("1,000s of New Housing Units") +
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        axis.text = element_text(size = 35, colour = axis_labs_col),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 28))+
        coord_flip()
  #plot.margin = unit(c(1,0,1,-1), "mm")) +
  


x_urban <- 24
x_suburban <- 19
x_exurban <- 14


y_pos_urban <- df$total_new_units[x_urban]
y_pos_suburban <- df$total_new_units[x_suburban]
y_pos_exurban <- df$total_new_units[x_exurban]

size_text <- 12

g2 <- g2+  annotate(geom="text", x=x_urban, y=y_pos_urban+8500, label="Urban", color="#045a8d", size=size_text)+
  annotate(geom="text", x=x_suburban, y=y_pos_suburban+10500, label="Suburban", color="#2b8cbe", size=size_text)+
  annotate(geom="text", x=x_exurban, y=y_pos_exurban+9000, label="Exurban", color="#74c476", size=size_text)

#g2
# join the two graphs with the county label graph

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

#gg.mid

#p1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(2.05/9,2.05/9,4.9/9))
p1 <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(2/9,2.2/9,5/9))
p1




# save the graph
ggsave(paste0(out_dir_ch02,"p2.g2_",dateo,"_acs_cnt_1950_2017_single_family_prop_and_new_housing_units.jpg"),
       plot = p1, dpi = 300, width = 25, height = 15, units = c("in"))

write.csv(df,paste0(out_dir_ch02,"p2.g2_",dateo,"_acs_cnt_1950_2017_single_family_prop_and_new_housing_units.csv"),row.names = F)

