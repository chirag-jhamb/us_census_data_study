#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2017_v02.py
# and tries to make plots from it
# for presentation purposes
#
# April 10, 2019
#
# acs_county_plots_ch01_ch03_v12_sr.R
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

# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch01/")


# load the data
acs_cnt_2005_2009 <- read.csv(paste0(data_dir,"20190404_cnty_acs_2005_2009_absolute_values.csv"),stringsAsFactors = F)



#df <- acs_cnt_2005_2009

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
  # data <- data %>% mutate('family_with_no_childern'=dplyr::select(.,matches('B11003_7_|B11003_14_|B11003_20_')) %>%
  #                           apply(1, sum, na.rm=TRUE)) %>%
  #   mutate("percent_families_with_children"=round((B11003_1_total-family_with_no_childern)/B11003_1_total*100,2))
  # 

  print(unique(data$year))
  
  # get different type of family households (single, 2 to 4 units)
  # because 2015 and 2017 have addintional column of Built 2010 or later we need to put it in else loop
  if(unique(data$year)<2015 & unique(data$year)>2010){
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
    
    
  }
  
  if (unique(data$year)>2014){
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
  
  if(unique(data$year)<2011){
    data <- data %>% mutate('total_2000_or_later'=dplyr::select(.,matches('^B25036_3_|B25036_4_|B25036_13_|B25036_14_')) %>% apply(1, sum, na.rm=TRUE))
    
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
  
  if(unique(data$year)>2010){
  # subset for all the relevant columns
  data_subset <- data %>% dplyr::select(STATE_FIPS, county_code, year, total_population, total_housing_units, median_housing_value,
                                 median_household_income, county_name, state_name, state_code,
                                 total_single_family_households_since_2000,total_single_family_households_before_2000,
                                 total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
                                 total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
                                 total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
                                 total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000,
                                 total_owner_occupied,total_renter_occupied,median_gross_rent_as_a_percentage_of_household_income)
  
  }else{
    
    data_subset <- data %>% dplyr::select(STATE_FIPS, county_code, year, total_population, total_housing_units, median_housing_value,
                                          median_household_income, county_name, state_name, state_code,total_2000_or_later,
                                          total_owner_occupied,total_renter_occupied,median_gross_rent_as_a_percentage_of_household_income)
    
  }
  
 return (data_subset)

}

# get the data for 2009 with relevant columns by calling the function
acs_cnt_2005_2009_subset <- reformat_subset_data(acs_cnt_2005_2009)


#############################################################################################################
axis_labs_col <- "#737373"
axis_text_size <- 50
#############################################################################################################


############################### For 2009 #################################################################################
###################### SR1.G1. Plot Housing Units Area Wise - County Level ###############################################

colnames(acs_cnt_2005_2009_subset)[which(colnames(acs_cnt_2005_2009_subset)=='STATE_FIPS')] <- 'statefips'
colnames(acs_cnt_2005_2009_subset)[which(colnames(acs_cnt_2005_2009_subset)=='county_code')] <- 'countyfips'

acs_cnt_2005_2009_subset <- acs_cnt_2005_2009_subset %>% mutate("area_type"=ifelse((countyfips %in% c("001") & statefips %in% c("11"))|
                                                                       (countyfips %in% c("013","510") & statefips %in% c("51")),"Urban",
                                                                     ifelse((countyfips %in% c("033","031") & statefips %in% c("24"))|
                                                                              (countyfips %in% c("059","600","610") & statefips %in% c("51")),"Suburban",
                                                                            "Exurban")))

# replace NA with values of county, state and state code within groups
#https://stackoverflow.com/questions/31879390/replace-na-with-values-in-another-row-of-same-column-for-each-group-in-r

acs_cnt_2005_2009_subset$state_name <- as.character(acs_cnt_2005_2009_subset$state_name)

acs_cnt_2005_2009_subset$state_name[is.na(acs_cnt_2005_2009_subset$state_name)] <- 'District of Columbia'

acs_cnt_2005_2009_subset <- acs_cnt_2005_2009_subset %>% 
                                      group_by(statefips, countyfips) %>% mutate(county_name=unique(county_name[!is.na(county_name)]),
                                                                                      state_name=unique(state_name[!is.na(state_name)]),
                                                                                      state_code=unique(state_code[!is.na(state_code)]))



# convert tibble to dataframe
acs_cnt_2005_2009_subset <- acs_cnt_2005_2009_subset %>% mutate(county_name_state_code=str_c(county_name,", ",state_code)) %>% as.data.frame()


acs_cnt_2005_2009_subset$county_name_state_code <- ifelse(acs_cnt_2005_2009_subset$county_name_state_code=="District of Columbia, DC","Washington, DC",
                                                          acs_cnt_2005_2009_subset$county_name_state_code)

acs_cnt_2009_subset <- acs_cnt_2005_2009_subset %>%
                          dplyr::select(county_name_state_code,total_2000_or_later,area_type) %>% as.data.frame()


#write.csv(acs_cnt_2016_subset,"p1.g2.acs_cnt_2016_subset.csv",row.names = F)
# melt the dataframe
acs_cnt_2009_subset_melt <- melt(acs_cnt_2009_subset, id.var=c("county_name_state_code","area_type"))


colnames(acs_cnt_2009_subset_melt)[which(colnames(acs_cnt_2009_subset_melt)=='variable')] <- 'unit_type'
colnames(acs_cnt_2009_subset_melt)[which(colnames(acs_cnt_2009_subset_melt)=='value')] <- 'total_housing_units'

df_exurban_city <- acs_cnt_2009_subset_melt %>% filter(area_type=="Exurban" & str_detect(county_name_state_code, 'city'))

df_exurban_city_agg <- df_exurban_city %>% group_by(area_type, unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_exurban_city_data <- cbind(county_name_state_code="independent cities of\nFredericksburg, Manassas & Manassas Park",df_exurban_city_agg)


df_suburban_city <- acs_cnt_2009_subset_melt %>% filter(area_type=="Suburban" & str_detect(county_name_state_code, 'city'))

df_suburban_city_agg <- df_suburban_city %>% group_by(area_type, unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_suburban_city_data <- cbind(county_name_state_code="Fairfax & Falls Church cities",df_suburban_city_agg)


df_cnt_2009_area_wise_new_housing_melt <- rbind(acs_cnt_2009_subset_melt,df_exurban_city_data,df_suburban_city_data)

df_cnt_2009_area_wise_new_housing_melt <- df_cnt_2009_area_wise_new_housing_melt %>%
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
df <- df_cnt_2009_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


df <- df %>% arrange(area_type,total_housing_units)

df$group <- factor(df$group, levels = df$group)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))

exclude_counties <- c("Fairfax & Falls Church cities","Rappahannock County, VA","Clarke County, VA","Warren County, VA","Culpeper County, VA",
                      "independent cities of\nFredericksburg, Manassas & Manassas Park")




df1 <- df %>% filter(!(county_name_state_code %in% exclude_counties))

d1 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Suburban')
total_suburban <- d1$`sum(total_housing_units)`

d2 <- df %>% filter(county_name_state_code=="Fairfax & Falls Church cities")
total_farifax_falls <- d2$total_housing_units

fairfax_fall_church_arrow <- total_suburban-round(total_farifax_falls/2)


d3 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Exurban')
total_exburban <- d3$`sum(total_housing_units)`


d4 <- df %>% filter(county_name_state_code %in% c("Rappahannock County, VA","Clarke County, VA","Warren County, VA","Culpeper County, VA"))
total_rappanhock <- sum(d4$total_housing_units)-10

rappanhock_arrow <- total_exburban-round(total_rappanhock/2)

rappanhock_low_seg <- total_exburban-total_rappanhock

d5 <- df %>% filter(county_name_state_code=="independent cities of\nFredericksburg, Manassas & Manassas Park")
total_indep_cities <- d5$total_housing_units


indep_cities_arrow <- rappanhock_low_seg-round(total_indep_cities/2)

indep_cities_text <- indep_cities_arrow-3000

anno_text_size <- 7

p1 <- ggplot(df, aes(x = area_type, y = total_housing_units, fill =  group)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 4))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #axis.line.x = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = axis_text_size, colour = axis_labs_col),
        axis.title = element_text(size = axis_text_size, colour = axis_labs_col),
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
            color = "white", size = 6)+
  geom_segment(aes(x = 1.3, y = fairfax_fall_church_arrow, xend = 1.55 , yend = fairfax_fall_church_arrow), arrow = arrow(length = unit(0.3, "cm")),
               color="#525252")+
  annotate(geom="text", x=0.88, y=fairfax_fall_church_arrow, label="Fairfax & Falls Church cities", color="#525252", size=anno_text_size)+
  geom_segment(aes(x = 2.5, y = total_exburban, xend = 2.55 , yend = total_exburban),color="#525252")+
  geom_segment(aes(x = 2.45, y = rappanhock_arrow, xend = 2.5 , yend = rappanhock_arrow),color="#525252")+
  geom_segment(aes(x = 2.5, y = rappanhock_low_seg, xend = 2.55 , yend = rappanhock_low_seg),color="#525252")+
  geom_segment(aes(x = 2.5, y = rappanhock_low_seg, xend = 2.5 , yend = total_exburban), color="#525252")+
  annotate(geom="text", x=1.65, y=rappanhock_arrow, label="Rappahannock, Clarke, Warren & Culpeper Counties", color="#525252", size=anno_text_size)+
  geom_segment(aes(x = 2.3, y = indep_cities_arrow, xend = 2.55 , yend = indep_cities_arrow), arrow = arrow(length = unit(0.3, "cm")),color="#525252")+
  annotate(geom="text", x=1.9, y=indep_cities_text, label="independent cities of\nFredericksburg, Manassas & Manassas Park", color="#525252", size=anno_text_size)

p1

ggsave(paste0(out_dir_ch01,"sr1.g1_",dateo,"_acs_cnt_2009_housing_units_After_2000_area_county_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

df$group <- NULL

write.csv(df,paste0(out_dir_ch01,"sr1.g1_",dateo,"_acs_cnt_2009_housing_units_After_2000_area_county_wise.csv"),row.names = F)


##########################################################################################################################

############################### For 2000 #################################################################################
###################### SR1.G2. Plot Housing Units Area Wise - County Level ###############################################

acs_cnt_1910_2010 <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/sas_output/load_dec_census/was_msas_1910_2010_20190409.csv"),
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
                                                       "cv87" = "land area (sq mi, ccdb, 1950; sq m, 2010)",
                                                       "H34_2"="H34_2_built_1999_2000",
                                                       "H34_3"="H34_3_built_1995_1998",
                                                       "H34_4"="H34_4_built_1990_1994"))

colnames(acs_cnt_1910_2010)

# subset for required years
acs_cnt_2000 <- acs_cnt_1910_2010 %>% filter(year==2000)

# subset for required columns
acs_cnt_2000_subset <- acs_cnt_2000 %>% dplyr::select(statefips, countyfips, year, H34_2_built_1999_2000, H34_3_built_1995_1998,
                                                      H34_4_built_1990_1994)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
acs_cnt_2000_subset[['countyfips']] <- sapply(acs_cnt_2000_subset[["countyfips"]], padzero)

acs_cnt_2000_subset$year <- as.character(acs_cnt_2000_subset$year)

head(acs_cnt_2000_subset)

# attach columns values of county, state and state code 
acs_cnt_2000_subset <- cbind(acs_cnt_2000_subset,acs_cnt_2005_2009_subset[,c("county_name","state_name","state_code")])

acs_cnt_2000_subset <- acs_cnt_2000_subset %>% mutate("area_type"=ifelse((countyfips %in% c("001") & statefips %in% c("11"))|
                                                                       (countyfips %in% c("013","510") & statefips %in% c("51")),"Urban",
                                                                     ifelse((countyfips %in% c("033","031") & statefips %in% c("24"))|
                                                                              (countyfips %in% c("059","600","610") & statefips %in% c("51")),"Suburban",
                                                                            "Exurban")))






# convert tibble to dataframe
acs_cnt_2000_subset <- acs_cnt_2000_subset %>% mutate(county_name_state_code=str_c(county_name,", ",state_code)) %>% as.data.frame()



acs_cnt_2000_subset$county_name_state_code <- ifelse(acs_cnt_2000_subset$county_name_state_code=="District of Columbia, DC","Washington, DC",
                                                     acs_cnt_2000_subset$county_name_state_code)



acs_cnt_2000_subset <- acs_cnt_2000_subset %>% mutate("total_housing_1990_2000"=select(.,matches("^H34*")) %>% apply(1,sum,na.rm=T))


acs_cnt_2000_subset_1 <- acs_cnt_2000_subset %>% dplyr::select(county_name_state_code,total_housing_1990_2000,area_type) %>% as.data.frame()

acs_cnt_2000_subset_melt <- melt(acs_cnt_2000_subset_1,id.vars = c("county_name_state_code","area_type"))


colnames(acs_cnt_2000_subset_melt)[which(colnames(acs_cnt_2000_subset_melt)=="variable")] <- 'unit_type'
colnames(acs_cnt_2000_subset_melt)[which(colnames(acs_cnt_2000_subset_melt)=="value")] <- 'total_housing_units'



df_exurban_city <- acs_cnt_2000_subset_melt %>% filter(area_type=="Exurban" & str_detect(county_name_state_code,'city'))

df_exurban_city_agg <- df_exurban_city %>% group_by(area_type,unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_exurban_city_data <- cbind(county_name_state_code="independent cities of\nFredericksburg, Manassas & Manassas Park",df_exurban_city_agg)


df_suburban_city <- acs_cnt_2000_subset_melt %>% filter(area_type=="Suburban" & str_detect(county_name_state_code,"city"))

df_suburban_city_agg <- df_suburban_city %>% group_by(area_type,unit_type) %>% summarise(total_housing_units=sum(total_housing_units))

df_suburban_city_data <-  cbind(county_name_state_code="Fairfax & Falls Church cities",df_suburban_city_agg)


df_cnt_2000_area_wise_new_housing_melt <- rbind(acs_cnt_2000_subset_melt,df_exurban_city_data,df_suburban_city_data)


df_cnt_2000_area_wise_new_housing_melt <- df_cnt_2000_area_wise_new_housing_melt %>% 
                                    filter(!((area_type=="Exurban"|area_type=="Suburban") & str_detect(county_name_state_code,"city")))


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
df <- df_cnt_2000_area_wise_new_housing_melt
df$group <- paste0(df$area_type, "-", df$county_name_state_code, sep = "")

# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


# Build the colour pallete
colours <-ColourPalleteMulti(df, "area_type", "county_name_state_code")


df <- df %>% arrange(area_type,total_housing_units)

df$group <- factor(df$group, levels = df$group)

df$area_type <- factor(df$area_type,levels = c("Urban","Suburban","Exurban"))

exclude_counties <- c("Fairfax & Falls Church cities","Rappahannock County, VA","Clarke County, VA","Warren County, VA","Culpeper County, VA",
                      "independent cities of\nFredericksburg, Manassas & Manassas Park")



df1 <- df %>% filter(!(county_name_state_code %in% exclude_counties))

d1 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Suburban')
total_suburban <- d1$`sum(total_housing_units)`

d2 <- df %>% filter(county_name_state_code=="Fairfax & Falls Church cities")
total_farifax_falls <- d2$total_housing_units

fairfax_fall_church_arrow <- total_suburban-round(total_farifax_falls/2)


d3 <- df %>% group_by(area_type) %>% summarise(sum(total_housing_units)) %>% filter(area_type=='Exurban')
total_exburban <- d3$`sum(total_housing_units)`


d4 <- df %>% filter(county_name_state_code %in% c("Rappahannock County, VA","Clarke County, VA","Warren County, VA","Culpeper County, VA"))
total_rappanhock <- sum(d4$total_housing_units)-10

rappanhock_arrow <- total_exburban-round(total_rappanhock/2)

rappanhock_low_seg <- total_exburban-total_rappanhock

d5 <- df %>% filter(county_name_state_code=="independent cities of\nFredericksburg, Manassas & Manassas Park")
total_indep_cities <- d5$total_housing_units


indep_cities_arrow <- rappanhock_low_seg-round(total_indep_cities/2)

indep_cities_text <- indep_cities_arrow-3000

anno_text_size <- 7

p1 <- ggplot(df, aes(x = area_type, y = total_housing_units, fill =  group)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 4))+
  labs(x = "", y = "", colour = "Parameter")+
  scale_fill_manual("Subject", values=colours)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        #axis.line.x = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = axis_text_size, colour = axis_labs_col),
        axis.title = element_text(size = axis_text_size, colour = axis_labs_col),
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
            color = "white", size = 6)+
  geom_segment(aes(x = 1.3, y = fairfax_fall_church_arrow, xend = 1.55 , yend = fairfax_fall_church_arrow), arrow = arrow(length = unit(0.3, "cm")),
               color="#525252")+
  annotate(geom="text", x=0.88, y=fairfax_fall_church_arrow, label="Fairfax & Falls Church cities", color="#525252", size=anno_text_size)+
  geom_segment(aes(x = 2.5, y = total_exburban, xend = 2.55 , yend = total_exburban),color="#525252")+
  geom_segment(aes(x = 2.45, y = rappanhock_arrow, xend = 2.5 , yend = rappanhock_arrow),color="#525252")+
  geom_segment(aes(x = 2.5, y = rappanhock_low_seg, xend = 2.55 , yend = rappanhock_low_seg),color="#525252")+
  geom_segment(aes(x = 2.5, y = rappanhock_low_seg, xend = 2.5 , yend = total_exburban), color="#525252")+
  annotate(geom="text", x=1.65, y=rappanhock_arrow, label="Rappahannock, Clarke, Warren & Culpeper Counties", color="#525252", size=anno_text_size)+
  geom_segment(aes(x = 2.3, y = indep_cities_arrow, xend = 2.55 , yend = indep_cities_arrow), arrow = arrow(length = unit(0.3, "cm")),color="#525252")+
  annotate(geom="text", x=1.9, y=indep_cities_text, label="independent cities of\nFredericksburg, Manassas & Manassas Park", color="#525252", size=anno_text_size)

p1

ggsave(paste0(out_dir_ch01,"sr1.g2_",dateo,"_acs_cnt_2000_housing_units_1990_2000_area_county_wise.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

df$group <- NULL

write.csv(df,paste0(out_dir_ch01,"sr1.g2_",dateo,"_acs_cnt_2000_housing_units_1990_2000_area_county_wise.csv"),row.names = F)





