#####################################################################
#
# this program takes the summary data created in 
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2016_v01.py
# and tries to make plots from it
# for presentation purposes
#
# December 14, 2018
#
# acs_1950_2016_plots.R
#
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


# load the data
acs_cnt_2007_2011 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2007_2011_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2008_2012 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2008_2012_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2009_2013 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2009_2013_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2010_2014 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2010_2014_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2011_2015 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2011_2015_absolute_values.csv"),stringsAsFactors = F)
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20181217_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)



# write a function to reformat the data to bring it to county level and subset it for requried columns
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
colnames(data) <- make.unique(names(data))

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

data <- data %>% mutate(year=substr(FILETYPE,1,4))

#data %>% select(matches('^No own children')) %>% head(2)

data <- data %>% mutate('family_with_no_childern'=select(.,matches('^No own children')) %>% 
                                            apply(1, sum, na.rm=TRUE)) %>% 
                                            mutate("percent_families_with_children"=round((`Total:_y`-family_with_no_childern)/`Total:_y`*100,2))

data_subset <- data %>% select(STATE_FIPS, county_code, year, Total_population, total_housing_units, Median_Value, 
                               Median_household_income, percent_families_with_children, county_name, state_name, state_code)

return (data_subset)

}


acs_cnt_2007_2011_subset <- reformat_subset_data(acs_cnt_2007_2011)
acs_cnt_2008_2012_subset <- reformat_subset_data(acs_cnt_2008_2012)
acs_cnt_2009_2013_subset <- reformat_subset_data(acs_cnt_2009_2013)
acs_cnt_2010_2014_subset <- reformat_subset_data(acs_cnt_2010_2014)
acs_cnt_2011_2015_subset <- reformat_subset_data(acs_cnt_2011_2015)
acs_cnt_2012_2016_subset <- reformat_subset_data(acs_cnt_2012_2016)


acs_cnt_2011_2016 <- rbind(acs_cnt_2007_2011_subset,acs_cnt_2008_2012_subset, acs_cnt_2009_2013_subset, acs_cnt_2010_2014_subset, 
                           acs_cnt_2011_2015_subset, acs_cnt_2012_2016_subset)



acs_cnt_1910_2010 <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/sas_output/load_dec_census/was_msas_1910_2010_20181218.csv"),stringsAsFactors = F) 

colnames(acs_cnt_1910_2010)  

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
acs_cnt_1950_2010_subset <- acs_cnt_1950_2010 %>% select(statefips, countyfips, year, total_population, total_housing_units, median_housing_value,
                                                         median_household_income)

padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

acs_cnt_1950_2010_subset[['countyfips']] <- sapply(acs_cnt_1950_2010_subset[["countyfips"]], padzero)

acs_cnt_1950_2010_subset$year <- as.character(acs_cnt_1950_2010_subset$year)

head(acs_cnt_1950_2010_subset)      

# make column names same before stacking dataframes
colnames(acs_cnt_2011_2016)[1:7] <- colnames(acs_cnt_1950_2010_subset)[1:7]

# stack both dataframes to get pop and housing for year 1950-2016
acs_cnt_1950_2016 <- bind_rows(acs_cnt_1950_2010_subset, acs_cnt_2011_2016)

#acs_cnt_1950_2016$county_name <- paste(acs_cnt_1950_2016$NAME,acs_cnt_1950_2016$LSAD, na.exclude(TRUE))

###################### A. Plot Population and Housing Timeline Graph ###################################################

# find year wise total population and housing units for dc area
acs_cnt_1950_2016_pop_housing <- acs_cnt_1950_2016 %>% group_by(year) %>% summarise(population = sum(total_population, na.rm = T), 
                                                                                    housing_units = sum(total_housing_units, na.rm = T))

acs_cnt_1950_2016_pop_housing$year <- as.numeric(acs_cnt_1950_2016_pop_housing$year)

acs_cnt_1950_2016_pop_housing <- as.data.frame(acs_cnt_1950_2016_pop_housing)

p1 <- ggplot(acs_cnt_1950_2016_pop_housing, aes(x=year)) +
  geom_line(aes(y=population,size=0.1, color="population")) +
  geom_line(aes(y=housing_units,size=0.1, color="housing")) +
  #geom_point(aes(y=population,shape="np"), size=5) +
  #geom_point(aes(y=housing_units,shape="np"), size=5) +
  #geom_point(aes(shape="np"), size=5) +
  scale_y_continuous(labels = comma,  limits = c(450000, 6500000), breaks = c(seq(1000000,6500000,1000000)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  scale_colour_manual(values = c("orange","green"))+
  labs(y = "", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.15),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+ 
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE) 

  
ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_pop_housing.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))


padzero <- function(x){
  y <- str_pad(x,3,side ="left",pad="0")
  return(y)
}

################################### B. Plot Area Wise Population and Housing ################################################

acs_cnt_1950_2016[['countyfips']] <- sapply(acs_cnt_1950_2016[["countyfips"]], padzero)

acs_cnt_1950_2016 <- acs_cnt_1950_2016 %>% mutate("area_type"=ifelse(countyfips %in% c("001","013") & statefips %in% c("11","51"),"Urban",
                                           ifelse(countyfips %in% c("033","031","059","600","610") & statefips %in% c("24","51"),"SubUrban",
                                                  "ExUrban")))

#test %>% filter(area_type=="SubUrban") %>% arrange(statefips, countyfips)

acs_cnt_1950_2016_area_wise_pop_housing <- acs_cnt_1950_2016 %>% 
                                                group_by(year, area_type) %>% 
                                                            summarise(population = sum(total_population, na.rm = T), 
                                                                      housing_units = sum(total_housing_units, na.rm = T))


acs_cnt_1950_2016_area_wise_pop_housing <- as.data.frame(acs_cnt_1950_2016_area_wise_pop_housing)

acs_cnt_1950_2016_area_wise_pop_housing$year <- as.numeric(acs_cnt_1950_2016_area_wise_pop_housing$year)

p1 <- ggplot(acs_cnt_1950_2016_area_wise_pop_housing, aes(x=year, y=population, fill=area_type)) + 
  geom_area()+
 scale_y_continuous(labels = comma,  limits = c(0, 6500000), breaks = c(seq(0,6500000,1000000)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(y = "population", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.2,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+ 
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE) 

ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_area_wise_pop.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))



p1 <- ggplot(acs_cnt_1950_2016_area_wise_pop_housing, aes(x=year, y=housing_units, fill=area_type)) + 
  geom_area()+
  scale_y_continuous(labels = comma,  limits = c(0, 2500000), breaks = c(seq(0,2500000,500000)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(y = "housing units", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.2,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+ 
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE) 


ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_area_wise_housing.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))


################ C. Population and Housing Density ######################

# read the county area census 2010 data
county_area <- st_read(paste0(groupDir,"/maps/united_states/census2010/counties"),layer="cnty_2010_20140313",stringsAsFactors = F)

# make the geometry NULL
st_geometry(county_area) <- NULL

# convert the state_fips column to character from numeric for joining
acs_cnt_1950_2016$statefips <- as.character(acs_cnt_1950_2016$statefips)

# join the county area data to summary data for respective states and counties
acs_cnt_1950_2016 <- left_join(acs_cnt_1950_2016,county_area,by=c("statefips"="STATE","countyfips"="COUNTY"))

# create the poplation density and housing units density columns

# before make sure same rows which are NA in population and housing are NA in CENSUS area as well to calculate density correctly
acs_cnt_1950_2016 <- acs_cnt_1950_2016 %>% mutate(CENSUSAREA=ifelse(total_population=='NA' | total_housing_units=='NA', NA,CENSUSAREA))


acs_cnt_1950_2016_pop_housing_density <- acs_cnt_1950_2016 %>% 
                                                group_by(year) %>%
                                                      summarise(population_density = sum(total_population, na.rm = T)/sum(CENSUSAREA, na.rm = T),
                                                                housing_density = sum(total_housing_units, na.rm = T)/sum(CENSUSAREA, na.rm = T))


acs_cnt_1950_2016_pop_housing_density$year <- as.numeric(acs_cnt_1950_2016_pop_housing_density$year)


acs_cnt_1950_2016_area_wise_pop_housing_density <- acs_cnt_1950_2016 %>% 
                                                        group_by(year, area_type) %>%
                                                        summarise(population_density = sum(total_population, na.rm = T)/sum(CENSUSAREA, na.rm = T),
                                                                  housing_density = sum(total_housing_units, na.rm = T)/sum(CENSUSAREA, na.rm = T))


acs_cnt_1950_2016_area_wise_pop_housing_density$year <- as.numeric(acs_cnt_1950_2016_area_wise_pop_housing_density$year)

p1 <- ggplot(acs_cnt_1950_2016_pop_housing_density, aes(x=year)) +
  geom_line(aes(y=population_density,size=0.1, color="population density")) +
  geom_line(aes(y=housing_density,size=0.1, color="housing density")) +
  #geom_point(aes(y=population,shape="np"), size=5) +
  #geom_point(aes(y=housing_units,shape="np"), size=5) +
  #geom_point(aes(shape="np"), size=5) +
 # scale_y_continuous(labels = comma,  limits = c(450000, 6500000), breaks = c(seq(1000000,6500000,1000000)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  scale_colour_manual(values = c("orange","green"))+
  labs(y = "", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.15),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)


ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_pop_housing_density.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))


################################### C. Area wise Population and Housing Density ################################################

p1 <- ggplot(acs_cnt_1950_2016_area_wise_pop_housing_density, aes(x=year, y=population_density, fill=area_type)) +
  geom_area()+
 scale_y_continuous(labels = comma,  limits = c(0, 12500), breaks = c(seq(0,12500,2500)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(y = "population density", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.5,1),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)

ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_area_wise_pop_density.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))



p1 <- ggplot(acs_cnt_1950_2016_area_wise_pop_housing_density, aes(x=year, y=housing_density, fill=area_type)) +
  geom_area()+
  #scale_y_continuous(labels = comma,  limits = c(0, 2500000), breaks = c(seq(0,2500000,500000)))+
  scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(y = "housing density", x = "year", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.2,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)


ggsave(paste0(out_dir,dateo,"_acs_cnt_1950_2016_area_wise_housing_density.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))


################################### D. Comparison Graphs for Census 2000 vs 2016  ################################################

acs_cnt_2000_2016_comp <- acs_cnt_1950_2016 %>% filter(year=="2000" | year=="2016") 


acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% 
                                  group_by(statefips, countyfips) %>%
                                  arrange(year, .by_group=TRUE) %>%
                                  mutate(housing_units_pct_change_from_2000=(total_housing_units/lag(total_housing_units)-1) * 100) %>%
                                  mutate(housing_units_pct_change_from_2000=round(housing_units_pct_change_from_2000,2))
                                          

acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% mutate(housing_density=round(total_housing_units/CENSUSAREA))


acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% 
                                  group_by(statefips, countyfips) %>%
                                  arrange(year, .by_group=TRUE) %>%
                                  mutate(housing_units_density_pct_change_from_2000=(housing_density/lag(housing_density)-1) * 100) %>%
                                  mutate(housing_units_density_pct_change_from_2000=round(housing_units_density_pct_change_from_2000,2))


acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% 
                                  group_by(statefips, countyfips) %>%
                                  arrange(year, .by_group=TRUE) %>%
                                  mutate(median_housing_value_pct_change_from_2000=(median_housing_value/lag(median_housing_value)-1) * 100) %>%
                                  mutate(median_housing_value_pct_change_from_2000=round(median_housing_value_pct_change_from_2000,2))


acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% 
                                  group_by(statefips, countyfips) %>%
                                  arrange(year, .by_group=TRUE) %>%
                                  mutate(median_household_income_pct_change_from_2000=(median_household_income/lag(median_household_income)-1) * 100) %>%
                                  mutate(median_household_income_pct_change_from_2000=round(median_household_income_pct_change_from_2000,2))

acs_cnt_2000_2016_comp <- acs_cnt_2000_2016_comp %>% arrange(year) %>% as.data.frame()


########### a ####################
p1 <- ggplot(data = subset(acs_cnt_2000_2016_comp, year %in% c("2016")), aes(x = median_housing_value, y = housing_units_pct_change_from_2000)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 750000), breaks = c(seq(0,750000,150000))) +
  #scale_y_continuous(limits= c(0, 30000), breaks = c(seq(0,30000,10000))) +
  labs(x = "median housing value", y="% change in no. of housing units from 2000 to 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  geom_abline(intercept = 0, slope = 0.4, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10))) 


ggsave(paste0(out_dir,dateo,"_acs_cnt_comp_2000_2016_scatter_house_median_value_percent_change_housing_units.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))

########### b ####################
p1 <- ggplot(data = subset(acs_cnt_2000_2016_comp, year %in% c("2016")), aes(x = median_housing_value_pct_change_from_2000, 
                                                                       y = housing_units_pct_change_from_2000)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 250), breaks = c(seq(0,250,25))) +
  #scale_y_continuous(limits= c(0, 30000), breaks = c(seq(0,30000,10000))) +
  labs(x = "% change in median housing value from 2000 to 2016", y="% change in no. of housing units from 2000 to 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  #geom_abline(intercept = 0, slope = 1, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10))) 

ggsave(paste0(out_dir,dateo,"_acs_cnt_comp_2000_2016_scatter_percent_chg_median_home_value_percent_chg_housing_units.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))

########### d ####################

p1 <- ggplot(data = subset(acs_cnt_2000_2016_comp, year %in% c("2016")), aes(x = median_household_income, y = housing_units_pct_change_from_2000)) +
  #geom_point(aes(color = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none")),
  #                fill = factor(damage_eyeball,levels=c("irreparable","extensive","minimal","none"))),size=7) +
  #geom_point(aes(color = factor(STATE)),shape = 16, size = 3)+
  geom_point(shape = 16, size = 5)+geom_text(aes(label=county_name),hjust=-0.1, vjust=0.15)+
  scale_x_continuous(labels = comma,limits= c(0, 130000), breaks = c(seq(0,130000,30000))) +
  #scale_y_continuous(limits= c(0, 30000), breaks = c(seq(0,30000,10000))) +
  labs(x = "median household income", y="% change in no. of housing units from 2000 to 2016")+
  #scale_color_manual(values = c("none" = damage_scale[4], "minimal" = damage_scale[3],"extensive" = damage_scale[2],"irreparable" = damage_scale[1]))+
  geom_abline(intercept = 0, slope = 0.4, color = "grey")+  # 45 degree line
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color="gray"),
        #legend.position = "right",
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.2),
        legend.justification = c(0.5,0.5),
        legend.text = element_text(size=25),
        legend.key.size = unit(2,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.35,"cm"))+ guides(colour = guide_legend(override.aes = list(size=10))) 

ggsave(paste0(out_dir,dateo,"_acs_cnt_comp_2000_2016_scatter_household_median_inc_percent_chg_housing_units.jpg"),
       plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))

################## c, e, f ########################################

# create a function to plot the absolute values 
plot_county_level_absolute_values <- function(df,colname,county_col){
  
  #print(colname)
  colname_str <- quo_name(colname)
  county_col_str <- quo_name(county_col)
  #print(colname_str)
  #print(county_col_str)
  
  #print(factor(df[,county_col_str]))
  #print(levels(df[,county_col_str]))
  
  # sort the data by colname and retain order by county name
  df <- df[order(df[colname_str]),] # sort
  
  df[[county_col_str]] <- factor(df[,county_col_str], levels = df[,county_col_str])  # to retain the order in plot.
  
  # create the scale vector to hold colors for all the 24 counties
  my_scale <- rep("NA",24)
  
  # provide desired colors to the counties on the basis of state
  my_scale[which(df$state_code=="DC")] <- rep("grey",1)
  my_scale[which(df$state_code=="VA")] <- rep("orange",17)
  my_scale[which(df$state_code=="MD")] <- rep("purple",5)
  my_scale[which(df$state_code=="WV")] <- rep("yellow",1)
  
  # create the plot for respective column
  p <- ggplot(df,aes_(x = county_col, y = colname,fill = county_col)) +
    geom_bar(stat = "identity")+ scale_fill_manual(values=my_scale)+
    geom_text(aes_(label=colname), hjust=-0.1, color="black", size=3.5)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          #panel.grid.major.y = element_line(color="gray"),
          legend.position = "none",
          title = element_text(size = 12.5),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12.5),
          panel.grid = element_blank(), panel.border = element_blank())
  
  # Here we define spaces as the big separator
  point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
  
  
  # make the barplot horizontal
  p1 <- p + coord_flip() + scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))
  
  #print(p1)
  
  ggsave(paste0(out_dir,dateo,"_acs_cnt_comp_2000_2016_",colname_str,".jpg"),
         plot = p1, dpi = 300, width = 11, height = 8.5, units = c("in"))
  
  
}

# provide column names for which we want absolute value plot on county level
col_vec <- c("housing_units_density_pct_change_from_2000", "median_household_income_pct_change_from_2000","percent_families_with_children")

# call the funtion to create plot for each variable
for (col in col_vec){
  plot_county_level_absolute_values(subset(acs_cnt_2000_2016_comp, year %in% c("2016")),quo(!!sym(col)),quo(!!sym("county_name")))
}

