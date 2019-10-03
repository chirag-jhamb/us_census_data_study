#####################################################################
#
# this program takes the summary data created in
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2010_2016_v02.py
# and tries to make plots from it
# for presentation purposes
#
# January 24, 2018
#
# acs_county_page_plots_2016.R
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
out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/")


# load the data
acs_cnt_2012_2016 <- read.csv(paste0(data_dir,"20190130_cnty_acs_2012_2016_absolute_values.csv"),stringsAsFactors = F)


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
  
  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))
  
  
  return(data)
}

# get the data for year 2016 with relevant columns by calling the function
acs_cnt_2012_2016_subset <- reformat_subset_data(acs_cnt_2012_2016)


head(acs_cnt_2012_2016_subset)

acs_cnt_2012_2016_subset %>% select(intersect(starts_with("B25127"),contains("2_to_4"))) %>% head(2)


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                      mutate('1_Units'=select(.,intersect(starts_with("B25127"),contains("1,_detached"))) %>%
                                   apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                            mutate('2_to_4_Units'=select(.,intersect(starts_with("B25127"),contains("2_to_4"))) %>%
                                     apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                                    mutate('5_to_19_Units'=select(.,intersect(starts_with("B25127"),contains("5_to_19"))) %>%
                                             apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                            mutate('20_to_49_Units'=select(.,intersect(starts_with("B25127"),contains("20_to_49"))) %>%
                                     apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                              mutate('50_or_more_Units'=select(.,intersect(starts_with("B25127"),contains("50_or_more"))) %>%
                                       apply(1, sum, na.rm=TRUE))


#acs_cnt_2012_2016_subset %>% select(matches('^B11003_4_|B11003_11_|B11003_17_'))

#acs_cnt_2012_2016_subset %>% select(matches('^B11003_5_|B11003_12_|B11003_18_'))

#acs_cnt_2012_2016_subset %>% select(matches('^B11003_6_|B11003_13_|B11003_19_'))

#acs_cnt_2012_2016_subset %>% select(matches('^B11003_7_|B11003_14_|B11003_20_'))  ## no kids under 18


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                              mutate('children_under_6_years_only'=select(.,matches('^B11003_4_|B11003_11_|B11003_17_')) %>%
                                        apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                            mutate('children_under_6_years_and_6_to_17_years'=select(.,matches('^B11003_5_|B11003_12_|B11003_18_')) %>%
                                     apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                                  mutate('children_6_to_17_years_only'=select(.,matches('^B11003_6_|B11003_13_|B11003_19_')) %>%
                                           apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                          mutate('no_own_children_of_the_householder_under_18_years'=select(.,matches('^B11003_7_|B11003_14_|B11003_20_')) %>%
                                         apply(1, sum, na.rm=TRUE))

acs_cnt_2012_2016_subset %>% select(matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_'))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% 
                                mutate('total_white_hispanic_black_asian'=select(.,matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_')) %>%
                                apply(1, sum, na.rm=TRUE))


acs_cnt_2012_2016_subset <- acs_cnt_2012_2016_subset %>% mutate('non_white_hispanic_black_asian'=B03002_1_total-total_white_hispanic_black_asian)


#acs_cnt_2012_2016_subset %>% select(matches('^B25056'))

############################################### 1 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_built_year <- acs_cnt_2012_2016_subset %>%
  select(STATE_FIPS,county_code,county_name,intersect(starts_with("B25034"),contains("_built_"))) %>%
  as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_built_year)[4:ncol(acs_cnt_2012_2016_subset_built_year)] 

built_cols <- gsub("B25034_([0-9]|1[0-9])_","",built_cols)

colnames(acs_cnt_2012_2016_subset_built_year)[4:ncol(acs_cnt_2012_2016_subset_built_year)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_built_year_melt <- melt(acs_cnt_2012_2016_subset_built_year, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_built_year_melt_dc <- acs_cnt_2012_2016_subset_built_year_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

acs_cnt_2012_2016_subset_built_year_melt_dc$variable <- factor(acs_cnt_2012_2016_subset_built_year_melt_dc$variable, 
                                                               rev(levels(acs_cnt_2012_2016_subset_built_year_melt_dc$variable)))
# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_built_year_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "age of housing", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12.5),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=F), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
p1


# save the graph
ggsave(paste0(out_dir,"c1.g1_",dateo,"_dc_acs_cnt_2016_housing_units_by_age_of_housing_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 2 ############################################################

# filter for the relevant columns
# acs_cnt_2012_2016_subset_structure_type <- acs_cnt_2012_2016_subset %>%
#                                 select(STATE_FIPS,county_code,county_name,`1_Units`,`2_to_4_Units`,`5_to_19_Units`,
#                                        `20_to_49_Units`,`50_or_more_Units`) %>%
#                                               as.data.frame()

acs_cnt_2012_2016_subset_structure_type <- acs_cnt_2012_2016_subset %>%
                                select(STATE_FIPS,county_code,county_name,contains("_Units")) %>%
                                              as.data.frame()

# melt the dataframe
acs_cnt_2012_2016_subset_structure_type_melt <- melt(acs_cnt_2012_2016_subset_structure_type, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_structure_type_melt_dc <- acs_cnt_2012_2016_subset_structure_type_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_structure_type_melt_dc, aes(x = variable, y = value )) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "structure type", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
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
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


p1

# save the graph
ggsave(paste0(out_dir,"c1.g2_",dateo,"_dc_acs_cnt_2016_housing_units_by_structure_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

############################################### 3 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_owner_occupied <- acs_cnt_2012_2016_subset %>%
                                     select(STATE_FIPS,county_code,county_name,intersect(starts_with("B25075"),contains("_$"))) %>%
                                               as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_owner_occupied)[4:ncol(acs_cnt_2012_2016_subset_owner_occupied)] 

built_cols <- gsub("B25075_([0-9]|[1-2][0-9])_","",built_cols)

colnames(acs_cnt_2012_2016_subset_owner_occupied)[4:ncol(acs_cnt_2012_2016_subset_owner_occupied)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_owner_occupied_melt <- melt(acs_cnt_2012_2016_subset_owner_occupied, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_owner_occupied_melt_dc <- acs_cnt_2012_2016_subset_owner_occupied_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_owner_occupied_melt_dc, aes(x = county_name, y = value, fill = variable)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "owner occupied type", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
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
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)

p1

# save the graph
ggsave(paste0(out_dir,"c1.g3_",dateo,"_dc_acs_cnt_2016_housing_units_by_owner_occupied.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 4 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_household_income <- acs_cnt_2012_2016_subset %>%
                                            select(STATE_FIPS,county_code,county_name,intersect(starts_with("B19001"),contains("_$"))) %>%
                                                    as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_household_income)[4:ncol(acs_cnt_2012_2016_subset_household_income)] 

built_cols <- gsub("B19001_([0-9]|[1-2][0-9])_","",built_cols)

colnames(acs_cnt_2012_2016_subset_household_income)[4:ncol(acs_cnt_2012_2016_subset_household_income)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_household_income_melt <- melt(acs_cnt_2012_2016_subset_household_income, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_household_income_melt_dc <- acs_cnt_2012_2016_subset_household_income_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_household_income_melt_dc, aes(x = variable, y = value )) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "household income type", y = "num of people", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12.5),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)

p1

# save the graph
ggsave(paste0(out_dir,"c1.g4_",dateo,"_dc_acs_cnt_2016_household_income_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 5 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_race <- acs_cnt_2012_2016_subset %>%
                                               select(STATE_FIPS,county_code,county_name,matches('^B03002_3_|B03002_4_|B03002_6_|B03002_12_'),
                                                      non_white_hispanic_black_asian) %>%
                                                 as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_race)[4:ncol(acs_cnt_2012_2016_subset_race)] 

built_cols <- gsub("B03002_([0-9]|[1-2][0-9])_","",built_cols)

colnames(acs_cnt_2012_2016_subset_race)[4:ncol(acs_cnt_2012_2016_subset_race)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_race_melt <- melt(acs_cnt_2012_2016_subset_race, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_race_melt_dc <- acs_cnt_2012_2016_subset_race_melt %>% filter(STATE_FIPS=="11" & county_code=="001")

# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_race_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "race type", y = "num of people", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


p1

# save the graph
ggsave(paste0(out_dir,"c1.g5_",dateo,"_dc_acs_cnt_2016_housing_units_by_race_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


############################################### 6 ############################################################

# filter for the relevant columns
acs_cnt_2012_2016_subset_household_type <- acs_cnt_2012_2016_subset %>%
                                             select(STATE_FIPS,county_code,county_name,intersect(starts_with("B25056"),contains("_$")),
                                                    B25056_27_no_cash_rent)%>%
                                                                    as.data.frame()

built_cols <- colnames(acs_cnt_2012_2016_subset_household_type)[4:ncol(acs_cnt_2012_2016_subset_household_type)] 

built_cols <- gsub("B25056_([0-9]|[1-2][0-9])_","",built_cols)

colnames(acs_cnt_2012_2016_subset_household_type)[4:ncol(acs_cnt_2012_2016_subset_household_type)] <- built_cols

# melt the dataframe
acs_cnt_2012_2016_subset_household_type_melt <- melt(acs_cnt_2012_2016_subset_household_type, id.var=c("STATE_FIPS","county_name","county_code"))

acs_cnt_2012_2016_subset_household_type_melt_dc <- acs_cnt_2012_2016_subset_household_type_melt %>% filter(STATE_FIPS=="11" & county_code=="001")



# plot the graph
p1 <- ggplot(acs_cnt_2012_2016_subset_household_type_melt_dc, aes(x = variable, y = value)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
  #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
  #scale_colour_manual(values = c("orange","green"))+
  labs(x = "contract rent value", y = "num of housing units", colour = "Parameter")+
  scale_shape_manual(values = c(16, 21)) +
  geom_text(aes(label=value, size=value), vjust=-0.1, color="black", size=5)+
  #labs(x="", y="") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12.5),
        axis.title = element_text(size = 25),
        plot.title = element_text(size=25),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position=c(0.9,0.9),
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
  guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# Here we define spaces as the big separator
#point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


# make the barplot horizontal
p1

# save the graph
ggsave(paste0(out_dir,"c1.g6_",dateo,"_dc_acs_cnt_2016_housing_units_by_rent_type.jpg"),
       plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))



# # filter for the relevant columns
# acs_cnt_2012_2016_subset_race <- acs_cnt_2012_2016_subset %>%
#   select(STATE_FIPS,county_code,county_name,setdiff(starts_with("B02001"),contains("_total"))) %>%
#   as.data.frame()
# 
# built_cols <- colnames(acs_cnt_2012_2016_subset_race)[4:ncol(acs_cnt_2012_2016_subset_race)] 
# 
# built_cols <- gsub("B02001_([0-9]|[1-2][0-9])_","",built_cols)
# 
# colnames(acs_cnt_2012_2016_subset_race)[4:ncol(acs_cnt_2012_2016_subset_race)] <- built_cols
# 
# # melt the dataframe
# acs_cnt_2012_2016_subset_race_melt <- melt(acs_cnt_2012_2016_subset_race, id.var=c("STATE_FIPS","county_name","county_code"))
# 
# acs_cnt_2012_2016_subset_race_melt_dc <- acs_cnt_2012_2016_subset_race_melt %>% filter(STATE_FIPS=="11" & county_code=="001")
# 
# # plot the graph
# p <- ggplot(acs_cnt_2012_2016_subset_race_melt_dc, aes(x = county_name, y = value, fill = variable)) +
#   geom_bar(stat = "identity")+
#   scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
#   #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
#   #scale_colour_manual(values = c("orange","green"))+
#   labs(x = "race type", y = "num of people", colour = "Parameter")+
#   scale_shape_manual(values = c(16, 21)) +
#   #labs(x="", y="") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(color="gray"),
#         axis.line.x = element_line(color = "black"),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text = element_text(size = 25),
#         axis.title = element_text(size = 25),
#         plot.title = element_text(size=25),
#         legend.title = element_blank(),
#         legend.position=c(0.9,0.9),
#         legend.justification = c(1,1),
#         legend.text = element_text(size=25),
#         legend.key.size = unit(0.8,"line"),
#         legend.key = element_rect(fill = "white"),
#         legend.spacing = unit(0.45,"cm"))+
#   guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# # Here we define spaces as the big separator
# #point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
# 
# 
# # make the barplot horizontal
# p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))
# 
# p1
# 
# # save the graph
# ggsave(paste0(out_dir,"c1.g5_",dateo,"_dc_acs_cnt_2016_housing_units_by_race_type.jpg"),
#        plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))
# 
# 
# ############################################### 6 ############################################################
# 
# # filter for the relevant columns
# acs_cnt_2012_2016_subset_household_type <- acs_cnt_2012_2016_subset %>%
#   select(STATE_FIPS,county_code,county_name,contains("children"),-contains("B11003")) %>%
#   as.data.frame()
# 
# # melt the dataframe
# acs_cnt_2012_2016_subset_household_type_melt <- melt(acs_cnt_2012_2016_subset_household_type, id.var=c("STATE_FIPS","county_name","county_code"))
# 
# acs_cnt_2012_2016_subset_household_type_melt_dc <- acs_cnt_2012_2016_subset_household_type_melt %>% filter(STATE_FIPS=="11" & county_code=="001")
# 
# # plot the graph
# p <- ggplot(acs_cnt_2012_2016_subset_household_type_melt_dc, aes(x = county_name, y = value, fill = variable)) +
#   geom_bar(stat = "identity")+
#   scale_y_continuous(labels = scales::comma, breaks = trans_breaks(identity, identity, n = 5))+
#   #scale_x_continuous(limits= c(1950, 2016), breaks = c(seq(1950,2016,10))) +
#   #scale_colour_manual(values = c("orange","green"))+
#   labs(x = "household type", y = "num of children", colour = "Parameter")+
#   scale_shape_manual(values = c(16, 21)) +
#   #labs(x="", y="") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(color="gray"),
#         axis.line.x = element_line(color = "black"),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text = element_text(size = 25),
#         axis.title = element_text(size = 25),
#         plot.title = element_text(size=25),
#         legend.title = element_blank(),
#         legend.position=c(0.9,0.9),
#         legend.justification = c(1,1),
#         legend.text = element_text(size=25),
#         legend.key.size = unit(0.8,"line"),
#         legend.key = element_rect(fill = "white"),
#         legend.spacing = unit(0.45,"cm"))+
#   guides(colour = guide_legend(override.aes = list(size=10),reverse=T), size=FALSE)
# # Here we define spaces as the big separator
# #point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
# 
# 
# # make the barplot horizontal
# p1 <- p + coord_flip() #+ scale_y_continuous(labels = point,expand = expand_scale(mult = c(0, .1)))
# 
# p1
# 
# # save the graph
# ggsave(paste0(out_dir,"c1.g6_",dateo,"_dc_acs_cnt_2016_housing_units_by_household_type.jpg"),
#        plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))
