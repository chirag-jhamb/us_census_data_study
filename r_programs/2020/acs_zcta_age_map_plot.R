#####################################################################
# Author: Chirag Jhamb
# this program takes the summary data created
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/acs_zcta_2017.py
# and makes plots from it
# for presentation purposes
#
##############################################################################
# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(RColorBrewer)
library(sf)
library(grid)
library(gridExtra)

########################### Functions to format the data ###################################################

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- stringr::str_pad(x,3,side ="left",pad="0")
  return(y)
}
# 'B01001_2_male', 'B01001_3_under_5_years', 'B01001_4_5_to_9_years', 'B01001_5_10_to_14_years', 'B01001_6_15_to_17_years', 'B01001_7_18_and_19_years', 'B01001_8_20_years', 'B01001_9_21_years', 'B01001_10_22_to_24_years', 'B01001_11_25_to_29_years', 'B01001_12_30_to_34_years', 'B01001_13_35_to_39_years', 'B01001_14_40_to_44_years', 'B01001_15_45_to_49_years', 'B01001_16_50_to_54_years', 'B01001_17_55_to_59_years', 'B01001_18_60_and_61_years', 'B01001_19_62_to_64_years', 'B01001_20_65_and_66_years', 'B01001_21_67_to_69_years', 'B01001_22_70_to_74_years', 'B01001_23_75_to_79_years', 'B01001_24_80_to_84_years', 'B01001_25_85_years_and_over',
# 'B01001_26_female', 'B01001_27_under_5_years', 'B01001_28_5_to_9_years', 'B01001_29_10_to_14_years', 'B01001_30_15_to_17_years', 'B01001_31_18_and_19_years', 'B01001_32_20_years', 'B01001_33_21_years', 'B01001_34_22_to_24_years', 'B01001_35_25_to_29_years', 'B01001_36_30_to_34_years', 'B01001_37_35_to_39_years', 'B01001_38_40_to_44_years', 'B01001_39_45_to_49_years', 'B01001_40_50_to_54_years', 'B01001_41_55_to_59_years', 'B01001_42_60_and_61_years', 'B01001_43_62_to_64_years', 'B01001_44_65_and_66_years', 'B01001_45_67_to_69_years', 'B01001_46_70_to_74_years', 'B01001_47_75_to_79_years', 'B01001_48_80_to_84_years', 'B01001_49_85_years_and_over'
# sum age variables on ACS years data to create age columns (also the total_population column)
reformat_acs_age_subset <- function(data) {
  st_geometry(data) <- NULL
  data['age_less_than_18'] <- rowSums(data[,c('B01001_3_under_5_years', 'B01001_4_5_to_9_years', 'B01001_5_10_to_14_years', 'B01001_6_15_to_17_years','B01001_27_under_5_years', 'B01001_28_5_to_9_years', 'B01001_29_10_to_14_years', 'B01001_30_15_to_17_years')])
  data['age_18_to_29'] <- rowSums(data[,c('B01001_7_18_and_19_years', 'B01001_8_20_years', 'B01001_9_21_years', 'B01001_10_22_to_24_years', 'B01001_11_25_to_29_years','B01001_31_18_and_19_years', 'B01001_32_20_years', 'B01001_33_21_years', 'B01001_34_22_to_24_years', 'B01001_35_25_to_29_years')])
  data['age_30_to_44'] <- rowSums(data[,c('B01001_12_30_to_34_years', 'B01001_13_35_to_39_years', 'B01001_14_40_to_44_years',
                      'B01001_36_30_to_34_years', 'B01001_37_35_to_39_years', 'B01001_38_40_to_44_years')])
  data['age_45_to_59'] <- rowSums(data[,c('B01001_15_45_to_49_years', 'B01001_16_50_to_54_years', 'B01001_17_55_to_59_years',
                      'B01001_39_45_to_49_years', 'B01001_40_50_to_54_years', 'B01001_41_55_to_59_years')])
  data['age_above_59'] <- rowSums(data[,c('B01001_18_60_and_61_years', 'B01001_19_62_to_64_years', 'B01001_20_65_and_66_years', 'B01001_21_67_to_69_years', 'B01001_22_70_to_74_years', 'B01001_23_75_to_79_years', 'B01001_24_80_to_84_years', 'B01001_25_85_years_and_over','B01001_42_60_and_61_years', 'B01001_43_62_to_64_years', 'B01001_44_65_and_66_years', 'B01001_45_67_to_69_years', 'B01001_46_70_to_74_years', 'B01001_47_75_to_79_years', 'B01001_48_80_to_84_years', 'B01001_49_85_years_and_over')])
  colnames(data)[which(colnames(data) == "B01001_1_total")] <- 'total_population'
  return(data)
}


##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/zcta_data/20200113_acs_zcta_2013_2017.csv
# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_data/")
input_file <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/zcta_data/20200113_acs_zcta_2013_2017.csv"
#out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/zcta_maps/")
out_dir <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/202001_meeting/"
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/ch01/")
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/ch02/")


out_dir_county_maps <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/county_pages/")


# load the data
acs_zcta_2013_2017 <- read.csv(input_file,stringsAsFactors = F)


zctas <- st_read(paste0(groupDir,"/maps/united_states/census2010/zcta/2017_census_zcta_shapefile"),
                 layer="cb_2017_us_zcta510_500k",stringsAsFactors = F)

names(zctas)[1] <- 'ZCTA'

zcta_2017 <- merge(zctas,acs_zcta_2013_2017,by='ZCTA')

head(zcta_2017)


# apply the function to get countyfips column as length 3
zcta_2017[['COUNTY']] <- sapply(zcta_2017[["COUNTY"]], padzero)
zcta_2017$FIPS <- paste(zcta_2017$STATE,zcta_2017$COUNTY, sep="")

head(zcta_2017)


age_data <- zcta_2017

data <- reformat_acs_age_subset(age_data)

age_columns <- c("age_less_than_18",'age_18_to_29','age_30_to_44','age_45_to_59','age_above_59')
# age_data <- data[c("FIPS","STATE","COUNTY","ZCTA","age_less_than_18",'age_18_to_29','age_30_to_44','age_45_to_59','age_above_59','total_population')]


for (i in age_columns){
  print(i)
  data[[i]] <- data[[i]]/data$total_population
  age_data[[i]] <- data[[i]]
  # p <- ggplot()+aes(age_data[[i]])+geom_histogram() + ggtitle(i)
  # fname <- paste(out_dir,'age/',dateo,'_histogram_',i,'.jpg',sep="")
  # print(fname)
  # jpeg(fname)
  # print(p)
  # dev.off()
}

cborders1960 <- st_read(dsn = paste0(groupDir,"/maps/united_states/census2010/counties/"), layer = "cnty_2010_20140313")


cborders1960c <- cborders1960[which((cborders1960$COUNTY %in% c("001") & cborders1960$STATE %in% c("11"))|
                                      (cborders1960$COUNTY %in% c("009","017","021","031","033") & cborders1960$STATE %in% c("24"))|
                                      (cborders1960$COUNTY %in% c("037") & cborders1960$STATE %in% c("54"))|
                                      (cborders1960$COUNTY %in% c("013","043","047","059","061","107","153","157","177","179","187",
                                                                  "510","600","610","630","683","685")
                                       & cborders1960$STATE %in% c("51"))),]



cborders1960c <- cborders1960c %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                               (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                             ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                      (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                    "Exurban")))


metro_dc <- st_read(dsn = paste0(groupDir,"/maps/washington_dc/metro/Metro__Lines"), layer = "Metro__Lines")

national_highway <- st_read(dsn = paste0(groupDir,"/maps/washington_dc/highway/tl_2017_us_primaryroads"), layer = "tl_2017_us_primaryroads")

national_highway_interstate <- national_highway %>% filter(RTTYP=="I")


border_size <- 0.5

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

print(fname)
jpeg(fname)
print(p)
dev.off()

colname <- "age_less_than_18"
colname_str <- quo_name(colname)
# compute labels
labels <- c()
brks <- unique(quantile(age_data[[colname_str]],probs = seq(0, 1, 0.25), na.rm=T))
# create the color vector
my.cols <- brewer.pal(length(brks), "Purples")


# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

# put labels into label vector
labels <- labels[1:length(labels)-1]
print(labels)
# if(labels[2]<1){
#   labels <- round(labels,2)
# }else{
#   labels <- round(labels)
# }
# define a new variable on the data set just as above
age_data$brks <- cut(age_data[[colname_str]],
               breaks = brks,
               include.lowest = TRUE,
               labels = labels)

brks_levels <- as.character(sort(as.numeric(levels(as.factor(age_data$brks)))))
age_data$brks <- factor(age_data$brks,levels=brks_levels)
brks_scale <- levels(age_data$brks)
labels_scale <- rev(brks_levels)

df.int <- st_intersection(age_data,cborders1960c)
national_highway_interstate.int <- st_intersection(national_highway_interstate,cborders1960c)

p <- ggplot(df.int) +
  geom_sf(aes(fill=brks),colour="white", size=0.3)+
  geom_sf(data = metro_dc, fill=NA, colour = "#de2d26", size=border_size)+
  geom_sf(data = national_highway_interstate.int, fill=NA, colour = "#fc9272", size=border_size)+
  #geom_sf(data = cborders1960c, fill=NA, colour = "#737373", size=border_size)+
  geom_sf(data = cborders1960c, fill=NA, colour = "#545353", size=0.7)+
  coord_sf() +
  theme_map() +
  #theme(legend.position = "bottom",legend.background = element_rect(color = NA))#+
  theme(legend.position = "none",legend.background = element_rect(color = NA))#+

  # # provide manual scale and colors to the graph
tester <- p +
  # now we have to use a manual scale,
  # because only ever one number should be shown per label
  scale_fill_manual(
    # in manual scales, one has to define colors, well, we have done it earlier
    values = my.cols,
    breaks = rev(brks_scale),
    name = paste0("Quantiles of ",colname_str),
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2.5, units = "mm"),
      keywidth = unit(85 / length(labels), units = "mm"),
      title.position = 'top',
      # shift the labels around, the should be placed
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )

fname <- paste(out_dir,'age/',dateo,'_test_map.jpg',sep="")
print(fname)
jpeg(fname)
print(tester)
dev.off()
