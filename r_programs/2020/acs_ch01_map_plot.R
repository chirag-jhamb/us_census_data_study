#####################################################################
# Author: Chirag Jhamb
# this program takes the summary data created
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/acs_zcta_2017.py
# and makes plots from it
# used in r_programs/2020/acs_g2_plot_v*.R
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
options(scipen=5)

########################### Functions to format the data ###################################################

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- stringr::str_pad(x,3,side ="left",pad="0")
  return(y)
}


reformat_acs_household_size_subset <- function(data) {
  data <- data %>% mutate('household_size_1'=select(.,matches('B11016_10_1-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_2'=select(.,matches('B11016_3_2-person_household|B11016_11_2-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_3_to_4'=select(.,matches('B11016_12_3-person_household|B11016_13_4-person_household|B11016_5_4-person_household|B11016_4_3-person_household')) %>% apply(1, sum, na.rm=TRUE))
  data <- data %>% mutate('household_size_more_than_4'=select(.,matches('B11016_14_5-person_household|B11016_15_6-person_household|B11016_16_7-or-more_person_household|B11016_6_5-person_household|B11016_7_6-person_household|B11016_8_7-or-more_person_household')) %>% apply(1, sum, na.rm=TRUE))
  return(data)
}


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

reformat_acs_race_subset <- function(data) {
  data$geometry <- NULL
  data['white_alone'] <- rowSums(data[,c('B03002_3_white_alone','B03002_13_white_alone')])
  data['AA_alone'] <- rowSums(data[,c('B03002_4_black_or_african_american_alone','B03002_14_black_or_african_american_alone')])
  colnames(data)[which(colnames(data) == "B03002_12_hispanic_or_latino")] <- 'hispanic_or_latino'
  return(data)
}

# sum race variables on ACS years data to create household type columns
reformat_acs_household_type_subset <- function(data) {
  colnames(data)[which(colnames(data) == "B11005_2_households_with_one_or_more_people_under_18_years")] <- 'household_with_kids'
  colnames(data)[which(colnames(data) == "B11005_12_family_households")] <- 'family_household_without_kids'
  colnames(data)[which(colnames(data) == "B11005_17_nonfamily_households")] <- 'non_family_household_without_kids'
  return(data)
}



# plot legend using bar graph plotting. Set Y values to 1s and use lables+colors on x to depict the break values of legend
plot_special_legend <- function(lbl, color_val, save_location){
  print("Plotting legend")
  # lbl <- c(0.2,0.3,0.4,0.8)
  print(lbl)
  options(scipen=4)
  df <- data.frame(x_ones = c(1,1,1,1,1),
                   q = c(4,3,2,1,0),
                   cutoffs = lbl)
  df$q <- as.factor(df$q)
  lbl <- paste(lbl, " ", sep="")
  df <- mutate(df, cutoffs.sum=cumsum(cutoffs))
  df
  cc <- df$cutoffs.sum
  # my.cols <- c("#F2F0F7","#CBC9E2","#9E9AC8","#6A51A3","#6A41A3")

  my.cols <- color_val
  my.cols <- rev(my.cols)
  p1 <- ggplot() +
    geom_bar(data = df,
             mapping = aes(x = x_ones, fill = q, y = cutoffs),
             position = "stack",
             stat = "identity",
             width = 0.1) +
    coord_flip() +
    labs(x = "",
         y = "") +
    scale_x_continuous(limits = c(0.95,1.25)) +
    scale_y_continuous(breaks = cc, labels = lbl) +
    scale_fill_manual(values=my.cols)+
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 15,
                                 margin = margin(t = 0, r = 0, b = 0, l = -20)),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = "none"
    )

# save_location
  ggsave("~/temp.jpg", plot = p1, dpi = 300, width = 15, height = 5, units = c("in"))

}

# lbl2 <- c(0.1,0.74,0.87,1.00)
# q <- ggplot()+geom_bar(data = tdata,
#          mapping = aes(x = group_col, fill = as.factor(name), y =  as.factor(name)),
#          position = "fill",
#          stat = "identity",
#          width = 0.1)+
#
#          # position_fill()+
#          labs(x = "")+
#          labs(y = "")+
#          # scale_y_continuous(waiver())+
#          scale_fill_manual(values=my.cols)+
#          # scale_color_manual(my.cols)+#+ scale_fill_manual(values=my.cols)
#          theme(axis.text.x = element_blank(),
#            axis.text.y = element_text(size = 15,  hjust = 0.01, vjust=0.1), #angle = 70, ,margin = margin(t = 0, r = 0, b = 0, l = -20)),
#            axis.ticks = element_blank(),
#            panel.background = element_rect(fill = "white"),
#            legend.position = "none")
#
#
# ggsave("~/temp.jpg", plot = q, dpi = 300, width = 16, height = 11, units = c("in"))

### make a little dataframe
# df <- data.frame(x_ones = c(1,1,1,1),
#                  q = labels,
#                  cutoffs = labels)
#
# df$q <- as.factor(df$q)
#
# ### can i make a cumulative sum?
# df <- mutate(df, cutoffs.sum=cumsum(cutoffs))
# cc <- df$cutoffs.sum
#
# p1 <- ggplot() +
# geom_bar(data = df,
#          mapping = aes(x = x_ones, fill = q, y = cutoffs),
#          # mapping = aes(x = cutoffs, fill = q, y = x_ones),
#          position = "stack",
#          stat = "identity",
#          width = 0.1) +
# coord_flip() +
# labs(x = "",
#      y = "") +
# # scale_x_continuous(limits = c(0.95,1.25)) +
# scale_y_continuous(breaks = my.cols) +
# theme(axis.text.y = element_blank(),
#   axis.text.x = element_text(size = 15,
#                              margin = margin(t = 0, r = 0, b = 0, l = -20)),
#   axis.ticks = element_blank(),
#   panel.background = element_rect(fill = "white"),
#   legend.position = "none")+ scale_fill_manual(values=my.cols)
# ggsave("~/temp.jpg", plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/zcta_data/20200113_acs_zcta_2013_2017.csv
# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_data/")

# input_file <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/zcta_data/20200218_acs_zcta_2013_2017.csv"
input_file <-"/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/zcta_data/20200219_acs_zcta_2014_2018.csv"
#out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/zcta_maps/")
out_dir <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/202001_meeting/"

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


data <- zcta_2017
# get required format for age:
formatted_data <- reformat_acs_age_subset(data)
# get required format for race:
formatted_data <- reformat_acs_race_subset(formatted_data)
# get required format for median income:
# colnames(formatted_data)[which(colnames(formatted_data) == "B19013_1_median_household_income_in_the_past_12_months_.in_2017_inflation.adjusted_dollars.")] <- 'median_income'
colnames(formatted_data)[which(colnames(formatted_data) == "B19013_1_median_household_income_in_the_past_12_months_.in_2018_inflation.adjusted_dollars.")] <- 'median_income'
formatted_data$median_income <- unlist(lapply(formatted_data$median_income, as.numeric))

# get required format for household type and size columns
formatted_data <- reformat_acs_household_size_subset(formatted_data)
formatted_data <- reformat_acs_household_type_subset(formatted_data)

to_format_columns <- c("age_less_than_18",'age_18_to_29','age_30_to_44',
                        'age_45_to_59','age_above_59', "white_alone", "AA_alone", "hispanic_or_latino",
                      "household_with_kids")

# formatted_data <- data[c("FIPS","STATE","COUNTY","ZCTA","age_less_than_18",'age_18_to_29','age_30_to_44','age_45_to_59','age_above_59','total_population')]


for (i in to_format_columns){
  formatted_data[[i]] <- formatted_data[[i]]/formatted_data$total_population
}

cborders1960 <- st_read(dsn = paste0(groupDir,"/maps/united_states/census2010/counties/"), layer = "cnty_2010_20140313")


cborders1960c <- cborders1960[which((cborders1960$COUNTY %in% c("001") & cborders1960$STATE %in% c("11"))|
                                      (cborders1960$COUNTY %in% c("009","017","021","031","033") & cborders1960$STATE %in% c("24"))|
                                      (cborders1960$COUNTY %in% c("037") & cborders1960$STATE %in% c("54"))|
                                      (cborders1960$COUNTY %in% c("013","043","047","059","061","107","153","157","177","179","187",
                                                                  "510","600","610","630","683","685")
                                       & cborders1960$STATE %in% c("51"))),]




metro_dc <- st_read(dsn = paste0(groupDir,"/maps/washington_dc/metro/Metro__Lines"), layer = "Metro__Lines")

national_highway <- st_read(dsn = paste0(groupDir,"/maps/washington_dc/highway/tl_2017_us_primaryroads"), layer = "tl_2017_us_primaryroads")

national_highway_interstate <- national_highway %>% filter(RTTYP=="I")


plot_quantile_map <- function(colname,  fname, exurban = T, roads = T){
  border_size <- 0.1

  cborders1960c <- cborders1960c %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                                 (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                               ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                        (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                      "Exurban")))
  if (exurban==F){
    cborders1960c <- subset(cborders1960c, area_type!= "Exurban")
  }
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
  colname_str <- quo_name(colname)
  print(colname_str)
  # compute labels
  labels <- c()
  brks <- unique(quantile(formatted_data[[colname]],probs = seq(0, 1, 0.25), na.rm=T))
  # create the color vector
  my.cols <- brewer.pal(length(brks), "Purples")
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  # put labels into label vector
  labels <- labels[1:length(labels)-1]
  print(labels)
  formatted_data$brks <- cut(formatted_data[[colname_str]],
                 breaks = brks,
                 include.lowest = TRUE,
                 labels = labels)

  brks_levels <- as.character(sort(as.numeric(levels(as.factor(formatted_data$brks)))))
  formatted_data$brks <- factor(formatted_data$brks,levels=brks_levels)
  brks_scale <- levels(formatted_data$brks)
  labels_scale <- rev(brks_levels)
  # restore the geometry for intersection
  formatted_data$geometry <- zcta_2017$geometry
  df.int <- st_intersection(st_as_sf(formatted_data),cborders1960c)
  national_highway_interstate.int <- st_intersection(national_highway_interstate,cborders1960c)
  if (roads==T){
    p <- ggplot(df.int) +
      geom_sf(aes(fill=brks),colour="white", size=0.3)+
      geom_sf(data = metro_dc, fill=NA, colour = "#de2d26", size=border_size)+
      geom_sf(data = national_highway_interstate.int, fill=NA, colour = "#fc9272", size=border_size)+
      geom_sf(data = cborders1960c, fill=NA, colour = "#545353", size=0.3)+
      coord_sf() +
      theme_map() +
      theme(legend.position = "none",legend.background = element_rect(color = NA))#+
  } else{
    print("No road program running!")
    p <- ggplot(df.int) +
      geom_sf(aes(fill=brks),colour="white", size=0.05)+
      geom_sf(data = cborders1960c, fill=NA, colour = "#545353", size=0.1)+
      coord_sf() +
      theme_map() +
      theme(legend.position = "none",legend.background = element_rect(color = NA))#+
  }



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
  print(fname)
  ggsave(fname, plot =tester, dpi = 300, width = 16, height = 11, units = c("in"))
  fname <- str_replace(fname, ".jpg","_legend.jpg")
  plot_special_legend(brks, my.cols ,fname)
}


  plot_quantile_map_per_county <- function(colname,  fname){
      border_size <- 0.2
      cborders1960c <- cborders1960c %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                                     (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                                   ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                            (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                            "Exurban")))

      cborders1960c$STATE<- lapply(cborders1960c$STATE, as.character)
      cborders1960c$COUNTY<- lapply(cborders1960c$COUNTY, as.character)
      cborders1960c$FIPS <- paste(cborders1960c$STATE,cborders1960c$COUNTY, sep="")
      formatted_data_original <- formatted_data
      for (fip in unique(cborders1960c$FIPS)){
        #subset for present county
        cborders1960_sub <- subset(cborders1960c, FIPS== fip)
        formatted_data <-subset(formatted_data_original, FIPS== fip)

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
        colname_str <- quo_name(colname)
        # compute labels
        labels <- c()
        brks <- unique(quantile(formatted_data[[colname_str]],probs = seq(0, 1, 0.25), na.rm=T))
        # create the color vector
        my.cols <- brewer.pal(length(brks), "Purples")
        # round the labels (actually, only the extremes)
        for(idx in 1:length(brks)){
          labels <- c(labels,round(brks[idx + 1], 2))
        }
        # put labels into label vector
        labels <- labels[1:length(labels)-1]
        print(labels)
        formatted_data$brks <- cut(formatted_data[[colname_str]],
                       breaks = brks,
                       include.lowest = TRUE,
                       labels = labels)
        brks_levels <- as.character(sort(as.numeric(levels(as.factor(formatted_data$brks)))))
        formatted_data$brks <- factor(formatted_data$brks,levels=brks_levels)
        brks_scale <- levels(formatted_data$brks)
        labels_scale <- rev(brks_levels)
        # restore the geometry for intersection
        print("assigning geometry")
        zcta_2017_sub <- subset(zcta_2017, FIPS== fip)
        formatted_data$geometry <- zcta_2017_sub$geometry
        print("running intersection")

        df.int <- st_intersection(st_as_sf(formatted_data),cborders1960_sub)
        national_highway_interstate.int <- st_intersection(national_highway_interstate,cborders1960_sub)

        p <- ggplot(df.int) +
          geom_sf(aes(fill=brks),colour="white", size=0.3)+
          # geom_sf(data = metro_dc, fill=NA, colour = "#de2d26", size=border_size)+
          # geom_sf(data = national_highway_interstate.int, fill=NA, colour = "#fc9272", size=border_size)+
          geom_sf(data = cborders1960_sub, fill=NA, colour = "#545353", size=0.7)+
          coord_sf() +
          theme_map() +
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

        print("Saved temp pdf")
        save_name <- paste(fname,fip,"_income_",dateo,".jpg",sep="")
        print(save_name)
        ggsave(save_name, plot =tester, dpi = 300, width = 16, height = 11, units = c("in"))
        save_name <- paste(fname,fip,"_income_legend_",dateo,".jpg",sep="")
        plot_special_legend(brks, my.cols ,save_name)
      }
    }
