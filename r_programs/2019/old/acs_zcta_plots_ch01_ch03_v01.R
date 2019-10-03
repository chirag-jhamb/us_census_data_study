#####################################################################
#
# this program takes the summary data created
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_county_2012_2016_v02.py
# and tries to make plots from it
# for presentation purposes
#
# december 30, 2018
#
# acs_zcta_2012_2016_plots.R
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

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch01/")
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch02/")
out_dir_ch03 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch03/")


# load the data
acs_zcta_2012_2016 <- read.csv(paste0(data_dir,"acs_zcta_2012_2016.csv"),stringsAsFactors = F)

# clean up the column names having "X" or "." in the begining or "." in the end
colnames(acs_zcta_2012_2016) <- gsub("^X", "",  colnames(acs_zcta_2012_2016))
colnames(acs_zcta_2012_2016) <- gsub("^\\.", "",  colnames(acs_zcta_2012_2016))
colnames(acs_zcta_2012_2016) <- gsub("\\.$", "",  colnames(acs_zcta_2012_2016))


# rename the columns to avoid column duplicates error
colnames(acs_zcta_2012_2016) <- make.unique(names(acs_zcta_2012_2016))
names(acs_zcta_2012_2016)[names(acs_zcta_2012_2016)=="total_total_population"] <- "total_population"

#names of columns in data frame
cols <- colnames(acs_zcta_2012_2016)

# character variables
cols.char <- c("FILETYPE","LOGRECNO","STATE","GEO_ID","NAME","LSAD")

#numeric variables
cols.num <- cols[!cols %in% cols.char]



# write a function to convert the required columns to numeric
make_num <- function(x)
{
  return(as.numeric(as.character(x)))
}

# make all the required columns numeric
acs_zcta_2012_2016[cols.num] <- lapply(acs_zcta_2012_2016[cols.num],make_num)

acs_zcta_2012_2016[is.na(acs_zcta_2012_2016)] <- 0

sum(is.na(acs_zcta_2012_2016))


head(acs_zcta_2012_2016)



zctas <- st_read(paste0(groupDir,"/maps/united_states/census2010/zcta/2017_census_zcta_shapefile"),
                 layer="cb_2017_us_zcta510_500k",stringsAsFactors = F)

names(zctas)[1] <- 'ZCTA'

zcta_2016 <- merge(zctas,acs_zcta_2012_2016,by='ZCTA')

# clean up the column names having "X" or "." in the begining or "." in the end
colnames(zcta_2016) <- gsub("^X", "",  colnames(zcta_2016))
colnames(zcta_2016) <- gsub("^\\.", "",  colnames(zcta_2016))
colnames(zcta_2016) <- gsub("\\.$", "",  colnames(zcta_2016))

head(zcta_2016)


data <- zcta_2016

st_geometry(data) <- NULL



data %>% select(matches('^1..detached.*attached$|attached.1$|attached.6$|attached.7$')) %>% head(2)

data %>% select(.,setdiff(starts_with('2.to'),matches('.4$|4.1$|4.6$|4.7$'))) %>% head(2)


data <- data %>% mutate('total_single_family_households_since_2000'=select(.,matches('^1..detached.*attached$|attached.1$|attached.6$|attached.7$'))
                        %>% apply(1, sum, na.rm=TRUE))

#data %>% select(setdiff(starts_with('1, detached'),
#                       matches('attached$|attached.1$|attached.6$|attached.7$'))) %>% head(2)

data <- data %>% mutate('total_single_family_households_before_2000'=select(.,setdiff(starts_with('1..detached'),
                                                                                      matches('attached$|attached.1$|attached.6$|attached.7$'))) %>%  apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_2_to_4_family_households_since_2000'=select(.,matches('^2.to.4$|4.1$|4.6$|4.7$')) %>%
                          apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_2_to_4_family_households_before_2000'=select(.,setdiff(starts_with('2.to'),
                                                                                      matches('.4$|4.1$|4.6$|4.7$'))) %>% apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_5_to_19_family_households_since_2000'=select(.,matches('^5.to.19$|19.1$|19.6$|19.7$')) %>%
                          apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_5_to_19_family_households_before_2000'=select(.,setdiff(starts_with('5.to'),
                                                                                       matches('to.19$|19.1$|19.6$|19.7$'))) %>% apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_20_to_49_family_households_since_2000'=select(.,matches('^20.to.49$|49.1$|49.6$|49.7$')) %>%
                          apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_20_to_49_family_households_before_2000'=select(.,setdiff(starts_with('20.to'),
                                                                                        matches('.49$|49.1$|49.6$|49.7$'))) %>% apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('total_50_or_more_family_households_since_2000'=select(.,matches('^50.or.more$|more.1$|more.6$|more.7$')) %>%
                          apply(1, sum, na.rm=TRUE))


data <- data %>% mutate('total_50_or_more_family_households_before_2000'=select(.,setdiff(starts_with('50.or'),
                                                                                          matches('.more$|more.1$|more.6$|more.7$'))) %>% apply(1, sum, na.rm=TRUE))



# filter for the relevant columns
# acs_zcta_2016_new_ext_housing <- data %>%
#                           select(ZCTA,total_single_family_households_since_2000,total_single_family_households_before_2000,
#                                  total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
#                                  total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
#                                  total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
#                                  total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000) %>%
#                                     as.data.frame()


#data %>% select(matches('*since_2000$')) %>% head(2)

data <- data %>% mutate('new_constructions'=select(.,matches('*since_2000$'))
                        %>% apply(1, sum, na.rm=TRUE))

#data %>% select(matches('^total_single_family*')) %>% head(2)

data <- data %>% mutate('total_single_family_households'=select(.,matches('^total_single_family*'))
                        %>% apply(1, sum, na.rm=TRUE))

print(head(data[,c(242,243,253)]))


zcta_2016$new_constructions <- data$new_constructions

zcta_2016$total_single_family_households_since_2000 <- data$total_single_family_households_since_2000

zcta_2016$total_single_family_households <- data$total_single_family_households

head(zcta_2016)

#zcta_2016 <- st_transform(zcta_2016,crs = 4326)

#zcta_2016 <- zcta_2016 %>% mutate("median_value_by_household"=round(`Median.value..dollars`/`Median.household.income.in.the.past.12.months..in.2016.inflation.adjusted.dollars`,2))

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


# create a function to plot the absolute values 
plot_zcta_level_quantile_values <- function(df,colname){
  
  colname_str <- quo_name(colname)
  
  # create the color vector
  my.cols <- brewer.pal(4, "Blues")
  
  # compute labels
  labels <- c()
  print(df[[colname_str]])
  
  # put manual breaks as desired
  brks <- quantile(df[[colname_str]])
  
  
  
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  # put labels into label vector
  labels <- labels[1:length(labels)-1]
  
  labels <- round(labels)
  
  # define a new variable on the data set just as above
  df$brks <- cut(df[[colname_str]], 
                 breaks = brks, 
                 include.lowest = TRUE, 
                 labels = labels)
  
  # define breaks scale and labels scales
  
  brks_scale <- levels(df$brks)
  labels_scale <- rev(brks_scale)
  
  print(df)
  
  # # draw the plot with legend at the bottom
  p <- ggplot(df) +
    geom_sf(aes(fill=brks),colour="white")+
    coord_sf() +
    theme_map() +
    theme(legend.position = "bottom",legend.background = element_rect(color = NA))
  
  #print(p)
  # 
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
  
  if (colname_str=='new_constructions'){
    save_plot <- paste0(out_dir_ch01,"p1.m1_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  } else if (colname_str=='total_single_family_households_since_2000'){
    save_plot <- paste0(out_dir_ch02,"p2.m1_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  } else if (colname_str=='total_single_family_households'){
    save_plot <- paste0(out_dir_ch02,"p2.m1_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  } else if (colname_str=='median_value_by_household'){
    save_plot <- paste0(out_dir_ch03,"p3.m1_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  }  else {
    save_plot <- paste0(out_dir_ch03,"p3.m1_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  }
  
  ggsave(save_plot,plot = tester, dpi = 300, width = 16, height = 11, units = c("in"))
  
}

# provide column names for which we want absolute value plot on county level
#col_vec <- c("New","Median.value..dollars","Median.household.income.in.the.past.12.months..in.2016.inflation.adjusted.dollars")
#col_vec <- c("total_single_family_households_since_2000","total_single_family_households")

#col_vec <- c("new_constructions")

col_vec <- c("new_constructions","total_single_family_households_since_2000","total_single_family_households")

# call the funtion to create plot for each variable
for (col in col_vec){
  plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)))
}



# df <- zcta_2016
# 
# # create a function to plot the absolute values
# #plot_zcta_level_quantile_values <- function(df,colname){
# 
# #colname_str <- quo_name(colname)
# 
# # create the color vector
# my.cols <- brewer.pal(4, "Dark2")
# 
# # compute labels
# labels <- c()
# #print(df[[colname_str]])
# 
# # put manual breaks as desired
# brks <- quantile(df[["new_constructions"]])
# 
# 
# 
# # round the labels (actually, only the extremes)
# for(idx in 1:length(brks)){
#   labels <- c(labels,round(brks[idx + 1], 2))
# }
# 
# # put labels into label vector
# labels <- labels[1:length(labels)-1]
# 
# labels <- round(labels)
# 
# # define a new variable on the data set just as above
# df$brks <- cut(df[["new_constructions"]],
#                breaks = brks,
#                include.lowest = TRUE,
#                labels = labels)
# 
# # define breaks scale and labels scales
# 
# brks_scale <- levels(df$brks)
# labels_scale <- rev(brks_scale)
# 
# print(df)
# 
# 
# # read the county boundaries data
# cborders1960 <- st_read(dsn = paste0(groupDir,"/maps/united_states/census1960/counties/"), layer = "US_county_1960_conflated")
# 
# 
# # make a state variable
# cborders1960$fipsstate <- substr(cborders1960$NHGISST,1,2)
# 
# # make a county variable
# cborders1960$fipscounty <- substr(cborders1960$GISJOIN,5,7)
# 
# 
# cborders1960c <- cborders1960[which((cborders1960$fipscounty %in% c("001") & cborders1960$fipsstate %in% c("11"))|
#                                       (cborders1960$fipscounty %in% c("009","017","021","031","033") & cborders1960$fipsstate %in% c("24"))|
#                                       (cborders1960$fipscounty %in% c("037") & cborders1960$fipsstate %in% c("54"))|
#                                       (cborders1960$fipscounty %in% c("013","043","047","059","061","107","153","157","177","179","187",
#                                                                       "510","600","610","630","683","685")
#                                        & cborders1960$fipsstate %in% c("51"))),]
# 
# 
# # # draw the plot with legend at the bottom
# p <- ggplot(df) +
#   geom_sf(aes(fill=brks),colour="white")+
#   geom_sf(fill = "transparent", color = "grey50", size = 0.5, data = cborders1960c)+
#   coord_sf() +
#   theme_map() +
#   theme(legend.position = "bottom",legend.background = element_rect(color = NA))
# 
# 
# #print(p)
# #
# # # provide manual scale and colors to the graph
# tester <- p +
#   # now we have to use a manual scale,
#   # because only ever one number should be shown per label
#   scale_fill_manual(
#     # in manual scales, one has to define colors, well, we have done it earlier
#     values = my.cols,
#     breaks = rev(brks_scale),
#     name = paste0("Quantiles of ","new_constructions"),
#     drop = FALSE,
#     labels = labels_scale,
#     guide = guide_legend(
#       direction = "horizontal",
#       keyheight = unit(2.5, units = "mm"),
#       keywidth = unit(85 / length(labels), units = "mm"),
#       title.position = 'top',
#       # shift the labels around, the should be placed
#       # exactly at the right end of each legend key
#       title.hjust = 0.5,
#       label.hjust = 1,
#       nrow = 1,
#       byrow = T,
#       # also the guide needs to be reversed
#       reverse = T,
#       label.position = "bottom"
#     )
#   )
# 
# tester
# 
# 
# 
# ggsave(paste0(out_dir_ch01,"p1.m1_",dateo,"_acs_zcta_quantiles_of_","new_constructions",".jpg"),
#        plot = tester, dpi = 300, width = 16, height = 11, units = c("in"))


