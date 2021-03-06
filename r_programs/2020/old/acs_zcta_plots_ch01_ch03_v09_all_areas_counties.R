#####################################################################
#
# this program takes the summary data created
# by python program
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2019/acs_census_zcta_ch01_ch03_v04.py
# and tries to make plots from it
# for presentation purposes
#
# Macrh 20, 2019
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
library(grid)
library(gridExtra) 

##############################################################################
# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

# data and output directories
data_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data/")
#out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/zcta_maps/")

out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch01/")
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/ch02/")


out_dir_county_maps <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/county_maps/county_maps/")

out_dir_county_maps_data <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/county_page/county_maps/county_maps_data/")


# load the data
acs_zcta_2012_2016 <- read.csv(paste0(data_dir,"20190315_acs_zcta_2013_2017.csv"),stringsAsFactors = F)


zctas <- st_read(paste0(groupDir,"/maps/united_states/census2010/zcta/2017_census_zcta_shapefile"),
                 layer="cb_2017_us_zcta510_500k",stringsAsFactors = F)

names(zctas)[1] <- 'ZCTA'

zcta_2016 <- merge(zctas,acs_zcta_2012_2016,by='ZCTA')

# clean up the column names having "X" or "." in the begining or "." in the end
# colnames(zcta_2016) <- gsub("^X", "",  colnames(zcta_2016))
# colnames(zcta_2016) <- gsub("^\\.", "",  colnames(zcta_2016))
# colnames(zcta_2016) <- gsub("\\.$", "",  colnames(zcta_2016))

head(zcta_2016)


data <- zcta_2016

st_geometry(data) <- NULL



data %>% select(matches('^B25127_8_|B25127_15_|B25127_51_|B25127_58_')) %>% head(2)


data %>% select(matches('^B25127_18_|B25127_25_|B25127_32_|B25127_39_|B25127_61_|B25127_68_|B25127_75_|B25127_82_')) %>% head(2)


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



# filter for the relevant columns
# acs_zcta_2016_new_ext_housing <- data %>% 
#                           select(ZCTA,total_single_family_households_since_2000,total_single_family_households_before_2000,
#                                  total_2_to_4_family_households_since_2000,total_2_to_4_family_households_before_2000,
#                                  total_5_to_19_family_households_since_2000, total_5_to_19_family_households_before_2000,
#                                  total_20_to_49_family_households_since_2000,total_20_to_49_family_households_before_2000,
#                                  total_50_or_more_family_households_since_2000,total_50_or_more_family_households_before_2000) %>% 
#                                     as.data.frame()


#data %>% select(matches('*since_2000$')) %>% head(2)

data %>% select(matches('*before_2000$')) %>% head(2)

data <- data %>% mutate('new_constructions'=select(.,matches('*since_2000$')) 
                        %>% apply(1, sum, na.rm=TRUE))

data <- data %>% mutate('existing_constructions'=select(.,matches('*before_2000$')) 
                        %>% apply(1, sum, na.rm=TRUE))

#data <- data %>% mutate('new_existing_constructions_ratio'=round(new_constructions/existing_constructions,2)*100)

data <- data %>% mutate('new_existing_constructions_ratio'=round(new_constructions/existing_constructions,2))
#data %>% select(matches('^total_single_family*')) %>% head(2)

data$new_existing_constructions_ratio <-  ifelse(is.na(data$new_existing_constructions_ratio)==TRUE,0,data$new_existing_constructions_ratio)

data <- data %>% mutate('total_single_family_households'=select(.,matches('^total_single_family*')) 
                        %>% apply(1, sum, na.rm=TRUE))


#data <- data %>% mutate('share_single_family_new_constructions'=round(total_single_family_households_since_2000/new_constructions,2)*100)

data <- data %>% mutate('share_single_family_new_constructions'=round(total_single_family_households_since_2000/new_constructions,2))

data$share_single_family_new_constructions <-  ifelse(is.na(data$share_single_family_new_constructions)==TRUE,0,
                                                      data$share_single_family_new_constructions)

print(head(data[,c(242,243,253)]))


zcta_2016$new_constructions <- data$new_constructions

zcta_2016$existing_constructions <- data$existing_constructions

zcta_2016$total_single_family_households_since_2000 <- data$total_single_family_households_since_2000

zcta_2016$total_single_family_households <- data$total_single_family_households

zcta_2016$new_existing_constructions_ratio <- data$new_existing_constructions_ratio

zcta_2016$share_single_family_new_constructions <- data$share_single_family_new_constructions



head(zcta_2016)

# define a function to make countyfips column length 3
padzero <- function(x){
  y <- stringr::str_pad(x,3,side ="left",pad="0")
  return(y)
}

# apply the function to get countyfips column as length 3
zcta_2016[['COUNTY']] <- sapply(zcta_2016[["COUNTY"]], padzero)


#zcta_2016 <- st_transform(zcta_2016,crs = 4326)


zcta_2016 <- zcta_2016 %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                       (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                     ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                              (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                            "Exurban")))


zcta_2016[,c(240:249)] 


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


border_size <- 0.8

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


#df <- zcta_2016
#colname <- "share_single_family_new_constructions"

# create a function to plot the absolute values 
plot_zcta_level_quantile_values <- function(df,colname,area_type){
  
  colname_str <- quo_name(colname)
  
  # compute labels
  labels <- c()
  #print(df[[colname_str]])
  
  # put manual breaks as desired
  brks <- unique(quantile(df[[colname_str]],probs = seq(0, 1, 0.25)))
  
  
  # create the color vector
  my.cols <- brewer.pal(length(brks), "Purples")
  
  
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  # put labels into label vector
  labels <- labels[1:length(labels)-1]
  
  
  if(labels[2]<1){
    labels <- round(labels,2)
  }else{
    labels <- round(labels)
  }
  # define a new variable on the data set just as above
  df$brks <- cut(df[[colname_str]], 
                 breaks = brks, 
                 include.lowest = TRUE, 
                 labels = labels)
  
  
  df$brks <- as.character(df$brks)
  
  if(length(unique(df$brks))<4){
    df$brks <- ifelse(df[[colname_str]]==0,"0",df$brks)
  }
  
  #df$brks <- ifelse(df[[colname_str]]==-1,"-1",df$brks)
  
  brks_levels <- as.character(sort(as.numeric(levels(as.factor(df$brks)))))
  
  df$brks <- factor(df$brks,levels=brks_levels)
  
  brks_scale <- levels(df$brks)
  labels_scale <- rev(brks_scale)
  
  # if(labels_scale[length(labels_scale)]=="-1"){
  #   labels_scale[length(labels_scale)] <- "NA"
  #   my.cols <- c("#ef1a21",my.cols)
  # }
  
  if(area_type!="00000"){
    df <- df %>% filter(area_type=="Urban" | area_type=="Suburban")
    cborders1960c <- cborders1960c %>% filter(area_type=="Urban" | area_type=="Suburban")
    #print(cborders1960c)
  }
  
  df.int <- st_intersection(df,cborders1960c)
  
  #my.cols[2] <- "#000000"
  #https://stackoverflow.com/questions/44678978/how-to-label-an-individual-state-on-the-map-while-the-others-at-sub-divisional-l    
  # df.int <- df.int %>% mutate(
  #   lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
  #   lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  # )
  national_highway_interstate.int <- st_intersection(national_highway_interstate,cborders1960c)
  
  # # draw the plot with legend at the bottom
  p <- ggplot(df.int) +
    geom_sf(aes(fill=brks),colour="white", size=0.3)+
    geom_sf(data = metro_dc, fill=NA, colour = "#de2d26", size=border_size)+
    geom_sf(data = national_highway_interstate.int, fill=NA, colour = "#fc9272", size=border_size)+
    geom_sf(data = cborders1960c, fill=NA, colour = "#737373", size=border_size)+
    coord_sf() +
    theme_map() +
    #theme(legend.position = "bottom",legend.background = element_rect(color = NA))#+ 
    theme(legend.position = "none",legend.background = element_rect(color = NA))#+ 
  # geom_text(data = subset(df.int, brks==-1), #angle=90,
  #       aes(label = ZCTA, x = lon, y = lat), # << move each label down by 1 unit
  #      # position = position_dodge(0.9), hjust=3, #vjust=-0.5,
  #       color = "yellow", size = 5)
  
  
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
  
  #print(tester)

  #df_sub <- df[,c(1,234:235,240:ncol(df),236:239,2:5)]
  
  df.int_sub <- df.int %>% dplyr::select("ZCTA","NAME","LSAD","STATE", "COUNTY","new_constructions","existing_constructions","new_existing_constructions_ratio",
                                         "share_single_family_new_constructions", "total_single_family_households_since_2000", "total_single_family_households",
                                         "CENSUSAREA","zcta_area","intersected_area","percent_intersected_area","brks")                                                                                                     
  
  st_geometry(df.int_sub) <- NULL
  
  #df.int_sub_NA_zctas <- df.int_sub %>% filter(brks==-1)
  
  if (colname_str=='new_constructions'){
    save_plot <- paste0(out_dir_ch01,"p1.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_ch01,"p1.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),row.names = F)
  } else if (colname_str=='new_existing_constructions_ratio'){
    save_plot <- paste0(out_dir_ch01,"p1.m2.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_ch01,"p1.m2.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),row.names = F)
  } else if (colname_str=='share_single_family_new_constructions'){
    save_plot <- paste0(out_dir_ch02,"p2.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_ch02,"p2.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),row.names = F)
  } 
  # else if (colname_str=='Median.value..dollars'){
  #   save_plot <- paste0(out_dir_ch03,"p3.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  # }  else {
  #   save_plot <- paste0(out_dir_ch03,"p3.m1.",area_type,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,".jpg")
  # }
  
  print(save_plot)
  
  ggsave(save_plot,plot = tester, dpi = 300, width = 16, height = 11, units = c("in"))
  
}

# provide column names for which we want absolute value plot on county level
#col_vec <- c("new_constructions","new_existing_constructions_ratio","share_single_family_new_constructions")

col_vec <- c("new_constructions","share_single_family_new_constructions")


# call the funtion to create plot for each variable
for (col in col_vec){
  plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),"00000")
  plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),"11111")
}


# df <- zcta_2016
# colname <- "new_constructions"
# statefips <- "11"
# countyfips <- "001"


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



plot_zcta_level_quantile_values <- function(df,colname,statefips,countyfips){
  
  colname_str <- quo_name(colname)
  
  # compute labels
  labels <- c()
  #print(df[[colname_str]])
  
  # put manual breaks as desired
  brks <- unique(quantile(df[[colname_str]],probs = seq(0, 1, 0.25)))
  
  
  # create the color vector
  my.cols <- brewer.pal(length(brks), "Purples")
  
  
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  # put labels into label vector
  labels <- labels[1:length(labels)-1]
  
  
  if(labels[2]<1){
    labels <- round(labels,2)
  }else{
    labels <- round(labels)
  }
  # define a new variable on the data set just as above
  df$brks <- cut(df[[colname_str]], 
                 breaks = brks, 
                 include.lowest = TRUE, 
                 labels = labels)
  
  
  df$brks <- as.character(df$brks)
  
  if(length(unique(df$brks))<4){
    df$brks <- ifelse(df[[colname_str]]==0,"0",df$brks)
  }
  
  #df$brks <- ifelse(df[[colname_str]]==-1,"-1",df$brks)
  
  brks_levels <- as.character(sort(as.numeric(levels(as.factor(df$brks)))))
  
  df$brks <- factor(df$brks,levels=brks_levels)
  
  brks_scale <- levels(df$brks)
  labels_scale <- rev(brks_scale)
  
  # if(labels_scale[length(labels_scale)]=="-1"){
  #   labels_scale[length(labels_scale)] <- "NA"
  #   my.cols <- c("#ef1a21",my.cols)
  # }
  
  # if(area_type!="00000"){
  #   df <- df %>% filter(area_type=="Urban" | area_type=="Suburban")
  #   cborders1960c <- cborders1960c %>% filter(area_type=="Urban" | area_type=="Suburban")
  #   #print(cborders1960c)
  # }
  
  if(statefips!="00"){
    df_sub <- df %>% filter(STATE==statefips & COUNTY==countyfips)
    cborders1960c_sub <- cborders1960c %>% filter(STATE==statefips & COUNTY==countyfips)
  }else{
  cborders1960c_sub <- cborders1960c 
  df_sub <- df
  }
  
  df_sub <- st_transform(df_sub,crs = 4326)
  metro_dc <- st_transform(metro_dc,4326)
  cborders1960c_sub <- st_transform(cborders1960c_sub,4326)
  national_highway_interstate <- st_transform(national_highway_interstate,4326)
  
  df.int <- st_intersection(df_sub,cborders1960c_sub)
  
  national_highway_interstate.int <- st_intersection(national_highway_interstate,cborders1960c_sub)
  
  metro_dc.int <- st_intersection(metro_dc,cborders1960c_sub)
  
  # # draw the plot with legend at the bottom
  p <- ggplot(df.int) +
    geom_sf(aes(fill=brks),colour="white", size=0.3)+
    geom_sf(data = metro_dc.int, fill=NA, colour = "#de2d26", size=border_size)+
    geom_sf(data = cborders1960c_sub, fill=NA, colour = "#737373", size=border_size)+
    geom_sf(data = national_highway_interstate.int, fill=NA, colour = "#fc9272", size=border_size)+
    coord_sf() +
    theme_map() +
    #theme(legend.position = "bottom",legend.background = element_rect(color = NA))
    theme(legend.position = "none",legend.background = element_rect(color = NA))#+ 
  # geom_text(data = subset(df.int, brks==-1), #angle=90,
  #       aes(label = ZCTA, x = lon, y = lat), # << move each label down by 1 unit
  #      # position = position_dodge(0.9), hjust=3, #vjust=-0.5,
  #       color = "yellow", size = 5)
  
  
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
  
  #print(tester)
  
  
  df.int_sub <- df.int %>% dplyr::select("ZCTA","NAME","LSAD","STATE", "COUNTY","new_constructions","existing_constructions","new_existing_constructions_ratio",
                                         "share_single_family_new_constructions", "total_single_family_households_since_2000", "total_single_family_households",
                                         "CENSUSAREA","zcta_area","intersected_area","percent_intersected_area","brks")                                                                                                     
  
  st_geometry(df.int_sub) <- NULL
  
  #df.int_sub_NA_zctas <- df.int_sub %>% filter(brks==-1)
  
  if (colname_str=='new_constructions'){
    save_plot <- paste0(out_dir_county_maps,"c1.m1.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_county_maps_data,"c1.m1.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),
              row.names = F)
  } else if (colname_str=='new_existing_constructions_ratio'){
    save_plot <- paste0(out_dir_county_maps,"c1.m2.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_county_maps_data,"c1.m2.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),
              row.names = F)
  } else if (colname_str=='share_single_family_new_constructions'){
    save_plot <- paste0(out_dir_county_maps,"c2.m1.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.jpg")
    write.csv(df.int_sub,paste0(out_dir_county_maps_data,"c2.m1.",sfips,cfips,"_",dateo,"_acs_zcta_quantiles_of_",colname_str,"_all_zctas.csv"),
              row.names = F)
  } 

  
  print(save_plot)
  
  ggsave(save_plot,plot = tester, dpi = 300, width = 16, height = 11, units = c("in"))
  
}




col_vec <- c("new_constructions")

state_fips <- c("11","24","51","54")

#state_fips <- c("51")


# call the funtion to create plot for each variable
for (col in col_vec){
  for (sfips in state_fips){
    if(sfips=="00"){
      cfips <-  c("000")
      plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),sfips,cfips)
    } else if(sfips=="11"){
      cfips_vec <-  c("001")
      for (cfips in cfips_vec){
        plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),sfips,cfips)
      }
    } else if (sfips=="51"){
      cfips_vec <- c("013","043","047","059","061","107","153","157","177","179","187","510","600","610","630","683","685")
      #cfips_vec <- c("630","683","685")
      for (cfips in cfips_vec){
        plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),sfips,cfips)
      }
    } else if (sfips=="24"){
      cfips_vec <- c("009","017","021","031","033")
      for (cfips in cfips_vec){
        plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),sfips,cfips)
      }
    } else{
      cfips_vec <- c("037")
      for (cfips in cfips_vec){
        plot_zcta_level_quantile_values(zcta_2016,quo(!!sym(col)),sfips,cfips)
      }
    }
  }
}

