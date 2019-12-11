#####################################################################

# this program takes the CPI data
# january 28, 2019
# acs_county_CPI_data_v01.R

##############################################################################

##### A. start-up and set-up
library(dplyr)
library(data.table)


# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

cpi_data <- read.csv(paste0(groupDir,"/center_for_washington_area_studies/data/consumer_price_index/2018/","CPI_data.csv"),
                     sep = "\t",stringsAsFactors = F)


cpi_data$footnote_codes <- NULL

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# remove trailing white space from series id column
#https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace

cpi_data$series_id <- trim.trailing(cpi_data$series_id)

cpi_data_all_areas_old_base <- cpi_data %>% filter(series_id=="CUUR0100AA0")

cpi_data_all_areas_old_base_avg_cpi <- cpi_data_all_areas_old_base %>% 
                                                  filter(period!="M13") %>% 
                                                              group_by(year) %>% 
                                                                summarise(avg_cpi=mean(value)) %>% as.data.frame()


write.csv(cpi_data_all_areas_old_base_avg_cpi,
          paste0(groupDir,"/center_for_washington_area_studies/data/consumer_price_index/2018/","avg_cpi.csv"),row.names = F)
