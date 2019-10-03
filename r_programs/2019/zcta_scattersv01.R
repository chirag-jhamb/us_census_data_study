##############################################################
#
# this program tries to make some scatter plots by 
# zcta for the dc area
#
# december 18, 2018
#
# zcta_scattersv01.R
#
##################################################################

##### A. load stuff 

# Importing the required packages
library(dplyr)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(sf)

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo


##### B. load data 

zs <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data/20181217_state_county_zcta_data.csv")

zs2 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data/20181217_state_county_zcta.csv")

zs3 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data/acs_zcta_2012_2016.csv")

names(zs3)

##### C. histograms

# C.1. create some variables

zs3$population.density <- (zs3$total_total_population/zs3$zcta_area)*1000
zs3$housing.density <- (zs3$total_housing_units/zs3$zcta_area)*1000

h1 <- ggplot(zs3, aes(x=population.density)) +
      geom_histogram(binwidth=0.1)
ggsave(filename=paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/",dateo,"_zcta_housing_density.jpg"),
	plot=h1,
	device = "jpeg")

h2 <- ggplot(zs3, aes(x=housing.density)) +
      geom_histogram(binwidth=0.1)
ggsave(filename = paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/",dateo,"_zcta_housing_density.jpg"),
       plot = h2,
       device = "jpeg")




