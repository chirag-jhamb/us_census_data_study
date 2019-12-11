#####################################################################
# this program takes the
# 2017 ZCTAs: /groups/brooksgrp/maps/united_states/census2010/zcta/2017_census_zcta_shapefile/
# and intersect it with
# 2000 ZCTAs: /groups/brooksgrp/maps/united_states/census2000/zip_code_tabulation_areas

# zcta_2000_to_2017.R
# Created by: deepak
# Created on: 12/12/18
##############################################################################
Importing the required packages
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



