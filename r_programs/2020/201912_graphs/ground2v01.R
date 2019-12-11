#################################################################
#
# this file tries to make graphs for state of the capital region 2020
#
# data are created in 
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2020/data_set_up
# create_dataset_all_years.R
#
# december 7, 2019
#
# ground2v01.R 
#
##################################################################

####### A. basic set up stuff ##########################################

#### packages ######
require(tidyverse)
require(ggplot2)

#### set todays date #####
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

####### B. set gates ################################################

# set colors for all graphs #
gcolors <- TRUE

# modify dataset with things for many graphs #
cdata <- TRUE

#### age graphs #####
ageg1 <- TRUE

#####################################################################
###### gcolors : set colors for all graphs #######################
#####################################################################


#####################################################################
###### cdata: clean data with things needed for many graphs #########
#####################################################################

if(cdata){

  # these data are created in  
  # /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2020/data_set_up
  # create_dataset_all_years.R 
  orgd <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/20191203_data_all_years.csv")
  print(names(orgd))


} # end of cleaning / set up data gate


#############################################################################
#############################################################################
############## AGE GRAPHS ###################################################
#############################################################################
#############################################################################


############### graph 1: msas vs dc ########################################

if(ageg1){

  # load main data, keep only MSAs
  msas <- orgd[which(orgd$level == "msa"),]
  print("just msas")
  print(dim(msas))

  #### calculate population shares by age
  ## < 18
  msas$s_lt_18 <- msas$X18_to_29/msas$total_population
  print(summary(msas$s_lt_18))
  ## > 59
  msas$s_gt_59 <- msas$above_59/msas$total_population
  print(summary(msas$s_gt_59))

  ## keep only relevant columns
  msa_sub <- msas[,c("msa_name","CBSA","year","s_lt_18","s_gt_59")]
  print(msa_sub)

  ### make a graph
  age.g1 <- ggplot() +
    geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_lt_18, color = as.factor(CBSA)))

  ### output graph
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	       "age1_",
               dateo,
	       ".jpg")
  ggsave(filename = nm, 
       plot = age.g1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

  ### output csv
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	       "age1_",
               dateo,
	       ".csv")
  write.csv(x = msa_sub,
  	    file = nm,
	    row.names = FALSE)



} # end of age graph 1