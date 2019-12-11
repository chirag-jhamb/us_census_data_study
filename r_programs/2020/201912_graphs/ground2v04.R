#################################################################
#
# this file tries to make graphs for state of the capital region 2020
#
# data are created in 
# /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2020/data_set_up
# create_dataset_all_years.R
#
# december 7, 2019
# december 8, 2019
# december 9, 2019
# december 10, 2019
#
# ground2v04.R 
#
##################################################################

####### A. basic set up stuff ##########################################

#### packages ######
require(tidyverse)
require(ggplot2)
require(scales)

#### set todays date #####
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

####### B. set gates ################################################

# set colors for all graphs #
gcolors <- TRUE

# modify dataset with things for many graphs #
cdata <- TRUE

#### age graphs #####
ageg1 <- FALSE
ageg2 <- FALSE

#### total population graphs ####
totpop <- TRUE

#### race graphs ################
race1 <- FALSE

#### household composition graphs ##############
hh1 <- FALSE

#### income graphs ################

#### population tables
pop.tab <- FALSE



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
  #orgd <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/20191203_data_all_years.csv")
  orgd <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2020/data_set_up/20191210_dataset_all_years.csv")
  print(names(orgd))
  checker <- orgd

  # make state code and county code
  orgd$statefips <- substr(orgd$FIPS,1,2)
  orgd$countyfips <- substr(orgd$FIPS,3,5)

  # make urban/suburban/exurban markers
  orgd$area_type <- ifelse((orgd$countyfips %in% c("001") & orgd$statefips %in% c("11"))|
                           (orgd$countyfips %in% c("013","510") & orgd$statefips %in% c("51")),"Urban",
                      ifelse((orgd$countyfips %in% c("033","031") & orgd$statefips %in% c("24"))|
                             (orgd$countyfips %in% c("059","600","610") & orgd$statefips %in% c("51")),"Suburban",
                             "Exurban"))
  # clear if msa
  orgd$area_type <- ifelse(orgd$level == "msa",NA,orgd$area_type)
  print(table(orgd$area_type))

  # create race shares
  orgd$s_white <- orgd$white_alone/orgd$total_population
  orgd$s_aa <- orgd$AA_alone/orgd$total_population
  orgd$s_hisp <- orgd$hispanic_or_latino/orgd$total_population

  # create shares by household size 
  #### this first row should be in dataset that i get. should be able to delete this line 
  orgd$hhs_tot <- orgd$household_size_1 + orgd$household_size_2 + orgd$household_size_3_to_4 + orgd$household_size_more_than_4
  orgd$hhs_1 <- orgd$household_size_1 / orgd$hhs_tot
  orgd$hhs_2 <- orgd$household_size_2 / orgd$hhs_tot
  orgd$hhs_3 <- orgd$household_size_3_to_4 / orgd$hhs_tot
  orgd$hhs_4 <- orgd$household_size_more_than_4 / orgd$hhs_tot

  ###### make just msa data #######

  msas <- orgd[which(orgd$level == "msa_level"),]
  print("just msas")
  print(dim(msas))

  ##### make just county data ######

  # load main data, keep only counties
  cnties <- orgd[which(orgd$level == "county_level"),]
  print("just counties")
  print(dim(cnties))

  ##### make urban/suburban/exurban data #######

  # aggregate to three types
  cnties <- group_by(.data = cnties, area_type, year)
  threet <- summarize(.data = cnties, total_population = sum(total_population),
  	    		              less_than_18 = sum(less_than_18),
				      X18_to_29 = sum(X18_to_29),
				      above_59 = sum(above_59),
				      white_alone = sum(white_alone),
				      AA_alone = sum(AA_alone),
				      hispanic_or_latino = sum(hispanic_or_latino))

} # end of cleaning / set up data gate


#############################################################################
#############################################################################
############## AGE GRAPHS ###################################################
#############################################################################
#############################################################################


############### graph 1: msas vs dc ########################################

if(ageg1){

  #### calculate population shares by age
  ## < 18
  msas$s_lt_18 <- msas$less_than_18/msas$total_population
  print(summary(msas$s_lt_18))
  ## > 59
  msas$s_gt_59 <- msas$above_59/msas$total_population
  print(summary(msas$s_gt_59))

  ## keep only relevant columns
  msa_sub <- msas[,c("msa_name","CBSA","year","s_lt_18","s_gt_59")]
  print(msa_sub)

  #### age < 18

  ### make a graph -- age < 18
  age.g1 <- ggplot() +
    geom_line(data = msa_sub[which(msa_sub$CBSA != 47900),],
    	      mapping = aes(x = year, y = s_lt_18, color = as.factor(CBSA))) +
    geom_line(data = msa_sub[which(msa_sub$CBSA == 47900),],
    	      mapping = aes(x = year, y = s_lt_18, color = as.factor(CBSA)),
	      size = 1.5) +
    labs(title = "share < 18")

  ### output graph
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	       "age1a_",
               dateo,
	       ".jpg")
  ggsave(filename = nm, 
       plot = age.g1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

  #### age > 59

  ### make a graph -- age > 59
  age.g1 <- ggplot() +
    geom_line(data = msa_sub[which(msa_sub$CBSA != 47900),],
    	      mapping = aes(x = year, y = s_gt_59, color = as.factor(CBSA))) +
    geom_line(data = msa_sub[which(msa_sub$CBSA == 47900),],
    	      mapping = aes(x = year, y = s_gt_59, color = as.factor(CBSA)),
	      size = 1.5) + 
    labs(title = "share > 59")


  ### output graph
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	       "age1b_",
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

} # end of age graph 1 and 2


############### graph 2: urban/exurban/suburban ########################################

if(ageg2){

  #### calculate population shares by age
  ## < 18
  threet$s_lt_18 <- threet$less_than_18/threet$total_population
  print(summary(threet$s_lt_18))
  ## 18 to 29
  threet$s_18_to_29 <- threet$X18_to_29/threet$total_population
  print(summary(threet$s_lt_18))
  ## > 59
  threet$s_gt_59 <- threet$above_59/threet$total_population
  print(summary(threet$s_gt_59))

  ## keep only relevant columns
  threet_sub <- msas[,c("msa_name","statefips","countyfips","year","s_lt_18","s_gt_59")]
  print(threet_sub)

  ## make area_type factor
  threet$area_type_f <- as.factor(threet$area_type)

  ##### make three graphs
  ggo <- function(varin,namer1,tit.text){

    ### make a graph -- age > 59
    age.g1 <- ggplot() +
      geom_line(data = threet,
    	        mapping = aes_string(x = "year", y = varin, color = "area_type_f")) +
      labs(title = tit.text)

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	         "age2",
		 namer1,
		 "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
           plot = age.g1,
       	   device = "jpg",
       	   width = 11,
       	   height = 8, 
       	   units = c("in"))
  } # end of function to make three graphs 

  ##### call function for three graphs 
  # share < 18
  ggo(varin = "s_lt_18",
      namer1 = "a",
      tit.text = "share lt age 18")
  # share 18 to 29
  ggo(varin = "s_18_to_29",
      namer1 = "b",
      tit.text = "share 18 to 29")
  # share > 59
  ggo(varin = "s_gt_59",
      namer1 = "c",
      tit.text = "share gt age 59")

  ##### output csv #####
  tout <- threet[,c("area_type","year","s_lt_18","s_18_to_29","s_gt_59")]
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/age/",
     	       "age2_",
               dateo,
	       ".csv")
  write.csv(x = tout,
  	    file = nm,
	    row.names = FALSE)

} # end of second set of age graphs #

#############################################################################
#############################################################################
############## TOTAL POPULATION GRAPHS -- FOR CHECKING ###################################################
#############################################################################
#############################################################################

if(totpop){

  ##### msas only ######

  ### make a graph -- total population
  age.g1 <- ggplot() +
    geom_line(data = msas[which(msas$CBSA != 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(CBSA))) +
    geom_line(data = msas[which(msas$CBSA == 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(CBSA)),
	      size = 1.5) +
    scale_y_continuous(labels = comma) +
    labs(title = "msas total population")

  ### output graph
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/total_population/",
     	       "totpop_msa_",
               dateo,
	       ".jpg")
  ggsave(filename = nm, 
       plot = age.g1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

  #### counties only ####

  # load main data, keep only counties
  cnties <- orgd[which(orgd$level == "county_level"),]
  print("just counties")
  print(dim(cnties))

  ### make a graph -- total population
  age.g1 <- ggplot() +
    geom_line(data = cnties,
    	      mapping = aes(x = year, y = total_population, color = as.factor(FIPS))) +
    scale_y_continuous(labels = comma) +
    labs(title = "county total population")

  ### output graph
  nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/total_population/",
     	       "totpop_cnty_",
               dateo,
	       ".jpg")
  ggsave(filename = nm, 
       plot = age.g1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

} # end of totpop gate 


############################################################################
########### table: msa and county population by year #######################
########### table: msa and county pop growth rate by year ##################
############################################################################

if(pop.tab){

}





#############################################################################
#############################################################################
############## RACE GRAPHS ###################################################
#############################################################################
#############################################################################


if(race1){

  #### msas #####

  #### make a function to run through msas
  rfunc <- function(areacode){

    print("inside function")
    print(paste0("areacode is ",areacode))

    ## make a a subset 
    msa_sub <- msas[which(msas$CBSA == areacode),]

    tito <- paste0("race for msa: white in blue, aa in black, hisp in red. CBSA is ",
    	           areacode)

    ### make a graph -- total population
    race1 <- ggplot() +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_white),
	      color = "blue") +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_aa)) +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_hisp),
	      color = "red") +
      labs(title = tito,
           ytitle = "share of population")

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/race/",
     	       "msa_",
	       areacode,
	       "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
       plot = race1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by msa 

  #### call function to make graphs by msa 

  ## make a list of msas 
  msa.list <- unique(msas$CBSA)

  ## lapply to this list 
  lapply(msa.list,rfunc)

  #### counties #####

  #### make a function to run through msas
  rfunc2 <- function(areacode){

    print("inside function")
    print(paste0("areacode is ",areacode))

    ## make a a subset (msa name, but counties)
    msa_sub <- cnties[which(cnties$FIPS == areacode),]
  
    ## put name into 1x1 thing
    county_namep <- msa_sub[which(msa_sub$year == 2000),c("county_name")]
    print("county name is ")
    print(county_namep) 
    tito <- paste0("race for county: white in blue, aa in black, hisp in red. County is ",
    	           county_namep,
		   ", code ",
		   areacode)

    ### make a graph -- total population
    race1 <- ggplot() +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_white),
	      color = "blue") +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_aa)) +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_hisp),
	      color = "red") +
      labs(title = tito,
           ytitle = "share of population")

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/race/",
     	       "county_",
	       areacode,
	       "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
       plot = race1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by county

  #### call function to make graphs by county

  ## make a list of counties
  cnty.list <- unique(cnties$FIPS)

  ## lapply to this list 
  lapply(cnty.list,rfunc2)

  #########################################
  ##### urban/suburban/exurban ############
  #########################################

  # create race shares
  threet$s_white <- threet$white_alone/threet$total_population
  threet$s_aa <- threet$AA_alone/threet$total_population
  threet$s_hisp <- threet$hispanic_or_latino/threet$total_population

  #### make a function to run through msas
  rfunc2 <- function(areacode){

    print("inside function")
    print(paste0("areacode is ",areacode))

    ## make a a subset (msa name, but counties)
    msa_sub <- threet[which(threet$area_type == areacode),]
  
    ## make graph title
    tito <- paste0("race for county: white in blue, aa in black, hisp in red. Land type is  ",
		   areacode)

    ### make a graph -- total population
    race1 <- ggplot() +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_white),
	      color = "blue") +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_aa),
	      color = "black") +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = s_hisp),
	      color = "red") +
      labs(title = tito,
           ytitle = "share of population")

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/race/",
     	       "land_type_",
	       areacode,
	       "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
       plot = race1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by area_type

  #### call function to make graphs by area_type

  ## make a list of counties
  area.list <- unique(threet$area_type)

  ## lapply to this list 
  lapply(area.list,rfunc2)
  

} # end of race1 gate -- county and msa shares by racial group


#############################################################################
#############################################################################
############## HOUSEHOLD COMPOSITION GRAPHS ###################################################
#############################################################################
#############################################################################


## household size share by msa and county 

if(hh1){

  ##### set up useful for all graphs ######################

  ### name the variables for graph ##
  vals <- c("hhs_1","hhs_2","hhs_3","hhs_4")
  hh.name <- c("hh w/ 1 person",
  	       "hh w/ 2 people",
	       "hh w/ 3 to 4 people",
	       "hh w/ 5 + ppl")


  ###### msas ###########################################3

  ### make msas long to do this ####
  msas2 <- gather(data = msas, 
  	   	  key = hhs_type,
		  value = hhs_share,
		  hhs_1:hhs_4)
  
  ### keep only relevant variables to avoid confusion ###
  msas2 <- msas2[,c("NAME","year","CBSA","hhs_type","hhs_share")]
  print(head(msas2))

  #### msas #####
###### START HERE WITH LABELS ####
  #### make a function to run through msas
  rfunc <- function(areacode){

    print("inside function")
    print(paste0("areacode is ",areacode))

    ## make a a subset 
    msa_sub <- msas2[which(msas2$CBSA == areacode),]

    ## get msa name into 1x1
    msa_name <- msa_sub[which(msa_sub$year == 2000),c("NAME")]

    tito <- paste0(msa_name,
    	           ": Share of HH by HH size")

    print("vals is ")
    print(vals)

    ### make a graph -- total population
    race1 <- ggplot() +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = hhs_share, color = hhs_type)) +
      scale_color_manual(value = vals,
			 labels = hh.name) +
      labs(title = tito,
           ytitle = "share of population")

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/household_size/",
     	       "msa_",
	       areacode,
	       "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
       plot = race1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by msa 

  #### call function to make graphs by msa 

  ## make a list of msas 
  msa.list <- unique(msas$CBSA)
  #msa.list <- "47900"

  ## lapply to this list 
  lapply(msa.list,rfunc)


  ###### counties ###########################################3

  ### make counties long to do this ####
  msas2 <- gather(data = cnties, 
  	   	  key = hhs_type,
		  value = hhs_share,
		  hhs_1:hhs_4)
  
  ### keep only relevant variables to avoid confusion ###
  msas2 <- msas2[,c("NAME","year","FIPS","hhs_type","hhs_share")]
  print(head(msas2))

  #### make a function to run through msas
  rfunc <- function(areacode){

    print("inside function")
    print(paste0("areacode is ",areacode))

    ## make a a subset 
    msa_sub <- msas2[which(msas2$FIPS == areacode),]

    ## get msa name into 1x1
    msa_name <- msa_sub[which(msa_sub$year == 2000),c("NAME")]

    tito <- paste0(msa_name,
    	           ": Share of HH by HH size")

    ### make a graph -- total population
    race1 <- ggplot() +
      geom_line(data = msa_sub,
    	      mapping = aes(x = year, y = hhs_share, color = hhs_type)) +
      labs(title = tito,
           ytitle = "share of population")

    ### output graph
    nm <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/household_size/",
     	       "cnty_",
	       areacode,
	       "_",
               dateo,
	       ".jpg")
    ggsave(filename = nm, 
       plot = race1,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by msa 

  #### call function to make graphs by msa 

  ## make a list of counties
  cnty.list <- unique(cnties$FIPS)
  #msa.list <- "47900"

  ## lapply to this list 
  lapply(cnty.list,rfunc)


} # end of household composition by size  