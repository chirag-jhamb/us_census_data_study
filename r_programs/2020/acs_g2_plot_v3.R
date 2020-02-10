# Plot g2:  Stacked area graph showing population by level
# Author: Chirag Jhamb
################################################################################
# import libraries required:
library(tidyverse)
library(haven)
library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(sf)
library(stringr)
library(tidyr)


####### GATES ############

#### introduction #####
gate_intro_g2 = FALSE
gate_intro_g3 = FALSE

#### chapter 1 #####
gate_ch01_g1 = FALSE
gate_ch01_g3 = FALSE
gate_ch01_g4 = FALSE

#### chapter 2 ####
gate_ch01_map = FALSE
gate_ch02_g4 = FALSE

#### chapter 3 ####
# cleans data #
gate_ch03_inccln = FALSE
gate_ch03_g1 = TRUE
gate_ch03_g2 = TRUE
gate_ch03_g3 = TRUE


#for proper numerical representation in graphs:
options(scipen=5)
# date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

####################################################################################
#### load data #####################################################################
####################################################################################

df <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/20191212_dataset_all_years.csv")
dmv_subset <- subset(df, level=="county_level")
# list of CBSAs to plot
sub_cbsa <- c(12060,14460,26420,33100,37980,47900)
# MSA level dataset:
msa_sub <- subset(df,level=="msa_level")
msa_sub <- msa_sub[msa_sub$CBSA %in% sub_cbsa,]
# msa_sub <- msa_sub[c("total_population","NAME", "level")]
dmv_subset$COUNTY <- substr(dmv_subset$FIPS, 3,5)
dmv_subset$STATE <- substr(dmv_subset$FIPS, 0,2)
dmv_subset <- dmv_subset %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                             (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                           ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                    (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                  "Exurban")))

# dmv_subset_graph <- dmv_subset[c("area_type","year","total_population")]
# sum all area_type, year pairs using group by:
dmv_subset_graph <- dmv_subset %>% group_by(area_type, year) %>% summarise(total_population = sum(total_population))
groupDir <- "/groups/brooksgrp"
out_dir_intro <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/introduction/introduction_")
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/ch01/ch01_")
out_dir_ch02 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/ch02/ch02_")
out_dir_ch03 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/ch03/ch03_")
if (gate_intro_g2){
  # Save dataset into a csv:
  save_path <- paste0(out_dir_intro,"g2_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(dmv_subset_graph, file = save_path, row.names = FALSE)

  p <- ggplot() +
        geom_area(data = dmv_subset_graph,
                  mapping = aes(x = year, y =total_population,fill=area_type))
  save_path <- paste0(out_dir_intro,"g2_",dateo,".jpg")
  ggsave(save_path, plot = p, dpi = 300, width = 16, height = 11, units = c("in"))
}


####################################################################################
#### introduction #####################################################################
####################################################################################

# compare DC MSA population growth to other areas
if (gate_intro_g3){
  save_path <- paste0(out_dir_intro,"g3_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(msa_sub, file = save_path, row.names = FALSE)
  intro_g3 <- ggplot() +geom_line(data = msa_sub[which(msa_sub$CBSA != 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(NAME))) +
            geom_line(data = msa_sub[which(msa_sub$CBSA == 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(NAME)),
	      size = 1.5)
  save_path <- paste0(out_dir_intro,"g3_",dateo,".jpg")
  ggsave(save_path, plot = intro_g3, dpi = 300, width = 16, height = 11, units = c("in"))
}

####################################################################################
#### chapter 1: race #####################################################################
####################################################################################

if (gate_ch01_g1){
  # 3 panel: each graph has urban/suburban/exurban By % hispanic, % aa, % white (see image). Make all graphs have the same y axis
  dmv_race_subset_df <- dmv_subset[c("white_alone" ,"AA_alone" ,"hispanic_or_latino","total_population","area_type","year")]
  # sum all area_type, year pairs using group by:
  dmv_race_subset <- dmv_race_subset_df %>% group_by(area_type, year) %>% summarise(total_population = sum(total_population))
  to_add_cols <- c("white_alone" ,"AA_alone" ,"hispanic_or_latino")
  # 1970 data missing, so delete 1970 to avoid NULL values:
  dmv_race_subset <- subset(dmv_race_subset, year!=1970)

  for (k in to_add_cols){
    # sum all area_type, year pairs using group by:
    df_temp <- dmv_race_subset_df %>% group_by(area_type, year) %>% summarise(tempo = sum(!!sym(k)))
    colnames(df_temp)[which(colnames(df_temp) == "tempo")] <- k
    dmv_race_subset <- merge(dmv_race_subset, df_temp, all.x=TRUE)
    print(dim(dmv_race_subset))
    dmv_race_subset[k] <- 100*dmv_race_subset[k]/dmv_race_subset$total_population
    print("\n")
  }

  areas <- c("Urban","Suburban","Exurban")
  ## Saving the csv:
  save_path <- paste0(out_dir_ch01,"g1_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(dmv_race_subset, file = save_path, row.names = FALSE)
  # loop over each area type:
  for (ar in areas){
    area_race_df <- subset(dmv_race_subset, area_type==ar)
    df <- area_race_df %>%
      select(year, white_alone,AA_alone,hispanic_or_latino) %>%
      gather(key = "variable", value = "value", -year)

    race_g1 <- ggplot(df, aes(x = year, y = value)) +
      geom_line(aes(color = variable)) +
      scale_color_manual(values = c("red", "blue", "black"))+scale_y_continuous(breaks=c(0,20,40,60,80), labels=c(0,20,40,60,80),limits=c(0, 90))
    save_path <- paste0(out_dir_ch01,"g1_",ar,"_",dateo,".jpg")
    print(save_path)
    ggsave(save_path, plot =race_g1, dpi = 300, width = 16, height = 11, units = c("in"))
  }
}

if (gate_ch01_g3){
  # One graph: Show change over time in charles and pg county in % aa
  ch01_g3 = dmv_subset[dmv_subset$FIPS %in% c(24017,24033),]
  ch01_g3$aa_share <- ch01_g3$AA_alone/ch01_g3$total_population
  # get rid of null values
  ch01_g3 <- subset(ch01_g3, ch01_g3$year!=1970)
  ## Saving the csv:
  save_path <- paste0(out_dir_ch01,"g3_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(ch01_g3, file = save_path, row.names = FALSE)
  # plot the graph
  ch01_g3_plot <- ggplot() +geom_line(data = ch01_g3,
    	      mapping = aes(x = year, y = aa_share, color = as.factor(county_name)))
  save_path <- paste0(out_dir_ch01,"g3_",dateo,".jpg")
  ggsave(save_path, plot =ch01_g3_plot, dpi = 300, width = 16, height = 11, units = c("in"))
}

if (gate_ch01_g4){
  # One graph: Just urban jurisdictions (alexandria, arlington) , share hispanic % change over time
  ch01_g4 = dmv_subset[dmv_subset$FIPS %in% c(51510,11001,51013),]
  ch01_g4$hispanic_share <- ch01_g4$hispanic_or_latino/ch01_g4$total_population
  # get rid of null values
  ch01_g4 <- subset(ch01_g4, ch01_g4$year!=1970)
  ## Saving the csv:
  save_path <- paste0(out_dir_ch01,"g4_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(ch01_g4, file = save_path, row.names = FALSE)
  # plot the graph
  ch01_g4_plot <- ggplot() +geom_line(data = ch01_g4,
    	      mapping = aes(x = year, y = hispanic_share, color = as.factor(county_name)))
  save_path <- paste0(out_dir_ch01,"g4_",dateo,".jpg")
  ggsave(save_path, plot =ch01_g4_plot, dpi = 300, width = 16, height = 11, units = c("in"))
}


if (gate_ch01_map){
  source("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_programs/2020/acs_ch01_map_plot.R", echo=T)
  col_names <- c("white_alone", "AA_alone", "hispanic_or_latino")
  for (col in col_names){
    save_path <- paste0(out_dir_ch01,"map_roads_",col,"_",dateo,".jpg")
    plot_quantile_map(colname=col, fname=save_path, exurban = F, roads = T)
    save_path <- paste0(out_dir_ch01,"map_",col,"_",dateo,".jpg")
    plot_quantile_map(colname=col, fname=save_path, exurban = F, roads = F)
  }
}

####################################################################################
#### chapter 2: household type #####################################################################
####################################################################################

if (gate_ch02_g4){
  ch02_g4 <- dmv_subset[c("household_with_kids","family_household_without_kids","non_family_household_without_kids","area_type","year")]
  # get rid of null values
  ch02_g4 <- subset(ch02_g4, ch02_g4$year!=1970)
  main_cols <- c("household_with_kids","family_household_without_kids","non_family_household_without_kids")
  # sum all area_type, year pairs using group by:
  ch02_g4 <- ch02_g4 %>%group_by(area_type, year) %>%summarise_each(funs(sum))
  ch02_g4$total_test <- rowSums(ch02_g4[main_cols])

  for (i in main_cols){
    ch02_g4[[i]] <- 100*ch02_g4[[i]]/ch02_g4$total
    save_path <- paste0(out_dir_ch02,"g4_",i,"_",dateo,".jpg")
    print(save_path)
    ch02_g4_plot <-ggplot(ch02_g4, aes(x = year, y = get(i))) +geom_line(aes(color = area_type))+scale_y_continuous(breaks=c(0,25,50,75), labels=c(0,25,50,75),limits=c(0, 75))
    ggsave(save_path, plot =ch02_g4_plot, dpi = 300, width = 16, height = 11, units = c("in"))
  }
  ## Saving the csv:
  save_path <- paste0(out_dir_ch02,"g4_data_",dateo,".csv")
  print("Saving to CSV")
  print(save_path)
  write.table(ch02_g4, file = save_path, row.names = FALSE)
}

if(gate_ch02_g1){

  ## keep only relevant columns
  # msa_sub <- msas[,c("NAME","CBSA","year","total_population","s_lt_18","s_gt_59")]
  ## rank msas by population to keep top 5
  msa_sub <- subset(df,level=="msa_level")
  msa_sub <- group_by(.data = msa_sub, year)
  msa_sub <- mutate(msa_sub, year_pop_rank = rank(-total_population))
  ## put 2017 rank for all years
  msa_sub <- group_by(.data = msa_sub, CBSA)
  msa_sub$pr2017 <- ifelse(msa_sub$year == 2017, msa_sub$year_pop_rank, NA)
  print(head(msa_sub))
  # max of 2017 rank is all rank
  msa_sub <- mutate(msa_sub, pr2017a = max(pr2017,na.rm = TRUE))

  ### just keep five largest
  msa_sub5 <- filter(.data = msa_sub, pr2017a >= 5)

  #### age < 18

  ### make a graph -- age < 18
  msa_sub5$less_than_18 <- msa_sub5$less_than_18/msa_sub5$total_population
  age.g1 <- ggplot() +
    geom_line(data = msa_sub5[which(msa_sub5$CBSA != 47900),],
    	      mapping = aes(x = year, y = less_than_18, color = as.factor(NAME))) +
    geom_line(data = msa_sub5[which(msa_sub5$CBSA == 47900),],
    	      mapping = aes(x = year, y = less_than_18, color = as.factor(NAME)),
	      size = 1.5) +
    labs(title = "share < 18")
  save_path <- paste0(out_dir_ch02,"g1_less_than_18_",dateo,".jpg")
  print(save_path)
  ggsave(save_path, plot =age.g1, dpi = 300, width = 16, height = 11, units = c("in"))

  ### make a graph -- age > 59
  msa_sub5$above_59 <- msa_sub5$above_59/msa_sub5$total_population
  age.g1 <- ggplot() +
    geom_line(data = msa_sub5[which(msa_sub5$CBSA != 47900),],
    	      mapping = aes(x = year, y = above_59, color = as.factor(NAME))) +
    geom_line(data = msa_sub5[which(msa_sub5$CBSA == 47900),],
    	      mapping = aes(x = year, y = above_59, color = as.factor(NAME)),
	      size = 1.5) +
    labs(title = "share above_59")
  save_path <- paste0(out_dir_ch02,"g1_above_59_",dateo,".jpg")
  print(save_path)
  ggsave(save_path, plot =age.g1, dpi = 300, width = 16, height = 11, units = c("in"))
}


####################################################################################
#### chapter 3: income #####################################################################
####################################################################################

if(gate_ch03_inccln){

  ###### load cpi #################################

  # cpi file
  cpinm <- "/groups/brooksgrp/consumer_price_index/all_urban_consumers/through_2018/cu.data.1.AllItems.txt"

  # read tab delimited text file
  cpi <- read.table(file = cpinm,
      	            header = TRUE,
		    sep = "\t")

  # limit file to relevant years and codes #
  cpis <- filter(.data = cpi, str_trim(as.character(series_id)) == "CUSR0000SA0" & substr(str_trim(as.character(cpi$period)),1,1) == "M")
  print(head(cpis))

  # make annual average
  cpis <- group_by(.data = cpis,year)
  cpi.annual <- summarize(.data = cpis, cpi.ann = mean(value))
  print(cpi.annual)

  # get 2018 value into all values of a variable
  c2018 <- filter(cpi.annual, year == 2018)
  cpi.annual$o2018 <- ifelse(cpi.annual$year == 2018, cpi.annual$cpi.ann, -9)
  cpi.annual <- mutate(.data = cpi.annual, v2018 = max(o2018, na.rm. = TRUE))
  print(head(cpi.annual))

  ## just keep relevant years
  cpi.annual <- filter(cpi.annual, year %in% c(1990,2000,2013,2017))
  print(cpi.annual)

  ###### load block group data ###########################################################

  #### acs block group data ####

  ### load block group income data ###
  # these data for years 2008-2012 and 2013-2017 are created in
  # /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/create_block_group.py
  # read.csv gives a lot of trouble loading character variables, so use read_csv()
  bg1 <- read_csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/block_group_data/20200123_block_group_tract_data.csv",
      	   col_types = cols(
     blockgrp = col_character(),
     tract = col_character(),
     statefips = col_character(),
     countyfips = col_character(),
     B19013_Median_household_income_in_the_past_12_months_ = col_double(),
     B01003_Total = col_double(),
     start_year = col_double(),
     end_year = col_double(),
     land_area = col_double()
   ))

  print("this is just-loaded bg1")
  print(str(bg1))
  # set variable year to last year of 5-year period
  bg1$year <- bg1$end_year
  # make bg_med_inc variable
  bg1$bg_med_inc <- bg1$B19013_Median_household_income_in_the_past_12_months_
  # make population variable
  bg1$pop <- bg1$B01003_Total
  # need to make state/county/tract/blkgrp into character variables
  bg1$blkgrp <- bg1$blockgrp
  # get rid of start and end year variables and other junk
  bg1 <- bg1[, !(names(bg1) %in% c("start_year","end_year","B19013_Median_household_income_in_the_past_12_months_",
      	       		    	   "blockgrp","B01003_Total"))]

  ### 1990 and 2000 block group data

  # these data are created in
  # /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_programs/2020/load_block_group_census/stackyearsv07.sas
  bg9020 <- read_sas("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_block_grp_census/bgs_1990_2000_dmv_20200121.sas7bdat")
  print("names from 90/2000 file upon loading")
  print(names(bg9020))

  ## fix bgs for 1990 and 2000 to align with acs dataframe
  # countyfips needs leading zeros
  # find length of countyfips
  bg9020$leno <- str_length(str_trim(bg9020$countyfips))
  # adjust with padding as needed
  bg9020$countyfips <- ifelse(bg9020$leno == 1, paste0("00",str_trim(bg9020$countyfips)),
  		         ifelse(bg9020$leno == 2, paste0("0",str_trim(bg9020$countyfips)),bg9020$countyfips))
  print(head(bg9020))
  # get rid of length variable
  bg9020 <- bg9020[,!(names(bg9020) == "leno")]

  # set marker variable for merge below
  bg9020$inbgs <- 1

  # add in padding zeros if needed for tract id
  # find length of tract
  bg9020$leno <- str_length(str_trim(bg9020$tract))
  # adjust with padding as needed
  bg9020$tract <- ifelse(bg9020$leno == 4, paste0(str_trim(bg9020$tract),"00"),
  		    ifelse(bg9020$leno == 5, paste0(str_trim(bg9020$tract),"0"),
		      ifelse(bg9020$leno == 6, bg9020$tract, bg9020$tract)))
  print(head(bg9020))
  # get rid of length variable
  bg9020 <- bg9020[,!(names(bg9020) == "leno")]

  print("this is organization for 1990/2000")
  print(head(bg9020))

  ######### add in land area from 1990 and 2000 census block groups #########

  #### 1990 data first

  ### load 1990 block group file map
  df1990 <- st_read("/groups/brooksgrp/maps/united_states/census1990/block_groups/nhgis0025_shape/US_blck_grp_1990.shp")

  ### calculate area
  df1990$land_area <- st_area(df1990)

  ## get rid of geometry
  st_geometry(df1990) <- NULL

  ## get year, state, tract, block group ids
  df1990$year <- 1990
  df1990$statefips <- substr(as.character(df1990$FIPSSTCO),1,2)
  df1990$countyfips <- substr(as.character(df1990$FIPSSTCO),3,5)
  df1990$tract <- as.character(df1990$TRACT)
  df1990$blkgrp <- as.character(df1990$GROUP)

  ### check it
  print("this is 1990 after clean-up")
  print(head(df1990))

  ### keep just relevant states and variables
  df1990 <- filter(.data
= df1990, statefips %in% c("11","24","51","54"))
  df1990 <- df1990[,c("year","statefips","countyfips","tract","blkgrp","land_area")]

  #### 2000 data #####

  ### load data 2000 map to find area
  df2000 <- st_read("/groups/brooksgrp/maps/united_states/census2000/block_groups/nhgis0026_shape/US_blck_grp_2000.shp")
  print(names(df2000))
  print(head(df2000))

  ### calculate area
  df2000$land_area <- st_area(df2000)

  ### get rid of geometry
  st_geometry(df2000) <- NULL

  ### get year, state, county, tract, block group of out file
  df2000$year <- 2000
  df2000$statefips <- substr(as.character(df2000$FIPSSTCO),1,2)
  df2000$countyfips <- substr(as.character(df2000$FIPSSTCO),3,5)
  df2000$tract <- as.character(df2000$TRACT)
  df2000$blkgrp <- as.character(df2000$GROUP)

  ### keep just relevant states and variables
  df2000 <- filter(.data = df2000, statefips %in% c("11","24","51","54"))
  df2000 <- df2000[,c("year","statefips","countyfips","tract","blkgrp","land_area")]

  #### stack 1990 and 2000 data
  lands <- rbind(df1990,df2000)
  lands$inland <- 1

  ##### merge land area into dataframe with data
  bg9020v2 <- merge(x = bg9020,
  	            y = lands,
		    by = c("year","statefips","countyfips","tract","blkgrp"),
		    all = TRUE)
  # make nas from merge zeros
  bg9020v2$inland <- ifelse(is.na(bg9020v2$inland) == TRUE,0,bg9020v2$inland)
  bg9020v2$inbgs <- ifelse(is.na(bg9020v2$inbgs) == TRUE,0,bg9020v2$inbgs)

  # check on merge status
  print(table(bg9020v2$inland,bg9020v2$inbgs))

  # just keep ones that have block group data
  bg9020v2 <- filter(.data = bg9020v2, inbgs == 1 & inland == 1)

  # make sure that merged dataset has original dimensions and stop if not
  print("check condition that merged dataset has same dimension as original one")
  print(paste0("original dataset has ",dim(bg9020)[1]," rows"))
  print(paste0("merged dataset has ",dim(bg9020v2)[1]," rows"))
  stopifnot(dim(bg9020v2)[1] == dim(bg9020)[1])

  # now drop marker variables from merge
  bg9020v2 <- bg9020v2[, !(names(bg9020v2) %in% c("inland","inbgs"))]

  ### put two block group datasets together ####
  print(names(bg9020v2))
  print(names(bg1))

  # rbind them!
  bgs <- rbind(bg9020v2,bg1)

  ### calculate population density and check ###
  bgs$pop.den <- bgs$pop / bgs$land_area
  summary(bgs$pop.den)
  ## check by year
  bgs <- group_by(bgs, year)
  popdbyyear <- summarize(.data = bgs, pop.mean = mean(pop, na.rm = TRUE),
  	     			       pop.d.mean = mean(pop.den, na.rm = TRUE),
  	     			       land.area.mean = mean(land_area, na.rm = TRUE),
				       la.miss = mean(is.na(land_area)))
  print(popdbyyear)

  ## create pop.den * 1000
  bgs$pop.den.1000 <- bgs$pop.den * 1000

  ############# merge in cpi data #######################################

  # set up the merge
  bgsc <- merge(x = bgs, y = cpi.annual,
                by.x = "year", by.y = "year", all = TRUE)
  print(dim(bgsc))

  # convert to real dollars
  bgsc$bg_med_inc_real <- bgsc$bg_med_inc / (bgsc$cpi.ann/bgsc$v2018)

  # check that it doesnt look crazy
  checker <- group_by(.data = bgsc, year)
  checker <- summarize(checker, mean_real_inc = mean(bg_med_inc_real), mean_nom_inc = mean(bg_med_inc) )
  print("means for real income by decade")
  print(checker)

  ####### state and county variable ########

  ### make state and county variable
  bgsc$st.cnty <- paste0(bgsc$statefips,bgsc$countyfips)

  ########## mark urban or suburban ####################

  ### mark urb/sub/exurb
  # better to have affirmative definition of stuff here for exurban #
  bgsc$area_type <-
    ifelse(  bgsc$statefips == "11" |
            (bgsc$statefips == "51" & bgsc$countyfips %in% c("013","510")), "Urban",
      ifelse((bgsc$statefips == "24" & bgsc$countyfips %in% c("033","031")) |
	     (bgsc$statefips == "51" & bgsc$countyfips %in% c("059","600","610")), "Suburban",
        ifelse((bgsc$statefips == "24" & bgsc$countyfips %in% c("009","017","021")) |			       
               (bgsc$statefips == "51" & bgsc$countyfips %in% c("043","047","059","061","069",
			       	       	 		   "107","153","157","177","179",
                                                   	   "187","600","610","630",
							   "683","685","840")) |
               (bgsc$statefips == "54" & bgsc$countyfips %in% c("037")), "Exurban", "Trouble")))
  print(table(bgsc$area_type))

  # my house?
  my.house <- bgsc[which(bgsc$statefips == "11" & bgsc$countyfips == "001" &
  	             	 bgsc$tract == "001600" & bgsc$blkgrp == "3"),]
  my.house


  ####### set missing values for income to zero when population is zero ####

  bgsc$bg_med_inc_real <- ifelse(bgsc$pop == 0, NA, bgsc$bg_med_inc_real)

  ####### save for later use ##################################

  # where to save dataset
  fn <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/interim_datasets/tract_income_",
               dateo,".rds")
  saveRDS(object = bgsc,
          file = fn)

}


##### ch3, g1: income: income distribution for full msa, 1990 and 2017 #############

if(gate_ch03_g1){

  #### input data 
  # set filename to read
  fn <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/interim_datasets/tract_income_",
               dateo,".rds")
  # bring in data
  bgsc <- readRDS(fn)

  ## look into no income block groups in 1990
  noinc <- filter(.data = bgsc, bgsc$year == 1990 & bgsc$bg_med_inc_real == 0)

  ## limit data to 1990 and 2017 for ease of graphing and output
  bgsc <- filter(.data = bgsc, year == 1990 | year == 2017)

  ## make graph of full msa, 1990 and latest year 
  dcg <- ggplot() +
      geom_density(data = bgsc,
                   mapping = aes(x = bg_med_inc_real, color = as.factor(year))) +
      scale_x_continuous(labels = comma) +
      labs(title = "real median income over time dc msa",
           x = "real median income by block group",
	   y = "share of block groups")

  ## save the graph 
  save_path <- paste0(out_dir_ch03,"g1_msa_income_",dateo,".jpg")
  ggsave(save_path, 
         plot = dcg,
	 dpi = 300, 
	 width = 16, 
	 height = 11, 
	 units = c("in"))

  ## output data csv
  bgsc <- bgsc[,c("statefips","countyfips","tract","blkgrp","area_type","pop","bg_med_inc_real")]
  save_path <- paste0(out_dir_ch03,"g1_msa_income_",dateo,".csv")
  write.csv(bgsc, save_path)

}  # end of chapter 3, graph 1


##### income g2: income distribution by area type, 1990 and 2017 #############

if(gate_ch03_g2){

  # for each area type, show 2 years distribution on graph 

  #### input data 
  # set filename to read
  fn <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/interim_datasets/tract_income_",
               dateo,".rds")
  # bring in data
  bgsc <- readRDS(fn)

  #### make a function to run through area types
  rfunc <- function(areacode){

    ## make a a subset to just the relevant area type
    bgsc_sub <- bgsc[which(bgsc$area_type == areacode),]

    ## make a subset to the relevant years 
    bgsc_sub <- filter(.data = bgsc_sub, year == 1990 | year == 2017)

    ## make the graph 
    dcg <- ggplot() +
      geom_density(data = bgsc_sub,
            mapping = aes(x = bg_med_inc_real, color = as.factor(year))) +
      scale_x_continuous(labels = comma,
      			 limits = c(0,300000),
			 breaks = seq(0,300000,100000)) +
      scale_y_continuous(limits = c(0,0.000018)) +
      labs(title = paste0(areacode, "counties: real median income 1990 and 2017"),
           y = "real median income by block group")

    ### output graph
    nm <- paste0(out_dir_ch03,
		"g2_",
		areacode,
		"_",
                dateo,
	        ".jpg")

    ggsave(filename = nm, 
       plot = dcg,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by msa 

  #### call function to make graphs by msa 

  ## make a list of counties
  at.list <- unique(bgsc$area_type)

  ## lapply to this list 
  lapply(at.list,rfunc)

} # end of income, graph 2 


##### income g3: income distribution for particular jurisdictions, 1990 and 2017 #############

if(gate_ch03_g3){

  #### input data 
  # set filename to read
  fn <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/interim_datasets/tract_income_",
               dateo,".rds")
  # bring in data
  bgsc <- readRDS(fn)

  # select relevant jurisdictions 
  bgsc <- filter(.data = bgsc,
                 (statefips == "11") |
		 (statefips == "51" & countyfips %in% c("683","630")))

  #### make a function to run through area types
  rfunc <- function(areacode){

    ## make a a subset to just the relevant area type
    bgsc_sub <- bgsc[which(bgsc$st.cnty == areacode),]

    ## make a subset to the relevant years 
    bgsc_sub <- filter(.data = bgsc_sub, year == 1990 | year == 2017)

    ## make the graph 
    dcg <- ggplot() +
      geom_density(data = bgsc_sub,
            mapping = aes(x = bg_med_inc_real, color = as.factor(year))) +
      scale_x_continuous(labels = comma,
      			 limits = c(0,300000),
			 breaks = seq(0,300000,100000)) +
      scale_y_continuous(limits = c(0,0.0000375)) +
      labs(title = paste0(areacode, ": real median income 1990 and 2017"),
           y = "real median income by block group")

    ### output graph
    nm <- paste0(out_dir_ch03,
		"g3_",
		areacode,
		"_",
                dateo,
	        ".jpg")

    ggsave(filename = nm, 
       plot = dcg,
       device = "jpg",
       width = 11,
       height = 8, 
       units = c("in"))

    } # end of function to do graphs by msa 

  #### call function to make graphs by msa 

  ## make a list of counties
  at.list <- unique(bgsc$st.cnty)

  ## lapply to this list 
  lapply(at.list,rfunc)
  

}