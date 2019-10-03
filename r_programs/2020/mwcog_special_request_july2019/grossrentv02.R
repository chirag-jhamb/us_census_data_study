##################################################################
#
# this program makes a picture of median gross rent for dc-area
# counties, 1980 to the present
#
# data are downloaded from nhgis special for this
#
# received speciail request from COG
#
# june 30, 2019
# july 1, 2019
#
# grossrentv02.R
#
##########################################################################


##### A. start up stuff #################################################

library(tidyverse)

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo


##### B. load and stack data #######################################################

# 1980
d1980 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds107_1980_county.csv")
names(d1980)
d1980 <- d1980[,c("STATEA","COUNTYA","YEAR","DFK001")]
colnames(d1980)[colnames(d1980)=="DFK001"] <- "median_gross_rent"
head(d1980)

# 1990
d1990 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds123_1990_county.csv")
names(d1990)
d1990 <- d1990[,c("STATEA","COUNTYA","YEAR","EYU001")]
colnames(d1990)[colnames(d1990)=="EYU001"] <- "median_gross_rent"
head(d1990)

# 2000
d2000 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds151_2000_county.csv")
names(d2000)
d2000 <- d2000[,c("STATEA","COUNTYA","YEAR","GBO001")]
colnames(d2000)[colnames(d2000)=="GBO001"] <- "median_gross_rent"
head(d2000)

### ACS
## 2009-2013
d2011 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds195_20095_2009_county.csv")
names(d2011)
d2011$YEAR <- 2011
d2011 <- d2011[,c("STATEA","COUNTYA","YEAR","RRUE001")]
colnames(d2011)[colnames(d2011)=="RRUE001"] <- "median_gross_rent"
head(d2011)
## 2010-2014
d2012 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds176_20105_2010_county.csv")
names(d2012)
d2012$YEAR <- 2012
d2012 <- d2012[,c("STATEA","COUNTYA","YEAR","JS5E001")]
colnames(d2012)[colnames(d2012)=="JS5E001"] <- "median_gross_rent"
head(d2012)
## 2011-2015
dvar <- "OBME001"
d2013 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds188_20113_2011_county.csv")
names(d2013)
d2013$YEAR <- 2013
d2013 <- d2013[,c("STATEA","COUNTYA","YEAR",dvar)]
colnames(d2013)[colnames(d2013)==dvar] <- "median_gross_rent"
head(d2013)
## 2012-2016
dvar <- "QZTE001"
d2014 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds191_20125_2012_county.csv")
names(d2014)
d2014$YEAR <- 2014
d2014 <- d2014[,c("STATEA","COUNTYA","YEAR",dvar)]
colnames(d2014)[colnames(d2014)==dvar] <- "median_gross_rent"
head(d2014)
## 2013-2017
dvar <- "UL9E001"
d2015 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0023_csv/nhgis0023_ds201_20135_2013_county.csv")
names(d2015)
d2015$YEAR <- 2015
d2015 <- d2015[,c("STATEA","COUNTYA","YEAR",dvar)]
colnames(d2015)[colnames(d2015)==dvar] <- "median_gross_rent"
head(d2015)

### stack them all ###
dall <- rbind(d1980,d1990,d2000,d2011,d2012,d2013,d2014,d2015)

## look at dc
dconly <- dall[which(dall$STATEA == 11),]
dconly


##### C. just keep dc-area counties ##########################################

dim(dall)
dall <- dall[which((dall$STATEA == 11 & dall$COUNTYA == 1) |
     		   (dall$STATEA == 24 & dall$COUNTYA %in% c(9, 17, 21, 31, 33)) |
		   (dall$STATEA == 51 & dall$COUNTYA %in% c(510,13,43,47,59,600,610,061,
                                  	   	  630,107,683,685,153,157,177,179,187))|
		   (dall$STATEA == 54 & dall$COUNTYA == 37)),]
dim(dall)

table(dall$STATEA)

##### D. inflation adjust #######################################################

## bring in cpi data to inflation adjust ##

cpis <- read.table("/groups/brooksgrp/consumer_price_index/all_urban_consumers/through_2018/cu.data.1.AllItems.txt",
		   sep = "\t", 
		   header = TRUE)
head(cpis)

# keep only relevant years
cpis <- cpis[which(cpis$year >= 1980 & cpis$year <= 2018),]
dim(cpis)
head(cpis)


# keep only relevant series
cpis <- cpis[which(cpis$series_id == "CUSR0000SA0      "),]
dim(cpis)

# and make annual average
cpis <- group_by(cpis,year)
cpiss <- summarize(.data = cpis, index = mean(value, na.rm = TRUE))

# merge inflation info into overall (note we have 2016,7,8 w/o rent data)
dim(dall)
dim(cpiss)
dall <- merge(x = dall,
     	      y = cpiss,
	      by.x = "YEAR",
	      by.y = "year",
	      all = TRUE)
dim(dall)

# pull value of 2018 into all rows
dall$denom1 <- ifelse(dall$YEAR == 2018,dall$index,0)
dall <- mutate(.data = dall, denom = max(denom1))

# create inflation number
dall$adjuster <- dall$denom / dall$index
dall$real_med_gross_rent <- dall$adjust * dall$median_gross_rent
head(dall)


##### E. by jurisdiction dataset and graphs #####################################

### E.1. save the datast

outer <- dall[,c("YEAR","COUNTYA","STATEA","median_gross_rent","real_med_gross_rent")]
out_dir <- "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/mwcog_special_request/"
fn <- paste0(out_dir,
             dateo,
	     "_dcarea_gross_rent.csv")
write.csv(outer,file = fn)
	  
### E.2. make some graphs

grapho <- function(statec,countyc,namer){

  # make a county subset 
  rs <- dall[which(dall$STATEA == statec & dall$COUNTYA == countyc),]

  axis_text_size <- 15

  med.rent <- 
    ggplot(data = rs) +
      geom_line(mapping = aes(x = YEAR, y = median_gross_rent)) +
      theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color="gray"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = axis_text_size),
        axis.title = element_text(size = axis_text_size),
        plot.title = element_text(size=25),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification = c(1,1),
        legend.text = element_text(size=25),
        legend.key.size = unit(0.8,"line"),
        legend.key = element_rect(fill = "white"),
        legend.spacing = unit(0.45,"cm"))+
      labs(title = paste0("Real Median Gross Rent, ",namer),
           y = "median gross rent, 2018 dollars",
	   x = "")

  fn <- paste0(out_dir,dateo,"_s",statec,"_c",countyc,".jpg")
  ggsave(filename = fn,
         plot = med.rent,
	 device = "jpeg",
	 height = 8.5,
	 width = 11,
	 units = c("in"),
	 dpi = 300)

} # end of graph function

# DC
junk <- grapho(11,1,"District of Columbia")

# MD
junk <- grapho(24,17,"Charles County, MD")
junk <- grapho(24,21,"Fredrick County, MD")
junk <- grapho(24,31,"Montgomery County, MD")
junk <- grapho(24,33,"Prince George's County, MD")

# VA
junk <- grapho(51,510,"City of Alexandria, VA")
junk <- grapho(51,13,"Arlington County, VA")
junk <- grapho(51,600,"City of Fairfax, VA")
junk <- grapho(51,59,"Fairfax County, VA")
junk <- grapho(51,610,"City of Falls Church, VA")
junk <- grapho(51,107,"Loudoun County, VA")
junk <- grapho(51,683,"City of Manassas, VA")
junk <- grapho(51,685,"City of Manassas Park, VA")
junk <- grapho(51,153,"Prince William County, VA")


