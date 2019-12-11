library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(sf)
library(stringr)

reformat_age_subset <- function(df, type="DMV") {

  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))

  # remove unnecessary columns
  data <- data[-c(1,2),]

  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index

  # print the df to check if the format is appropriate
  print(head(data))

  # rename the columns to avoid column duplicates error
  #colnames(data) <- make.unique(names(data))

  #names of columns in data frame
  cols <- colnames(data)

  # character variables
  cols.char <- c("FILEID","FILETYPE","STATE","GEO_NAME","GEO_ID")

  #numeric variables
  cols.num <- cols[!cols %in% cols.char]



  # write a function to convert the required columns to numeric
  make_num <- function(x)
  {
    return(as.numeric(as.character(x)))
  }

  # make all the required columns numeric
  data[cols.num] <- lapply(data[cols.num],make_num)

  # print the dataframe to check the data types
  print(str(data))

  # create column state country code
  data["state_county_code"] <- rownames(data)

  # split the column GEO NAME to extract county and state name
  data <- as.data.frame(cSplit(data,c("GEO_NAME"),',',drop = F))

  # split the column country code to get state and county codes
  data <- as.data.frame(cSplit(data,c("state_county_code"),'_',drop = F,type.convert = FALSE))

  # rename the splitted columns
  names(data)[names(data)=="GEO_NAME_1"] <- "county_name"
  names(data)[names(data)=="GEO_NAME_2"] <- "state_name"
  names(data)[names(data)=="state_county_code_1"] <- "state_code"
  names(data)[names(data)=="state_county_code_2"] <- "county_code"

  data$FILETYPE <- as.character(data$FILETYPE)

  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))

  data <- data %>% mutate('less_than_18'=
                     select(.,matches('under_5_years|5_to_9_years|10_to_14_years|15_to_17_years$')) %>% apply(1, sum, na.rm=TRUE))


  data <- data %>% mutate('18_to_29'=
      select(.,matches('18_and_19_years|20_years|21_years|22_to_24_years|25_to_29_years$')) %>% apply(1, sum, na.rm=TRUE))


  data <- data %>% mutate('30_to_44'=
      select(.,matches('30_to_34_years|35_to_39_years|40_to_44_years$')) %>% apply(1, sum, na.rm=TRUE))

  data <- data %>% mutate('45_to_59'=
                   select(.,matches('45_to_49_years|50_to_54_years|55_to_59_years$')) %>% apply(1, sum, na.rm=TRUE))


  data <- data %>% mutate('above_59'=
                select(.,matches('60_and_61_years|62_to_64_years|65_and_66_years|67_to_69_years|70_to_74_years|75_to_79_years|80_to_84_years|85_years_and_over$')) %>% apply(1, sum, na.rm=TRUE))

  data$type = type
  colnames(data)[which(colnames(data) == "B01001_1_total")] <- 'total_population'

  # subset for all the relevant columns
  data_subset <- data[c("STATE_FIPS","county_code","county_name","state_name","year","total_population","18_to_29","30_to_44","45_to_59","above_59")]

  return (data_subset)

}


reformat_race_subset <- function(df, type="DMV") {
  # transpose the data to show attributes as per county level
  data <- as.data.frame(t(df))

  # remove unnecessary columns
  data <- data[-c(1,2),]

  # reassign the column names in transposed df with the indexes
  colnames(data) <- df$index

  # print the df to check if the format is appropriate
  print(head(data))

  # rename the columns to avoid column duplicates error
  #colnames(data) <- make.unique(names(data))

  #names of columns in data frame
  cols <- colnames(data)

  # character variables
  cols.char <- c("FILEID","FILETYPE","STATE","GEO_NAME","GEO_ID")

  #numeric variables
  cols.num <- cols[!cols %in% cols.char]

  # write a function to convert the required columns to numeric
  make_num <- function(x)
  {
    return(as.numeric(as.character(x)))
  }

  # make all the required columns numeric
  data[cols.num] <- lapply(data[cols.num],make_num)

  # print the dataframe to check the data types
  print(str(data))

  # create column state country code
  data["state_county_code"] <- rownames(data)

  # split the column GEO NAME to extract county and state name
  data <- as.data.frame(cSplit(data,c("GEO_NAME"),',',drop = F))

  # split the column country code to get state and county codes
  data <- as.data.frame(cSplit(data,c("state_county_code"),'_',drop = F,type.convert = FALSE))

  # rename the splitted columns
  names(data)[names(data)=="GEO_NAME_1"] <- "county_name"
  names(data)[names(data)=="GEO_NAME_2"] <- "state_name"
  names(data)[names(data)=="state_county_code_1"] <- "state_code"
  names(data)[names(data)=="state_county_code_2"] <- "county_code"

  data$FILETYPE <- as.character(data$FILETYPE)

  # get the year column
  data <- data %>% mutate(year=substr(FILETYPE,1,4))

  data <- data %>% mutate('white_alone'=
                     select(.,matches('B03002_3_white_alone|B03002_13_white_alone$')) %>% apply(1, sum, na.rm=TRUE))


  data <- data %>% mutate('AA_alone'=
      select(.,matches('B03002_4_black_or_african_american_alone|B03002_14_black_or_african_american_alone$')) %>% apply(1, sum, na.rm=TRUE))


  # data <- data %>% mutate('hispanic_or_latino'=
  #     select(.,matches('B03002_12_hispanic_or_latino$')) %>% apply(1, sum, na.rm=TRUE))

  colnames(data)[which(colnames(data) == "B03002_12_hispanic_or_latino")] <- 'hispanic_or_latino'

  data$type = type
  colnames(data)[which(colnames(data) == "B03002_1_total")] <- 'total_population'

  # subset for all the relevant columns
  data_subset <- data[c("STATE_FIPS","county_code","county_name","state_name","year","total_population","white_alone","AA_alone","hispanic_or_latino")]

  return (data_subset)
}
