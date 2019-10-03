
import csv, json, re, numpy as np, sys, os
# %%%%%%%%%%%%% Cenus Data Import and Table Generation %%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%% Authors  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prof Leah Brooks------>Email: lfbrooks@gwu.edu
# Deepak Agarwal------>Email:deepakagarwal@email.gwu.edu
# %%%%%%%%%%%%% Date:
# V2 November - 10 - 2018
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%-----------------------------------------------------------------------
# Importing the required packages
#import urllib, os, xlrd
#import numpy as np
#import csv
import pandas as pd
import os
#%%-----------------------------------------------------------------------
# Specifying the path for documentation file

docFilePath = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/Sequence_Number_and_Table_Number_Lookup.xls'

# Import the documentation file as dataframe
documentation_file = pd.read_excel(docFilePath,header=0,dtype=object,na_filter=False)

# Printing the dataset observations
print(documentation_file.head())

# Printing the dataset columns
print(documentation_file.columns)

print(documentation_file.info())
print(documentation_file.isnull().sum(axis=0))


# Specifying the path for census data files
dataDir = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/VA'
print(dataDir)


# specify function to collect the data for required table and save it as a csv



def get_data(table_title,table_id):

    '''
    This function takes the table name as input and iterates over
    documentation file to get the respective columns, sequence file
    and starting position to read the Census data
    '''

    # specify a empty list to hold column names of the table
    col_names = []

    # specify variables to hold sequence file number and starting position
    seq_number, start_pos = None, None

    # loop through the documentation file for each row
    for index,row in documentation_file.iterrows():

        # get row table title if specified columns are not null
        if row['Table Title'] and row['Total Cells in Table']:
            current_table_title = row['Table Title']

        # get sequence number and start position
        # if table title matches with what we have given and start position is not null
        if current_table_title==table_title and row['Start Position'] and row['Table ID']==table_id:
            print(current_table_title, row['Table ID'])
            seq_number = int(row['Sequence Number'])
            start_pos = int(row['Start Position'])
        #if current_table_title==table_title and row['Total Cells in Sequence']:
         #   max_cols = row['Total Cells in Sequence']

        # get all column names for line numbers (only integers)
        # for given table name
        if current_table_title==table_title and row['Line Number'] and row['Table ID']==table_id:
            try:
                int(row['Line Number'])
                if float(row['Line Number']).is_integer():
                    #print(row['Line Number'])
                    col_names.append(row['Table Title'])
            except:
                pass

    # print sequence number, starting position
    print(seq_number)
    print(start_pos)

    # print length of column list and column names
    print(len(col_names))
    print(col_names)

    col_names_updated = ['FILEID', 'FILETYPE', 'STATE', 'CHARITER', 'SEQUENCE', 'LOGRECNO']+col_names
    print("updated col names",col_names_updated)

    #total_cols =start_pos+len(col_names)

    #if total_cols>max_cols:
     #   print("In here or not")
      #  cols_diff = total_cols-max_cols
       # col_names = col_names[:-cols_diff]

    # provide the path of the file in the data directory using sequence number
    path = dataDir+'/e20095va0'+str(seq_number).zfill(3)+'000.txt'
    print(path)

    # find the ending position of the column list
    end_pos = len(col_names) + start_pos - 1
    print(end_pos)

    #if end_pos>max_cols:
     #   end_pos=max_cols+1

    #print(list(range(start_pos - 1, end_pos+1)))

    # specify the column numbers as list to be taken from the sequence file
    col_list = list(range(start_pos - 1, end_pos))
    print(col_list)

    col_list_updated = list(range(0,6))+col_list
    print("updated col list",col_list_updated)

    # read the data from the give sequence file for specified column list
    data = pd.read_csv(path,header=None,usecols=col_list_updated,dtype=str)

    # provide the column names to the dataframe
    data.columns = col_names_updated

    # print the dimension of the dataset
    print(data.shape)

    # print the column names of the dataset
    print(data.columns)

    # print first few rows of the dataset
    print(data.head())


    # read the geograpy file
    path = dataDir + '/g20095va.txt'

    # create empty dictionaries for saving geoid and geoname for each logical record no.
    geo_geoid = {}
    geo_geoname = {}
    geo_sumlevel = {}
    geo_state = {}
    geo_county = {}

    # open the geography file and extract requried values
    with open(path, 'rb') as f:
        for row in f:
            sumlevel = row[8:11].decode('utf-8','ignore')
            state = row[25:27].decode('utf-8','ignore')
            county = row[27:30].decode('utf-8','ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            geoid = row[178:218].strip().decode('utf-8', 'ignore')
            gname = row[218:].strip().decode('utf-8', 'ignore')
            geo_geoid[logrecno] = geoid
            geo_geoname[logrecno] = gname
            geo_sumlevel[logrecno] = sumlevel
            geo_state[logrecno] = state
            geo_county[logrecno] = county

    print(geo_state)

    #print(list(geog.values())[:][0])

    # create GEO_ID and GEO_NAME columns for matching logical record number in estimate file and geography file
    data["SUMMARY_LEVEL"] = data["LOGRECNO"].map(geo_sumlevel)
    data["STATE_FIPS"] = data["LOGRECNO"].map(geo_state)
    data["COUNTY"] = data["LOGRECNO"].map(geo_county)
    data["GEO_ID"] = data["LOGRECNO"].map(geo_geoid)
    data["GEO_NAME"] = data["LOGRECNO"].map(geo_geoname)

    # for index, row in data.iterrows():
    #     row.extend(geog[row[5].encode('utf-8').strip()])

    # print the dimension of the dataset
    print(data.shape)

    # print the column names of the dataset
    print(data.columns)

    # print first few rows of the dataset
    print(data.head())

    # use the table title name to save the table data
    table_name = table_title + str(".csv")
    print(table_name)

    save_file = os.path.join(dataDir,table_name)

    df_cols_list = data.columns.tolist()

    col_list_ordered = df_cols_list[:6]+df_cols_list[-5:]+df_cols_list[6:-5]

    data = data[col_list_ordered]
    # save the data
    data.to_csv(save_file,sep=',',index=False)

    return data

# # specify the table name to save as csv

#table_title = ['TOTAL POPULATION','SEX BY AGE','RACE',	'HOUSEHOLD TYPE (INCLUDING LIVING ALONE)', 'HOUSEHOLD TYPE BY UNITS IN STRUCTURE',
#'SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER', 'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2009 INFLATION-ADJUSTED DOLLARS)',
#'HOUSING UNITS', 'TENURE', 'UNITS IN STRUCTURE', 'YEAR STRUCTURE BUILT', 'VALUE', 'PRICE ASKED']


Population_df = get_data('TOTAL POPULATION','B01003')
Age_df = get_data('SEX BY AGE','B01001')
# Race_df = get_data('RACE','B02001')
# Household_df = get_data('HOUSEHOLD TYPE (INCLUDING LIVING ALONE)','B11001')
# HouseholdComp_df = get_data('HOUSEHOLD TYPE BY UNITS IN STRUCTURE','B11011')
# Education_df = get_data('SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER','B15002')
# Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2009 INFLATION-ADJUSTED DOLLARS)','B19001')
# NoOfHousing_df = get_data('HOUSING UNITS','B25001')
# Tenure_df = get_data('TENURE','B25003')
# TypeOfHousing_df = get_data('UNITS IN STRUCTURE','B25024')
# AgeOfHousing_df = get_data('YEAR STRUCTURE BUILT','B25034')
# Value_df = get_data('VALUE','B25075')
# Price_df = get_data('PRICE ASKED','B25085')

newDF = pd.concat([Population_df, Age_df.iloc[:, 11:]], axis=1)

print(len(Population_df.columns))
print(len(Age_df.columns))
print(len(newDF.columns))

print(newDF.columns.tolist()[0:20])

# df = []
#
# for title in df_title:
#         #df_name = name+'_df'
#         newDF = get_data(title)
#         df.append(newDF)

#appended_data = pd.concat(df,axis=1)



#
# # call the function to get data as csv for the given table title
#get_data(table_title)