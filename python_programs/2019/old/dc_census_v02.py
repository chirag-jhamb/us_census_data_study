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

#docFilePath = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/Sequence_Number_and_Table_Number_Lookup.xls'

# Import the documentation file as dataframe
#documentation_file = pd.read_excel(docFilePath,header=0,dtype=object,na_filter=False)

# Printing the dataset observations
#print(documentation_file.head())

# Printing the dataset columns
#print(documentation_file.columns)

#print(documentation_file.info())
#print(documentation_file.isnull().sum(axis=0))


# Specifying the path for census data files
dataDir = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/DC'
print(dataDir)


# specify function to collect the data for required table and save it as a csv



# def get_data(table_title):
#
#     '''
#     This function takes the table name as input and iterates over
#     documentation file to get the respective columns, sequence file
#     and starting position to read the Census data
#     '''
#
#     # specify a empty list to hold column names of the table
#     col_names = []
#
#     # specify variables to hold sequence file number and starting position
#     seq_number, start_pos = None, None
#
#     # loop through the documentation file for each row
#     for index,row in documentation_file.iterrows():
#
#         # get row table title if specified columns are not null
#         if row['Table Title'] and row['Total Cells in Table']:
#             current_table_title=row['Table Title']
#
#         # get sequence number and start position
#         # if table title matches with what we have given and start position is not null
#         if current_table_title==table_title and row['Start Position']:
#             seq_number = int(row['Sequence Number'])
#             start_pos = int(row['Start Position'])
#         #if current_table_title==table_title and row['Total Cells in Sequence']:
#          #   max_cols = row['Total Cells in Sequence']
#
#         # get all column names for line numbers (only integers)
#         # for given table name
#         if current_table_title==table_title and row['Line Number']:
#             try:
#                 int(row['Line Number'])
#                 if float(row['Line Number']).is_integer():
#                     #print(row['Line Number'])
#                     col_names.append(row['Table Title'])
#             except:
#                 pass
#
#     # print sequence number, starting position
#     print(seq_number)
#     print(start_pos)
#
#     # print length of column list and column names
#     print(len(col_names))
#     print(col_names)
#
#     #total_cols =start_pos+len(col_names)
#
#     #if total_cols>max_cols:
#      #   print("In here or not")
#       #  cols_diff = total_cols-max_cols
#        # col_names = col_names[:-cols_diff]
#
#     # provide the path of the file in the data directory using sequence number
#     path = dataDir+'/e20095dc0'+str(seq_number).zfill(3)+'000.txt'
#     print(path)
#
#     # find the ending position of the column list
#     end_pos = len(col_names) + start_pos - 1
#     print(end_pos)
#
#     #if end_pos>max_cols:
#      #   end_pos=max_cols+1
#
#     #print(list(range(start_pos - 1, end_pos+1)))
#
#     # specify the column numbers as list to be taken from the sequence file
#     col_list = list(range(start_pos - 1, end_pos))
#     print(col_list)
#
#     # read the data from the give sequence file for specified column list
#     data = pd.read_csv(path,header=None,usecols=col_list)
#
#     # provide the column names to the dataframe
#     data.columns = col_names
#
#     # print the dimension of the dataset
#     print(data.shape)
#
#     # print the column names of the dataset
#     print(data.columns)
#
#     # print first few rows of the dataset
#     print(data.head())
#
#     # use the table title name to save the table data
#     table_name = table_title + str(".csv")
#     print(table_name)
#
#     save_file = os.path.join(dataDir,table_name)
#
#     # save the data
#
#     data.to_csv(save_file,sep=',',index=False)
#
# # specify the table name to save as csv
# table_title='HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2009 INFLATION-ADJUSTED DOLLARS) BY VALUE'
# #table_title = 'GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES'
# #table_title = 'MEAN HOUSEHOLD INCOME OF QUINTILES'
#
# # call the function to get data as csv for the given table title
# get_data(table_title)

import csv
import io

path = dataDir+'/g20095dc.txt'
print(path)
# geodata = pd.read_table(path, header=None)
#
# print(type(geodata))
# print(geodata.columns)
#
# print(geodata.head())

#geos = {}

geog = {}

with open(path, 'rb') as f:
    nottract = []
    for row in f:
        logrecno = row[13:20]
        geoid = row[178:218].strip()
        gname = row[218:].strip().decode('utf-8', 'ignore')
        geog[logrecno] = [geoid, gname]

        #geos[(row[1], row[4])] = row[-4]

print(geog)
