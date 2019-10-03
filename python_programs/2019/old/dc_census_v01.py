import csv, json, re, numpy as np, sys, os
# %%%%%%%%%%%%% Cenus Data Import and Table Generation %%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%% Authors  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prof Leah Brooks------>Email: lfbrooks@gwu.edu
# Deepak Agarwal------>Email:deepakagarwal@email.gwu.edu
# %%%%%%%%%%%%% Date:
# V2 November - 10 - 2018
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%-----------------------------------------------------------------------
# Importing the required packages
# import urllib, os, xlrd
# import numpy as np
# import csv
import pandas as pd
import os

# %%-----------------------------------------------------------------------
# Specifying the path for documentation file

docFilePath = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/Sequence_Number_and_Table_Number_Lookup.xls'

# Import the documentation file as dataframe
documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)

# Printing the dataset observations
print(documentation_file.head())

# Printing the dataset columns
print(documentation_file.columns)

print(documentation_file.info())
print(documentation_file.isnull().sum(axis=0))


# specify function to collect the data for required table and save it as a csv


def get_data(table_title, table_id, state):
    '''
    This function takes the table name as input and iterates over
    documentation file to get the respective columns, sequence file
    and starting position to read the Census data
    '''
    print(state,'state')

    # Specifying the path for census data files
    data_path = r'/groups/brooksgrp/census/american_community_survey/2005_2009_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/'
    dataDir = data_path + state
    print(dataDir)

    # specify a empty list to hold column names of the table
    col_names = []

    # specify variables to hold sequence file number and starting position
    seq_number, start_pos = None, None

    # loop through the documentation file for each row
    for index, row in documentation_file.iterrows():

        # get row table title if specified columns are not null
        if row['Table Title'] and row['Total Cells in Table']:
            current_table_title = row['Table Title']

        # get sequence number and start position
        # if table title matches with what we have given and start position is not null
        if current_table_title == table_title and row['Start Position'] and row['Table ID'] == table_id:
            print(current_table_title, row['Table ID'])
            seq_number = int(row['Sequence Number'])
            start_pos = int(row['Start Position'])
        # if current_table_title==table_title and row['Total Cells in Sequence']:
        #   max_cols = row['Total Cells in Sequence']

        # get all column names for line numbers (only integers)
        # for given table name
        if current_table_title == table_title and row['Line Number'] and row['Table ID'] == table_id:
            try:
                int(row['Line Number'])
                if float(row['Line Number']).is_integer():
                    # print(row['Line Number'])
                    col_names.append(row['Table Title'])
            except:
                pass

    # print sequence number, starting position
    print(seq_number)
    print(start_pos)

    # print length of column list and column names
    print(len(col_names))
    print(col_names)

    col_names_updated = ['FILEID', 'FILETYPE', 'STATE', 'CHARITER', 'SEQUENCE', 'LOGRECNO'] + col_names
    print("updated col names", col_names_updated)

    # total_cols =start_pos+len(col_names)

    # if total_cols>max_cols:
    #   print("In here or not")
    #  cols_diff = total_cols-max_cols
    # col_names = col_names[:-cols_diff]

    # provide the path of the file in the data directory using sequence number
    path = dataDir + '/e20095' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'
    print(path)

    # find the ending position of the column list
    end_pos = len(col_names) + start_pos - 1
    print(end_pos)

    # if end_pos>max_cols:
    #   end_pos=max_cols+1

    # print(list(range(start_pos - 1, end_pos+1)))

    # specify the column numbers as list to be taken from the sequence file
    col_list = list(range(start_pos - 1, end_pos))
    print(col_list)

    col_list_updated = list(range(0, 6)) + col_list
    print("updated col list", col_list_updated)

    # read the data from the give sequence file for specified column list
    data = pd.read_csv(path, header=None, usecols=col_list_updated, dtype=str)

    # provide the column names to the dataframe
    data.columns = col_names_updated

    # print the dimension of the dataset
    print(data.shape)

    # print the column names of the dataset
    print(data.columns)

    # print first few rows of the dataset
    print(data.head())

    # read the geograpy file
    path = dataDir + '/g20095' + state.lower() + '.txt'

    # create empty dictionaries for saving geoid and geoname for each logical record no.
    geo_geoid = {}
    geo_geoname = {}
    geo_sumlevel = {}
    geo_state = {}
    geo_county = {}

    # open the geography file and extract requried values
    with open(path, 'rb') as f:
        for row in f:
            sumlevel = row[8:11].decode('utf-8', 'ignore')
            state_fips = row[25:27].decode('utf-8', 'ignore')
            county = row[27:30].decode('utf-8', 'ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            geoid = row[178:218].strip().decode('utf-8', 'ignore')
            gname = row[218:].strip().decode('utf-8', 'ignore')
            geo_geoid[logrecno] = geoid
            geo_geoname[logrecno] = gname
            geo_sumlevel[logrecno] = sumlevel
            geo_state[logrecno] = state_fips
            geo_county[logrecno] = county

    #print(geo_state)

    # print(list(geog.values())[:][0])

    # create GEO_ID and GEO_NAME columns for matching logical record number in estimate file and geography file
    #data["SUMMARY_LEVEL"] = data["LOGRECNO"].map(geo_sumlevel)
    # data["STATE_FIPS"] = data["LOGRECNO"].map(geo_state)
    # data["COUNTY"] = data["LOGRECNO"].map(geo_county)
    # data["GEO_ID"] = data["LOGRECNO"].map(geo_geoid)
    # data["GEO_NAME"] = data["LOGRECNO"].map(geo_geoname)
    data.insert(loc=6, column="SUMMARY_LEVEL",value=data["LOGRECNO"].map(geo_sumlevel))
    data.insert(loc=7, column="STATE_FIPS", value=data["LOGRECNO"].map(geo_state))
    data.insert(loc=8, column="COUNTY", value=data["LOGRECNO"].map(geo_county))
    data.insert(loc=9, column="GEO_ID", value=data["LOGRECNO"].map(geo_geoid))
    data.insert(loc=10, column="GEO_NAME", value=data["LOGRECNO"].map(geo_geoname))


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

    save_file = os.path.join(dataDir, table_name)

    #df_cols_list = data.columns.tolist()

    #col_list_ordered = df_cols_list[:6] + df_cols_list[-5:] + df_cols_list[6:-5]

    #print(df_cols_list[:6])

    #print(df_cols_list[-5:])

    #print(df_cols_list[6:-5])

    #print("col list",col_list_ordered)

    #data1 = data[col_list_ordered]
    #print("colu",len(data1.columns.tolist()))

    # save the data
    #data.to_csv(save_file, sep=',', index=False)

    #print(df_cols_list)
    #print(col_list_ordered)
    #print(data.columns.tolist(),"columns")

    data = data.query('SUMMARY_LEVEL=="050"')

    try:
        if state == 'DC':
            data = data.query('COUNTY in ["001"]')
        elif state == 'MD':
            data = data.query('COUNTY in ["009","017","021","031","033"]')
        elif state == 'VA':
            data = data.query('COUNTY in ["510","013","043","047","059","600","610","061",'
                              '"630", "107","683","685","153","157","177","179","187"]')
        elif state == 'WV':
            data = data.query('COUNTY in ["037"]')
    except:
        print(state + ' is not in the list')

    print("Number of counties",data.COUNTY.value_counts())

    return data


# # specify the table name to save as csv

# table_title = ['TOTAL POPULATION','SEX BY AGE','RACE',	'HOUSEHOLD TYPE (INCLUDING LIVING ALONE)', 'HOUSEHOLD TYPE BY UNITS IN STRUCTURE',
# 'SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER', 'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2009 INFLATION-ADJUSTED DOLLARS)',
# 'HOUSING UNITS', 'TENURE', 'UNITS IN STRUCTURE', 'YEAR STRUCTURE BUILT', 'VALUE', 'PRICE ASKED']

def get_summary_data(state):

    Population_df = get_data('TOTAL POPULATION', 'B01003', state)
    Age_df = get_data('SEX BY AGE', 'B01001', state)
    Race_df = get_data('RACE', 'B02001', state)
    Household_df = get_data('HOUSEHOLD TYPE (INCLUDING LIVING ALONE)', 'B11001', state)
    Education_df = get_data('SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER', 'B15002', state)
    Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2009 INFLATION-ADJUSTED DOLLARS)', 'B19001', state)
    NoOfHousing_df = get_data('HOUSING UNITS', 'B25001', state)
    Tenure_df = get_data('TENURE', 'B25003', state)
    TypeOfHousing_df = get_data('UNITS IN STRUCTURE', 'B25024', state)
    MedianValue_df = get_data('MEDIAN VALUE (DOLLARS)', 'B25077', state)
    AgeOfHousing_df = get_data('YEAR STRUCTURE BUILT', 'B25034', state)



    df= Population_df

    df = df.rename(columns={'Total':'Total_population'})

    df['percent_below_18'] = Age_df.iloc[:, np.r_[13:17,37:41]].apply(pd.to_numeric).sum(axis=1)/Age_df['Total:'].apply(pd.to_numeric)*100

    df['percent_above_65'] = Age_df.iloc[:, np.r_[30:36,54:60]].apply(pd.to_numeric).sum(axis=1)/Age_df['Total:'].apply(pd.to_numeric)*100

    df['percent_AA'] = Race_df['Black or African American alone'].apply(pd.to_numeric)/Race_df['Total:'].apply(pd.to_numeric)*100

    df['percent_NonWhite'] = (Race_df['Total:'].apply(pd.to_numeric)-Race_df['White alone'].apply(pd.to_numeric))/Race_df['Total:'].apply(pd.to_numeric)*100

    df['percent_FamilyHouseholds'] = Household_df['Family households:'].apply(pd.to_numeric)/Household_df['Total:'].apply(pd.to_numeric)*100

    df['percent_MarriedCoupleFamily'] = Household_df['Married-couple family'].apply(pd.to_numeric)/Household_df['Total:'].apply(pd.to_numeric)*100

    df['percent_upto_HighSchool'] = Education_df.iloc[:, np.r_[13:22,30:39]].apply(pd.to_numeric).sum(axis=1)/Education_df['Total:'].apply(pd.to_numeric)*100

    df['percent_upto_College'] = Education_df.iloc[:, np.r_[22:24,39:41]].apply(pd.to_numeric).sum(axis=1)/Education_df['Total:'].apply(pd.to_numeric)*100

    df['percent_above_College'] = Education_df.iloc[:, np.r_[24:29,41:46]].apply(pd.to_numeric).sum(axis=1)/Education_df['Total:'].apply(pd.to_numeric)*100


    #print(Education_df.iloc[:, np.r_[13:22,30:39]].columns.tolist())
    #print(Education_df.iloc[:, np.r_[22:24,39:41]].columns.tolist())
    #print(Education_df.iloc[:, np.r_[24:29,41:46]].columns.tolist())

    df['percent_income_below_50k'] = Income_df.iloc[:, 12:21].apply(pd.to_numeric).sum(axis=1)/Income_df['Total:'].apply(pd.to_numeric)*100

    df['percent_income_above_200k'] = Income_df['$200,000 or more'].apply(pd.to_numeric)/Income_df['Total:'].apply(pd.to_numeric)*100

    #print(Income_df.iloc[:, 12:21].columns.tolist())


    df['housing_units_per_person'] = NoOfHousing_df['Total'].apply(pd.to_numeric)/df['Total_population'].apply(pd.to_numeric)

    df['percent_OwnerOccupied'] = Tenure_df['Owner occupied'].apply(pd.to_numeric)/Tenure_df['Total:'].apply(pd.to_numeric)

    df['percent_1_Units'] = TypeOfHousing_df.loc[:,['1, detached','1, attached']].apply(pd.to_numeric).sum(axis=1)/TypeOfHousing_df['Total:'].apply(pd.to_numeric)
    #
    #AgeOfHousing_df_subset = AgeOfHousing_df.iloc[:,11:]

    df['Median_Value'] = MedianValue_df['Median value (dollars)']


    AgeOfHousing_df.iloc[:,12:] = AgeOfHousing_df.iloc[:,12:].apply(pd.to_numeric).div(AgeOfHousing_df["Total:"].apply(pd.to_numeric),axis=0)

    AgeOfHousing_df_subset = AgeOfHousing_df.iloc[:,np.r_[5,12:21]]
    #print(AgeOfHousing_df.iloc[:, np.r_[5,12:21]].columns.tolist())

    mergedDF = pd.merge(df,AgeOfHousing_df_subset,on="LOGRECNO")

    print(mergedDF.columns.tolist())

    #print(mergedDf.head())

    return mergedDF



mergedDF_DC = get_summary_data('DC')

print(mergedDF_DC.head())


mergedDF_MD = get_summary_data('MD')

print(mergedDF_MD.head())






#print(df.iloc[:, np.r_[30:36, 54:60]].sum(axis=1)/df['Total:']*100)




#Population_df = Population_df.rename(columns={'Total':'Total_population'})
#print(Age_df.loc[:, ["COUNTY", "Total:"]])

#print(Age_df)
#print(Age_df.columns)

#print(Age_df.iloc[:,12:16])

#print(Age_df.columns)

# newDF = pd.concat([Population_df, Age_df.iloc[:, 11:], Race_df.iloc[:, 11:], Household_df.iloc[:, 11:], Education_df.iloc[:, 11:],
#                    Income_df.iloc[:, 11:], NoOfHousing_df.iloc[:, 11:], Tenure_df.iloc[:, 11:],TypeOfHousing_df.iloc[:, 11:],
#                    AgeOfHousing_df.iloc[:, 11:],Value_df.iloc[:, 11:]],axis=1)
#
# print(len(Population_df.columns))
# print(len(Age_df.columns))
# print(len(newDF.columns))
#
# print(newDF.columns.tolist()[0:20])
#
# print(len(newDF.columns))
# df = []
#
# for title in df_title:
#         #df_name = name+'_df'
#         newDF = get_data(title)
#         df.append(newDF)

# appended_data = pd.concat(df,axis=1)


#
# # call the function to get data as csv for the given table title
# get_data(table_title)










