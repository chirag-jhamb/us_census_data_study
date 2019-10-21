# %%%%%%%%%%%%% Cenus Data Import and Table Generation %%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%% Authors  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prof Leah Brooks------>Email: lfbrooks@gwu.edu
# Deepak Agarwal------>Email:deepakagarwal@email.gwu.edu
# %%%%%%%%%%%%% Date:
# V1 February - 26 - 2018
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%-----------------------------------------------------------------------

# Importing the required packages
import numpy as np
import pandas as pd
import os, datetime
from functools import reduce
import warnings

warnings.filterwarnings("ignore")
# %%-----------------------------------------------------------------------
######################## A. Read Documentation File ############################

# %%-----------------------------------------------------------------------
# Specifying the path for documentation file

# docFilePath = r'/groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/' \
#               r'ACS_5yr_Seq_Table_Number_Lookup.xls'
#
# # Import the documentation file as dataframe
# documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)
#
# # Printing the dataset observations
# print(documentation_file.head())
#
# # Printing the dataset columns
# print(documentation_file.columns)
#
# print(documentation_file.info())
# print(documentation_file.isnull().sum(axis=0))


# %%-----------------------------------------------------------------------
######################## B. Get Table Data ############################
def get_summary_year_data(start_year, end_year):

    start_year = str(start_year)
    end_year = str(end_year)

    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    outDir = r"/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2019/summary_files_data"



# %%-----------------------------------------------------------------------
# specify function to collect the data for required table and save it as a csv

    def get_data(table_title, table_id):
        '''
        This function takes the table name, table id and state as input and iterates over
        documentation file to get the table name, table id and respective columns,
        and gets the data from sequence file and geography file

        :param table_title: string - name of the table
        :param table_id: string - table id
        :param state: string - name of the state
        :return: dataframe - table data
        '''

        #print("Hello")
        # print('fetching data for state: ', state)

        # Specifying the path for documentation file
        if int(end_year) < 2013:
            docFilePath = inDir + '/Sequence_Number_and_Table_Number_Lookup.xls'
        else:
            docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.xls'

        # Import the documentation file as dataframe
        documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)

        # Printing the dataset observations
        print(documentation_file.head())

        # Printing the dataset columns
        print(documentation_file.columns)

        print(documentation_file.info())
        print(documentation_file.isnull().sum(axis=0))

        # Specifying the path for census data files
        dataDir = inDir + '/group1'

        # Specifying the path for census data files
        print("data directory is:", dataDir)

        # specify a empty list to hold column names of the table
        col_names = []

        # specify variables to hold sequence file number and starting position
        seq_number, start_pos = None, None

        # loop through the documentation file for each row
        for index, row in documentation_file.iterrows():

            # get row table title if specified columns are not null
            if row['Table Title'] and row['Total Cells in Table']:
                current_table_title = row['Table Title']
                #print(current_table_title)

            # get sequence number and start position
            # if table title matches with what we have given and start position is not null
            # if current_table_title == table_title and row['Start Position'] and row['Table ID'] == table_id:
            #     print(current_table_title, row['Table ID'])
            #     seq_number = int(row['Sequence Number'])
            #     start_pos = int(row['Start Position'])
            # if current_table_title==table_title and row['Total Cells in Sequence']:
            #   max_cols = row['Total Cells in Sequence']

                # get sequence number and start position
                # if table title matches with what we have given and start position is not null
            if current_table_title == table_title and row['Start Position'] and row['Table ID'] == table_id:
                #print(current_table_title, row['Table ID'])
                seq_number = int(str(row['Sequence Number']).strip())
                # start_pos = int(str(row['Start Position']).strip())
                # if row['Start Position'] == ' ':
                #     continue
                # else:
                #     start_pos = int(row['Start Position'])

                try:
                    int(row['Start Position'])
                    if float(row['Start Position']).is_integer():
                        # print(row['Line Number'])
                        start_pos = int(row['Start Position'])
                except:
                    pass

            # get all column names for line numbers (only integers)
            # for given table name
            if current_table_title == table_title and row['Line Number'] and row['Table ID'] == table_id:
                try:
                    int(row['Line Number'])
                    if float(row['Line Number']).is_integer():
                        # print(row['Line Number'])
                        col_names.append(str(row['Table ID']) + '_' + str(row['Line Number']) + '_' + str(
                            row['Table Title']).lower().replace(" ", "_").replace(":", ""))
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
        path = dataDir + '/e'+end_year+'5' + 'us' + '0' + str(seq_number).zfill(3) + '000.txt'
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
        # path = dataDir + '/g20165' + state.lower() + '.txt'
        path = '/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data/geog/g'+str(end_year)+'5us.txt'

        # create empty dictionaries for saving geoid and geoname for each logical record no.
        geo_geoid = {}
        geo_geoname = {}
        geo_sumlevel = {}
        # geo_state = {}
        # geo_county = {}
        geo_zcta = {}

        # open the geography file and extract requried values
        with open(path, 'rb') as f:
            for row in f:
                sumlevel = row[8:11].decode('utf-8', 'ignore')
                logrecno = row[13:20].decode('utf-8', 'ignore')
                # state_fips = row[25:27].decode('utf-8', 'ignore')
                # county = row[27:30].decode('utf-8', 'ignore')
                zcta = row[130:135].decode('utf-8', 'ignore')
                geoid = row[178:218].strip().decode('utf-8', 'ignore')
                gname = row[218:].strip().decode('utf-8', 'ignore')
                geo_geoid[logrecno] = geoid
                geo_geoname[logrecno] = gname
                geo_sumlevel[logrecno] = sumlevel
                # geo_state[logrecno] = state_fips
                # geo_county[logrecno] = county
                geo_zcta[logrecno] = zcta

        # print(geo_state)

        # for matching logical record numbers insert columns from geography file in table data
        data.insert(loc=6, column="SUMMARY_LEVEL", value=data["LOGRECNO"].map(geo_sumlevel))
        # data.insert(loc=7, column="STATE_FIPS", value=data["LOGRECNO"].map(geo_state))
        # data.insert(loc=8, column="COUNTY", value=data["LOGRECNO"].map(geo_county))
        data.insert(loc=7, column="ZCTA", value=data["LOGRECNO"].map(geo_zcta))
        data.insert(loc=8, column="GEO_ID", value=data["LOGRECNO"].map(geo_geoid))
        data.insert(loc=9, column="GEO_NAME", value=data["LOGRECNO"].map(geo_geoname))

        # print the dimension of the dataset
        print(data.shape)

        # print the column names of the dataset
        print(data.columns)

        # print first few rows of the dataset
        print(data.head())

        # use the table title name to save the table data
        table_name = table_title.replace(" ", "_") + str("_zcta_"+end_year+".csv")
        print(table_name)

        # specify the path to save the data
    #    save_file = os.path.join(outDir, table_name)

        # subset the data for summary_level 050 i.e. metro area
        data = data.query('SUMMARY_LEVEL=="860"')

        data.drop(['FILEID', 'STATE', 'CHARITER', 'SEQUENCE', 'SUMMARY_LEVEL', 'GEO_NAME'], axis=1, inplace=True)

        #data.columns = data.columns.str.replace('Total*', 'total_' + table_title.replace(" ", "_").lower())
        #data.columns = data.columns.str.replace('Male*', 'male_' + table_title.replace(" ", "_").lower())
        #data.columns = data.columns.str.replace('Female*', 'female_' + table_title.replace(" ", "_").lower())

        print(data.columns.tolist())

        return data


# %%-----------------------------------------------------------------------
#################### A. get table data #########################

    # call get_data function to get table data for respective tables
    Population_df = get_data('TOTAL POPULATION', 'B01003')
    Age_df = get_data('SEX BY AGE', 'B01001')
    Race_df = get_data('RACE', 'B02001')
    Household_df = get_data('HOUSEHOLD TYPE (INCLUDING LIVING ALONE)', 'B11001')
    Education_df = get_data('SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER', 'B15002')
    #Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)', 'B19001')
    #Median_Income_df = get_data('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS), 'B19013')
    Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19001')
    Median_Income_df = get_data('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19013')
    NoOfHousing_df = get_data('HOUSING UNITS', 'B25001')
    Tenure_df = get_data('TENURE', 'B25003')
    TypeOfHousing_df = get_data('UNITS IN STRUCTURE', 'B25024')
    MedianValue_df = get_data('MEDIAN VALUE (DOLLARS)', 'B25077')
    AgeOfHousing_df = get_data('YEAR STRUCTURE BUILT', 'B25034')
    Tenure_by_year_df = get_data('TENURE BY YEAR STRUCTURE BUILT BY UNITS IN STRUCTURE', 'B25127')
    FamilyType_by_children_df = get_data('FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS', 'B11003')
    Owner_Occupied_Housing_Value_df = get_data('VALUE', 'B25075')
    Race_Hispanic_Latino_df = get_data('HISPANIC OR LATINO ORIGIN BY RACE', 'B03002')
    #Contract_Rent_df = get_data('CONTRACT RENT', 'B25056')
    Gross_Rent_df = get_data('GROSS RENT', 'B25063')


    data_frames = [Population_df, Age_df, Race_df, Household_df, Education_df, Income_df, Median_Income_df, NoOfHousing_df,
                   Tenure_df, MedianValue_df, AgeOfHousing_df, Tenure_by_year_df, TypeOfHousing_df, FamilyType_by_children_df, Owner_Occupied_Housing_Value_df,
                   Race_Hispanic_Latino_df, Gross_Rent_df]

    mergedDF = reduce(lambda left, right: pd.merge(left, right, on=["LOGRECNO", "FILETYPE", "ZCTA", "GEO_ID"], how='outer'),
                      data_frames)

    path_to_zcta_county_mapping = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data"

    state_county_zcta_data = pd.read_csv(os.path.join(path_to_zcta_county_mapping, "20190226_state_county_zcta.csv"),
                                         header=0, dtype=object, na_filter=False)

    mergedDF_zcta = pd.merge(mergedDF, state_county_zcta_data, how='inner', on='ZCTA')

    print(mergedDF_zcta.head())

    save_file = os.path.join(outDir, datetime.datetime.today().strftime('%Y%m%d') + '_acs_zcta_' + start_year + '_' + end_year + '.csv')
    print(save_file)

    # save the final data
    mergedDF_zcta.to_csv(save_file, sep=',', index=False)



#
# #end_year = ['2011']
#
end_year = ['2011', '2012', '2013', '2014', '2015', '2016','2017']
#
#end_year = ['2011', '2012', '2013', '2014']

#end_year = ['2017']

for year in end_year:
    get_summary_year_data(str(int(year) - 4), year)