# %%%%%%%%%%%%% Cenus Data Import and Table Generation %%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%% Authors  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prof Leah Brooks------>Email: lfbrooks@gwu.edu
# Deepak Agarwal------>Email:deepakagarwal@email.gwu.edu
# %%%%%%%%%%%%% Date:
# V2 November - 22 - 2018
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%-----------------------------------------------------------------------

# Importing the required packages
import numpy as np
import pandas as pd
import os, datetime
# %%-----------------------------------------------------------------------
######################## A. Read Documentation File ############################

# %%-----------------------------------------------------------------------
# Specifying the path for documentation file

docFilePath = r'/groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/' \
              r'ACS_5yr_Seq_Table_Number_Lookup.xls'

# Import the documentation file as dataframe
documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)

# Printing the dataset observations
print(documentation_file.head())

# Printing the dataset columns
print(documentation_file.columns)

print(documentation_file.info())
print(documentation_file.isnull().sum(axis=0))
# %%-----------------------------------------------------------------------
######################## B. Get Table Data ############################

inDir = r'/groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/All_Geographies'
outDir = r"/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/summary_files_data"

# %%-----------------------------------------------------------------------
# specify function to collect the data for required table and save it as a csv

def get_data(table_title, table_id, state):
    '''
    This function takes the table name, table id and state as input and iterates over
    documentation file to get the table name, table id and respective columns,
    and gets the data from sequence file and geography file

    :param table_title: string - name of the table
    :param table_id: string - table id
    :param state: string - name of the state
    :return: dataframe - table data
    '''

    print('fetching data for state: ', state)

    # Specifying the path for census data files
    dataDir = inDir +'/data/tab4/sumfile/prod/2012thru2016/group1'

    #dataDir = data_path + state
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
    path = dataDir + '/e20165' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'
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
    path = inDir + '/geo/g20165' + state.lower() + '.txt'

    # create empty dictionaries for saving geoid and geoname for each logical record no.
    geo_geoid = {}
    geo_geoname = {}
    geo_sumlevel = {}
    geo_state = {}
    geo_county = {}
    #geo_zcta = {}

    # open the geography file and extract requried values
    with open(path, 'rb') as f:
        for row in f:
            sumlevel = row[8:11].decode('utf-8', 'ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            state_fips = row[25:27].decode('utf-8', 'ignore')
            county = row[27:30].decode('utf-8', 'ignore')
            #zcta = row[130:135].decode('utf-8', 'ignore')
            geoid = row[178:218].strip().decode('utf-8', 'ignore')
            gname = row[218:].strip().decode('utf-8', 'ignore')
            geo_geoid[logrecno] = geoid
            geo_geoname[logrecno] = gname
            geo_sumlevel[logrecno] = sumlevel
            geo_state[logrecno] = state_fips
            geo_county[logrecno] = county
           # geo_zcta[logrecno] = zcta

    #print(geo_state)

    # for matching logical record numbers insert columns from geography file in table data
    data.insert(loc=6, column="SUMMARY_LEVEL",value=data["LOGRECNO"].map(geo_sumlevel))
    data.insert(loc=7, column="STATE_FIPS", value=data["LOGRECNO"].map(geo_state))
    data.insert(loc=8, column="COUNTY", value=data["LOGRECNO"].map(geo_county))
    #data.insert(loc=9, column="ZCTA", value=data["LOGRECNO"].map(geo_zcta))
    data.insert(loc=9, column="GEO_ID", value=data["LOGRECNO"].map(geo_geoid))
    data.insert(loc=10, column="GEO_NAME", value=data["LOGRECNO"].map(geo_geoname))

    # print the dimension of the dataset
    print(data.shape)

    # print the column names of the dataset
    print(data.columns)

    # print first few rows of the dataset
    print(data.head())

    # use the table title name to save the table data
    table_name = table_title.replace(" ", "_") + str("_state_2016.csv")
    print(table_name)

    # specify the path to save the data
    save_file = os.path.join(outDir, table_name)

    # save the data
    # data.to_csv(save_file, sep=',', index=False)

    # subset the data for summary_level 050 i.e. metro area
    data = data.query('SUMMARY_LEVEL=="050"')

    # subset the data for zcta 860 i.e. metro area
    #data = data.query('ZCTA=="860"')

    # for respective state subset for require counties
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

    #print("Number of counties", data.COUNTY.value_counts())

    return data

# %%-----------------------------------------------------------------------

######################## C. Get Summary Data ############################

# %%-----------------------------------------------------------------------
# specify the function to get summary of the tables

def get_summary_data(state):

    '''
    This function takes state parameter to get table data and then generate summary statistics for it

    :param state: string - name of the state for which summary data is required
    :return: dataframe - summary statistics of the table data
    '''

    #################### A. get table data #########################

    # call get_data function to get table data for respective tables
    Population_df = get_data('TOTAL POPULATION', 'B01003', state)
    Age_df = get_data('SEX BY AGE', 'B01001', state)
    Race_df = get_data('RACE', 'B02001', state)
    Household_df = get_data('HOUSEHOLD TYPE (INCLUDING LIVING ALONE)', 'B11001', state)
    Education_df = get_data('SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER', 'B15002', state)
    Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)', 'B19001', state)
    Median_Income_df = get_data('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)', 'B19013', state)
    NoOfHousing_df = get_data('HOUSING UNITS', 'B25001', state)
    Tenure_df = get_data('TENURE', 'B25003', state)
    TypeOfHousing_df = get_data('UNITS IN STRUCTURE', 'B25024', state)
    MedianValue_df = get_data('MEDIAN VALUE (DOLLARS)', 'B25077', state)
    AgeOfHousing_df = get_data('YEAR STRUCTURE BUILT', 'B25034', state)
    Tenure_by_year_df = get_data('TENURE BY YEAR STRUCTURE BUILT BY UNITS IN STRUCTURE','B25127',state)


    #################### B. get summary statistics #########################

    # start with Population_df dataframe

    # Population
    df= Population_df
    df = df.rename(columns={'Total':'Total_population'})

    # Age
    # get percent below age 18 and above 65
    df['percent_below_18'] = Age_df.iloc[:, np.r_[13:17,37:41]].apply(pd.to_numeric).sum(axis=1)/\
                             Age_df['Total:'].apply(pd.to_numeric)

    df['percent_above_65'] = Age_df.iloc[:, np.r_[30:36,54:60]].apply(pd.to_numeric).sum(axis=1)/\
                             Age_df['Total:'].apply(pd.to_numeric)

    # Race
    # get % of African-American and % of Non-whites
    df['percent_AA'] = Race_df['Black or African American alone'].apply(pd.to_numeric)/\
                       Race_df['Total:'].apply(pd.to_numeric)

    df['percent_NonWhite'] = (Race_df['Total:'].apply(pd.to_numeric)-Race_df['White alone'].apply(pd.to_numeric))/\
                             Race_df['Total:'].apply(pd.to_numeric)

    # Household Type
    # get percent of family households and married couples
    df['percent_FamilyHouseholds'] = Household_df['Family households:'].apply(pd.to_numeric)/\
                                     Household_df['Total:'].apply(pd.to_numeric)

    df['percent_MarriedCoupleFamily'] = Household_df['Married-couple family'].apply(pd.to_numeric)/\
                                        Household_df['Total:'].apply(pd.to_numeric)

    # Education
    # get education upto highschool, upto college and above college
    df['percent_less_than_bachelors'] = Education_df.iloc[:, np.r_[13:24,30:41]].apply(pd.to_numeric).sum(axis=1)/\
                                    Education_df['Total:'].apply(pd.to_numeric)

    df['percent_bachelors'] = Education_df.iloc[:, np.r_[24:26,41:43]].apply(pd.to_numeric).sum(axis=1)/\
                                 Education_df['Total:'].apply(pd.to_numeric)

    df['percent_above_bachelors'] = Education_df.iloc[:, np.r_[26:29,43:46]].apply(pd.to_numeric).sum(axis=1)/\
                                  Education_df['Total:'].apply(pd.to_numeric)


    print(Education_df.iloc[:, np.r_[13:24,30:41]].columns.tolist())
    # ['No schooling completed', 'Nursery to 4th grade', '5th and 6th grade', '7th and 8th grade', '9th grade',
    #  '10th grade', '11th grade', '12th grade, no diploma', 'High school graduate, GED, or alternative',
    #  'No schooling completed', 'Nursery to 4th grade', '5th and 6th grade', '7th and 8th grade', '9th grade',
    #  '10th grade', '11th grade', '12th grade, no diploma', 'High school graduate, GED, or alternative']

    print(Education_df.iloc[:, np.r_[24:26,41:43]].columns.tolist())
    # ['Some college, less than 1 year', 'Some college, 1 or more years, no degree', 'Some college, less than 1 year',
    #  'Some college, 1 or more years, no degree']

    print(Education_df.iloc[:, np.r_[26:29,43:46]].columns.tolist())
    # ["Associate's degree", "Bachelor's degree", "Master's degree", 'Professional school degree', 'Doctorate degree',
    #  "Associate's degree", "Bachelor's degree", "Master's degree", 'Professional school degree', 'Doctorate degree']

    # Income
    # get percent below income 50k and more than 200k
    df['percent_income_below_50k'] = Income_df.iloc[:, 12:21].apply(pd.to_numeric).sum(axis=1)/\
                                     Income_df['Total:'].apply(pd.to_numeric)

    df['percent_income_above_200k'] = Income_df['$200,000 or more'].apply(pd.to_numeric)/\
                                      Income_df['Total:'].apply(pd.to_numeric)

    # get median value of houses
    df['Median_household_income'] = Median_Income_df['Median household income in the past 12 months (in 2016 inflation-adjusted dollars)']

    #print(Income_df.iloc[:, 12:21].columns.tolist())

    # No. of Housing Units
    # get housing units per person
    df['total_housing_units'] = NoOfHousing_df['Total'].apply(pd.to_numeric)

    df['housing_units_per_person'] = NoOfHousing_df['Total'].apply(pd.to_numeric)/\
                                     df['Total_population'].apply(pd.to_numeric)


    # Tenure
    # get percent owner occupied
    df['percent_OwnerOccupied'] = Tenure_df['Owner occupied'].apply(pd.to_numeric)/Tenure_df['Total:'].apply(pd.to_numeric)

    # Type of housing
    # get percent of 1, attached and 1, detached type of housing
    df['percent_1_Units'] = TypeOfHousing_df.loc[:,['1, detached','1, attached']].apply(pd.to_numeric).sum(axis=1)/\
                            TypeOfHousing_df['Total:'].apply(pd.to_numeric)
    #
    # Value
    # get median value of houses
    df['Median_Value'] = MedianValue_df['Median value (dollars)']



    # Age of housing
    # get decade wise share of housing
    AgeOfHousing_df.iloc[:,12:] = AgeOfHousing_df.iloc[:,12:].apply(pd.to_numeric)

    # subset the age of housing dataset for logical record number and decade wise share of housing data
    AgeOfHousing_df_subset = AgeOfHousing_df.iloc[:,np.r_[5,12:22]]
    #print(AgeOfHousing_df.iloc[:, np.r_[5,12:22]].columns.tolist())

    # merge the age of housing data with summary dataframe
    tempDF = pd.merge(df,AgeOfHousing_df_subset,on="LOGRECNO")

    # get decade wise share of housing
    Tenure_by_year_df = Tenure_by_year_df.rename(columns={'Total:': 'total_occupied_housing_units'})

    print(Tenure_by_year_df.columns.tolist())

    Tenure_by_year_df.iloc[:, 11:] = Tenure_by_year_df.iloc[:, 11:].apply(pd.to_numeric)

     # subset the age of housing dataset for logical record number and decade wise share of housing data
    Tenure_by_year_df_subset = Tenure_by_year_df.iloc[:, np.r_[5, 11:98]]

    print(Tenure_by_year_df.columns.tolist())

    print(Tenure_by_year_df.iloc[:, np.r_[5, 11:98]].columns.tolist())

    # merge the age of housing data with summary dataframe
    mergedDF = pd.merge(tempDF, Tenure_by_year_df_subset, on="LOGRECNO")

    # print columns of summary dataframe
    print(mergedDF.columns.tolist())
    #print(mergedDf.head())

    #cols = pd.Series(mergedDF.columns)

    # for dup in mergedDF.columns.get_duplicates():
    #     #cols[df.columns.get_loc(dup)] = [dup + '.' + str(d_idx) if d_idx != 0 else dup for d_idx in range(df.columns.get_loc(dup).sum())]
    #     print(dup)

    #df.columns = cols

    mergedDF_T = mergedDF.T

    # mergedDF_MD_T.columns = mergedDF_MD_T.loc["COUNTY"]
    mergedDF_T.columns = list([state+'_'] * len(mergedDF_T.columns)) + (mergedDF_T.loc["COUNTY"])

    mergedDF_T = mergedDF_T.drop("COUNTY")

    return mergedDF_T



mergedDF_DC_T = get_summary_data('DC')
#print(mergedDF_DC.head())
#print(mergedDF_DC.shape)
mergedDF_MD_T = get_summary_data('MD')
mergedDF_VA_T = get_summary_data('VA')
mergedDF_WV_T = get_summary_data('WV')


mergedDF = pd.concat([mergedDF_DC_T, mergedDF_MD_T, mergedDF_VA_T, mergedDF_WV_T ], axis=1)

print(mergedDF.head())
print(mergedDF.columns.tolist())
print(mergedDF.index.tolist())


# creating multilevel index

df = mergedDF

estimate_end_year = list(mergedDF.loc['FILETYPE'])[0][:4]
estimate_start_year = str(int(estimate_end_year)-4)

df['estimate_year'] = [estimate_start_year+'-'+estimate_end_year]*len(mergedDF.index)
#
## df['estimate_year'] = [list(mergedDF.loc['FILETYPE'])[0][:4]]*len(mergedDF.index)
#
df = df.reset_index()

df = df.set_index(['estimate_year','index'])

df.columns.name = ""

print(df)

filename = datetime.datetime.today().strftime('%Y%m%d')+'_cnty_acs_'+estimate_start_year+'_'+estimate_end_year+'_absolute_values.csv'
print(filename)
print(type(filename))

save_file = os.path.join(outDir, str(filename))

print(save_file)


df.to_csv(save_file, sep=',')











