# %%%%%%%%%%%%% Cenus Data Import and Table Generation %%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%% Authors  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Prof Leah Brooks------>Email: lfbrooks@gwu.edu
# Chirag Jhamb------->chirag@gwu.edu
# %%%%%%%%%%%%% Date:
# V1 February - 14 - 2020
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%-----------------------------------------------------------------------

# Importing the required packages
import numpy as np
import pandas as pd
import os, datetime
from functools import reduce
exec(open("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/data_functions.py").read())
top_msa_counties, top_msa_by_state, top_msas_dict = get_top_msa()
state_codes = get_state_code()
county_states = dict([[v,k] for k,v in state_codes.items()])
# %%-----------------------------------------------------------------------
######################## A. Read Documentation File ############################

# %%-----------------------------------------------------------------------
######################## B. Get Table Data ############################

def get_summary_year_data(start_year, end_year, msa):
    start_year = str(start_year)
    end_year = str(end_year)

    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    outDir = r"/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_data"

    # %%-----------------------------------------------------------------------
    # specify function to collect the data for required table and save it as a csv

    def get_data(table_title, table_id, state):
        print("table_title, table_id, state",table_title, table_id, state)
        '''
        This function takes the table name, table id and state as input and iterates over
        documentation file to get the table name, table id and respective columns,
        and gets the data from sequence file and geography file

        :param table_title: string - name of the table
        :param table_id: string - table id
        :param state: string - name of the state
        :return: dataframe - table data
        '''



        # Import the documentation file as dataframe
        if int(end_year)==2018:
            docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.csv'
            documentation_file = pd.read_csv(docFilePath, header=0, dtype=object, na_filter=False)
        else:
            # Specifying the path for documentation file
            if int(end_year) < 2013:
                docFilePath = inDir + '/Sequence_Number_and_Table_Number_Lookup.xls'
            else:
                docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.xls'
            documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)

        # Printing the dataset observations
        # print(documentation_file.head())
        #
        # # Printing the dataset columns
        # print(documentation_file.columns)
        #
        # print(documentation_file.info())
        # print(documentation_file.isnull().sum(axis=0))

        print('fetching data for state: ', state)

        # Specifying the path for census data files
        dataDir = inDir + '/group1'

        # dataDir = data_path + state
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
                # print(current_table_title)

            # get sequence number and start position
            # if table title matches with what we have given and start position is not null
            if current_table_title == table_title and row['Start Position'] and row['Table ID'] == table_id:
                # print(current_table_title, row['Table ID'])
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
            # if current_table_title==table_title and row['Total Cells in Sequence']:
            #   max_cols = row['Total Cells in Sequence']

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
        # print(seq_number)
        # print(start_pos)

        # print length of column list and column names
        # print(len(col_names))
        # print(col_names)

        col_names_updated = ['FILEID', 'FILETYPE', 'STATE', 'CHARITER', 'SEQUENCE', 'LOGRECNO'] + col_names
        print("updated col names", col_names_updated)

        # total_cols =start_pos+len(col_names)

        # if total_cols>max_cols:
        #   print("In here or not")
        #  cols_diff = total_cols-max_cols
        # col_names = col_names[:-cols_diff]

        # provide the path of the file in the data directory using sequence number
        path = dataDir + '/e' + end_year + '5' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'
        print(path)

        # find the ending position of the column list
        end_pos = len(col_names) + start_pos - 1
        print(end_pos)

        # if end_pos>max_cols:
        #   end_pos=max_cols+1

        # print(list(range(start_pos - 1, end_pos+1)))

        # specify the column numbers as list to be taken from the sequence file
        col_list = list(range(start_pos - 1, end_pos))
        # print(col_list)

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
        path = inDir + '/geog/g' + end_year + '5' + state.lower() + '.txt'

        # create empty dictionaries for saving geoid and geoname for each logical record no.
        geo_geoid = {}
        geo_geoname = {}
        geo_sumlevel = {}
        geo_state = {}
        geo_county = {}
        # geo_zcta = {}

        # open the geography file and extract requried values
        with open(path, 'rb') as f:
            for row in f:
                sumlevel = row[8:11].decode('utf-8', 'ignore')
                logrecno = row[13:20].decode('utf-8', 'ignore')
                state_fips = row[25:27].decode('utf-8', 'ignore')
                county = row[27:30].decode('utf-8', 'ignore')
                # zcta = row[130:135].decode('utf-8', 'ignore')
                geoid = row[178:218].strip().decode('utf-8', 'ignore')
                gname = row[218:].strip().decode('utf-8', 'ignore')
                geo_geoid[logrecno] = geoid
                geo_geoname[logrecno] = gname
                geo_sumlevel[logrecno] = sumlevel
                geo_state[logrecno] = state_fips
                geo_county[logrecno] = county
            # geo_zcta[logrecno] = zcta

        # print(geo_state)

        # for matching logical record numbers insert columns from geography file in table data
        data.insert(loc=6, column="SUMMARY_LEVEL", value=data["LOGRECNO"].map(geo_sumlevel))
        data.insert(loc=7, column="STATE_FIPS", value=data["LOGRECNO"].map(geo_state))
        data.insert(loc=8, column="COUNTY", value=data["LOGRECNO"].map(geo_county))
        # data.insert(loc=9, column="ZCTA", value=data["LOGRECNO"].map(geo_zcta))
        data.insert(loc=9, column="GEO_ID", value=data["LOGRECNO"].map(geo_geoid))
        data.insert(loc=10, column="GEO_NAME", value=data["LOGRECNO"].map(geo_geoname))

        # print the dimension of the dataset
        print(data.shape)

        # print the column names of the dataset
        print(data.columns)
        # print first few rows of the dataset
        print(data.head())
        # print(data["SUMMARY_LEVEL"], data["SUMMARY_LEVEL"].unique())

        # use the table title name to save the table data
        table_name = table_title.replace(" ", "_") + "_state_" + end_year + ".csv"
        print("table_name",table_name)

        # specify the path to save the data
        save_file = os.path.join(outDir, table_name)

        # save the data
        # data.to_csv(save_file, sep=',', index=False)

        # subset the data for summary_level 050 i.e. metro area
        data = data.query('SUMMARY_LEVEL=="050"')

        # subset the data for zcta 860 i.e. metro area
        # data = data.query('ZCTA=="860"')

        # for respective state subset for require counties

        # try:
        county_codes = top_msa_by_state[msa][county_states[state]]
        data = data[data["COUNTY"].isin(county_codes)]
        # except:
        #     print(state + ' is not in the list')

        print("Number of counties", data.COUNTY.value_counts())

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
        Income_df = get_data('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19001', state)
        Median_Income_df = get_data('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19013',state)
        NoOfHousing_df = get_data('HOUSING UNITS', 'B25001', state)
        Tenure_df = get_data('TENURE', 'B25003', state)
        TypeOfHousing_df = get_data('UNITS IN STRUCTURE', 'B25024', state)
        MedianValue_df = get_data('MEDIAN VALUE (DOLLARS)', 'B25077', state)
        AgeOfHousing_df = get_data('YEAR STRUCTURE BUILT', 'B25034', state)
        HouseholdFamily_df = get_data('HOUSEHOLDS BY PRESENCE OF PEOPLE UNDER 18 YEARS BY HOUSEHOLD TYPE','B11005',state)

        if int(end_year)>2010:
            Tenure_by_year_df = get_data('TENURE BY YEAR STRUCTURE BUILT BY UNITS IN STRUCTURE', 'B25127', state)

        # FamilyType_by_children_df = get_data('FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS', 'B11003',state)
        # Owner_Occupied_Housing_Value_df = get_data('VALUE', 'B25075', state)
        Race_Hispanic_Latino_df = get_data('HISPANIC OR LATINO ORIGIN BY RACE', 'B03002', state)
        # #Contract_Rent_df = get_data('CONTRACT RENT', 'B25056', state)
        # Gross_Rent_df = get_data('GROSS RENT', 'B25063', state)
        # Gross_Rent_percent_df = get_data('MEDIAN GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS (DOLLARS)', 'B25071', state)
        # Tenure_year_df = get_data('TENURE BY YEAR STRUCTURE BUILT', 'B25036', state)
        hosuehold_size_df = get_data('HOUSEHOLD TYPE BY HOUSEHOLD SIZE', 'B11016', state)
# B11016

        Age_df_subset = Age_df.iloc[:, np.r_[5, 11:len(Age_df.columns)]]
        Race_df_subset = Race_df.iloc[:, np.r_[5, 11:len(Race_df.columns)]]
        Household_df_subset = Household_df.iloc[:, np.r_[5, 11:len(Household_df.columns)]]
        Education_df_subset = Education_df.iloc[:, np.r_[5, 11:len(Education_df.columns)]]
        Income_df_subset = Income_df.iloc[:, np.r_[5, 11:len(Income_df.columns)]]
        Median_Income_df_subset = Median_Income_df.iloc[:, np.r_[5, 11:len(Median_Income_df.columns)]]
        NoOfHousing_df_subset = NoOfHousing_df.iloc[:, np.r_[5, 11:len(NoOfHousing_df.columns)]]
        Tenure_df_subset = Tenure_df.iloc[:, np.r_[5, 11:len(Tenure_df.columns)]]
        TypeOfHousing_df_subset = TypeOfHousing_df.iloc[:, np.r_[5, 11:len(TypeOfHousing_df.columns)]]
        MedianValue_df_subset = MedianValue_df.iloc[:, np.r_[5, 11:len(MedianValue_df.columns)]]
        AgeOfHousing_df_subset = AgeOfHousing_df.iloc[:, np.r_[5, 11:len(AgeOfHousing_df.columns)]]
        HouseholdFamily_df_subset = HouseholdFamily_df.iloc[:, np.r_[5, 11:len(HouseholdFamily_df.columns)]]
        if int(end_year) > 2010:
            Tenure_by_year_df_subset = Tenure_by_year_df.iloc[:, np.r_[5, 11:len(Tenure_by_year_df.columns)]]

        # FamilyType_by_children_df_subset = FamilyType_by_children_df.iloc[:, np.r_[5, 11:len(FamilyType_by_children_df.columns)]]
        # Owner_Occupied_Housing_Value_df_subset = Owner_Occupied_Housing_Value_df.iloc[:,np.r_[5, 11:len(Owner_Occupied_Housing_Value_df.columns)]]
        Race_Hispanic_Latino_df_subset = Race_Hispanic_Latino_df.iloc[:, np.r_[5, 11:len(Race_Hispanic_Latino_df.columns)]]
        # Gross_Rent_df_subset = Gross_Rent_df.iloc[:, np.r_[5, 11:len(Gross_Rent_df.columns)]]
        # Gross_Rent_percent_df_subset = Gross_Rent_percent_df.iloc[:, np.r_[5, 11:len(Gross_Rent_percent_df.columns)]]
        # Tenure_year_df_subset = Tenure_year_df.iloc[:, np.r_[5, 11:len(Tenure_year_df.columns)]]
        hosuehold_size_df_subset = hosuehold_size_df.iloc[:, np.r_[5, 11:len(hosuehold_size_df.columns)]]


        # create a list of all dataframes to merge them together
        # data_frames = [Population_df, Age_df_subset, Race_df_subset, Household_df_subset, Education_df_subset, Income_df_subset,
        #                Median_Income_df_subset, NoOfHousing_df_subset, Tenure_df_subset,
        #                TypeOfHousing_df_subset, MedianValue_df_subset, AgeOfHousing_df_subset, Tenure_by_year_df_subset,
                       # FamilyType_by_children_df_subset, Owner_Occupied_Housing_Value_df_subset, Race_Hispanic_Latino_df_subset, Gross_Rent_df_subset,
        #                Gross_Rent_percent_df_subset]

        print("Age_df:",Age_df.shape)
        print("Population_df:",Population_df.shape)
        print("Age_df_subset:",Age_df_subset.shape)
        if int(end_year) > 2010:
            data_frames = [Population_df, Age_df_subset, Race_df_subset, Household_df_subset, Education_df_subset,
                       Income_df_subset, Median_Income_df_subset, NoOfHousing_df_subset, Tenure_df_subset,
                       TypeOfHousing_df_subset, MedianValue_df_subset, AgeOfHousing_df_subset, Tenure_by_year_df_subset,
                       # ,FamilyType_by_children_df_subset, Owner_Occupied_Housing_Value_df_subset,
                       Race_Hispanic_Latino_df_subset, HouseholdFamily_df_subset, hosuehold_size_df_subset
                       # Gross_Rent_df_subset, Gross_Rent_percent_df_subset, Tenure_year_df_subset
                       ]
        else:
            data_frames = [Population_df, Age_df_subset, Race_df_subset, Household_df_subset, Education_df_subset,
                           Income_df_subset, Median_Income_df_subset, NoOfHousing_df_subset, Tenure_df_subset,
                           TypeOfHousing_df_subset, MedianValue_df_subset, AgeOfHousing_df_subset,
                           # ,FamilyType_by_children_df_subset, Owner_Occupied_Housing_Value_df_subset,
                           Race_Hispanic_Latino_df_subset, HouseholdFamily_df_subset, hosuehold_size_df_subset
                           # , Gross_Rent_df_subset,Gross_Rent_percent_df_subset, Tenure_year_df_subset
                           ]

        # merge all the dataframes together
        # print("\nmerging data_frames",data_frames)
        mergedDF = reduce(lambda left, right: pd.merge(left, right, on=['LOGRECNO'], how='outer'), data_frames)
        #
        # # print columns of summary dataframe
        # print(mergedDF.columns.tolist())
        # print(mergedDF.head())

        # cols = pd.Series(mergedDF.columns)

        # for dup in mergedDF.columns.get_duplicates():
        #     #cols[df.columns.get_loc(dup)] = [dup + '.' + str(d_idx) if d_idx != 0 else dup for d_idx in range(df.columns.get_loc(dup).sum())]
        #     print(dup)

        # df.columns = cols

        mergedDF_T = mergedDF.T

        # mergedDF_MD_T.columns = mergedDF_MD_T.loc["COUNTY"]
        # print("\n\n mergedDF_T:",mergedDF_T)
        mergedDF_T.columns = list([state + '_'] * len(mergedDF_T.columns)) + (mergedDF_T.loc["COUNTY"])
        # print(mergedDF_T.columns)
        mergedDF_T = mergedDF_T.drop("COUNTY")
        # print(mergedDF_T.head())
        return mergedDF_T

    DF_list = []
    for st in top_msa_by_state[msa].keys():
        DF_list.append(get_summary_data(state_codes[st]))

    mergedDF = pd.concat(DF_list, axis=1)
    # print(mergedDF.head())
    # print(mergedDF.columns.tolist())
    # print(mergedDF.index.tolist())

    # creating multilevel index

    df = mergedDF
    estimate_end_year = list(mergedDF.loc['FILETYPE'])[0][:4]
    estimate_start_year = str(int(estimate_end_year) - 4)

    df['estimate_year'] = [estimate_start_year + '-' + estimate_end_year] * len(mergedDF.index)
    #
    ## df['estimate_year'] = [list(mergedDF.loc['FILETYPE'])[0][:4]]*len(mergedDF.index)
    #
    df = df.reset_index()

    df = df.set_index(['estimate_year', 'index'])

    # print(df.columns.name)
    #
    df.columns.name = ""
    #
    # print(df)
    return df


#
#

#
# end_year = ['2013', '2014', '2015', '2016', '2017','2011', '2012']
#
end_year = ['2018']
for MSA in top_msa_counties.keys():
    data = pd.DataFrame()
    for year in end_year:
        dft = get_summary_year_data(str(int(year) - 4), year, MSA)
        print(dft.shape)
        filename = datetime.datetime.today().strftime('%Y%m%d') + '_cnty_acs_' + year + '_' + str(MSA) + '_population.csv'
        print("Writing to:",filename)
        dft.to_csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_msa/"+filename)
        print("\nFile saved:\n","/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_msa/"+filename)
# filename = datetime.datetime.today().strftime('%Y%m%d') + '_cnty_acs_all_years_population.csv'
# data = data.set_index(['estimate_year', 'index'])
# data.to_csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_msa/"+filename)#, index=False)
# print("\nFile saved", filename)
