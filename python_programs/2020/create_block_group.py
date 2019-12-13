# Author: Chirag Jhamb
# create block group data for  B19013
# parses the file in Tracts_Block_Groups_Only folder
# saves file after adding all the columns of required tables and data for DC
# years: 2013 to 2017


# import geography file: state, county, tract, block group, summary level, logrecno

import pandas as pd
import datetime

# start_year = '2013'
# end_year = '2017'
state = "dc"
# 3 temp values for testing, to be deleted
# table_id = 'B19001'
# table_name = 'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)'
# block group data to be collected for these tables, table id
# B19013
Income_df = ('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19001')
Median_Income_df = ('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19013')
Household_Income_B19080_df = ("HOUSEHOLD INCOME QUINTILE UPPER LIMITS","B19080")
Mean_Household_Income_B19081_df = ("MEAN HOUSEHOLD INCOME OF QUINTILES","B19081")


def documentation_file_data(table_id, table_name):  # reads the documentation file, gets the column name and row location of each detail
    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    if int(end_year) < 2013:
        docFilePath = inDir + '/Sequence_Number_and_Table_Number_Lookup.xls'
    else:
        docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.xls'
    dataDir = inDir + '/group1'
    # documentation file table IDs, names and row location
    documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)
    # folder having geography file:
    path = inDir + '/geog/g' + end_year + '5' + state.lower() + '.txt'
    # subset the data to the required table id, only to  get start position mentioned in the first row
    data_records = documentation_file[documentation_file["Table ID"]==table_id]
    # get start position mentioned in the first row
    start_pos = int(data_records["Start Position"].iloc[0])-1
    # subset the data where line isn't blank
    data_records = documentation_file[(documentation_file["Table ID"]==table_id) & (documentation_file["Line Number"]!='')]
    data_location = {}
    # get row location and column name of each item in table id:
    for i,r in data_records.iterrows():
        seq_number = int(str(r['Sequence Number']).strip())
        try:
            if float(r["Line Number"]).is_integer():   # to avoid the decimal 0.5 in some cases
                if table_id=="B19013":
                    data_location[r["Line Number"]+start_pos-1] = table_id+ "_Median_household_income_in_the_past_12_months"  #to get rid of year in column name, causing multiple columns at merge
                else:
                    data_location[r["Line Number"]+start_pos-1] = table_id+ "_" + r["Table Title"].replace(":","").replace(" ","_")
        except ValueError:  # to encounter erro when empty cell is parsed
            pass
    # get cblock_group for each logrecno
    result = []
    with open(path, 'rb') as f:
        for row in f:
            state_fips = row[25:27].decode('utf-8', 'ignore')
            county = row[27:30].decode('utf-8', 'ignore')
            sumlevel = row[8:11].decode('utf-8', 'ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            block_group = row[40:46].decode('utf-8', 'ignore')
            tract = row[46:47].decode('utf-8', 'ignore')
            result.append([sumlevel, logrecno,state.lower(),block_group, tract, state_fips, county])
    # convert to DF
    file1_result = pd.DataFrame(result, columns = ["sumlevel", "logrecno","state","blockgrp", "tract","statefips", "countyfips"])
    # set index ready for merge:
    file1_result["mkey"] = file1_result["state"] + file1_result["logrecno"]
    file1_result = file1_result.set_index(["mkey"])

    print(file1_result.head())
    return file1_result,data_location,seq_number

def tracts_block_groups_data(seq_number, data_location):
    result = []
    # load one table (not the geography data..
    dataDir = "/groups/brooksgrp/census/american_community_survey/"+start_year+"_"+end_year+"_5year_estimates/raw_data/Tracts_Block_Groups_Only"
    path = dataDir + '/e' + end_year + '5' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'
    print(path)
    # open path and get data from each row, using the data_location obtained from documentation_file_data function
    with open(path, 'rb') as f:
        for row in f:
            to_add = []
            cols = []

            to_add.append(row.decode('utf-8', 'ignore').split(",")[5])  # geting logrecno
            to_add.append(row.decode('utf-8', 'ignore').split(",")[2])  # getting state
            cols.append("logrecno")
            cols.append("state")
            # get data from each row as per data_location
            for i in data_location.items():
                to_add.append(row.decode('utf-8', 'ignore').split(",")[i[0]])
                cols.append(i[1])
            # append everything to result, to be converted to DF
            result.append(to_add)
    # convert to DF
    file2_result = pd.DataFrame(result, columns = cols)
    print("shape:",file2_result.shape)
    # set index ready for merge:
    file2_result["mkey"] = file2_result["state"]+file2_result["logrecno"]
    file2_result = file2_result.set_index(["mkey"])
    del file2_result["state"]  # not required
    del file2_result["logrecno"]   # was required only for indexing
    return file2_result
    # print(file2_result.head())
    # final_df = pd.concat([file2_result, file1_result], axis=1, join = 'inner', sort = True)
    # return final_df
end_years = ['2017', '2013']
all_files_per_year = []
for year in end_years:
    print(year)
    start_year = str(int(year) - 4)
    end_year = year
    # for data_index in [Median_Income_df]:
    data_index = Median_Income_df
    table_name = data_index[0]
    table_id = data_index[1]
    print(table_id, table_name)
    # gets the column name and row location of each detail:
    df_documentation_file, data_location, seq_number = documentation_file_data(table_id, table_name)
    print(df_documentation_file.head())
    print(data_location, seq_number)
    # get other data which is required:
    data_file = tracts_block_groups_data(seq_number, data_location)
    print(data_file.head())
    # merge then two by their index:
    final_file = pd.concat([data_file, df_documentation_file], axis=1, join = 'inner', sort = True)
    print("Returned merged file:")
    print(final_file.head())
    print("\nSummary level 150:")
    # subset file to only contain summary_level 150
    subfile = final_file[final_file["sumlevel"]=='150']
    subfile["start_year"] = start_year
    subfile["end_year"] = end_year
    print(subfile.head())
    print(subfile.shape)
    all_files_per_year.append(subfile)
print("Final concat", len(all_files_per_year), " files")
final_file = pd.concat(all_files_per_year, axis=0)
final_file["statefips"] = state_code
# convert all to values to constant strings, to keep leading zeros in a column when exported to csv
for i in final_file.columns:
    final_file[i] = final_file[i].astype("str")
print(final_file.head())
save_location = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/block_group_data/"
filename = save_location + datetime.datetime.today().strftime('%Y%m%d') + '_block_group_tract_data.csv'
final_file.to_csv(filename, index=False)
print("File saved",filename)
