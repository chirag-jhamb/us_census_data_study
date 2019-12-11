# Author: Chirag Jhamb
# create block group data for B19001, B19013, B19080, B19081
# parses the file in Tracts_Block_Groups_Only folder
# put a header that says what this program is

# comment all parts

# import geography file: state, county, tract, block group, summary level, logrecno

import pandas as pd
import datetime

start_year = '2013'
end_year = '2017'
state = 'DC'


table_id = 'B19001'
table_name = 'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)'
Income_df = ('HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19001', "59")
Median_Income_df = ('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19013',"59")
Household_Income_B19080_df = ("HOUSEHOLD INCOME QUINTILE UPPER LIMITS","B19080","63")
Mean_Household_Income_B19081_df = ("MEAN HOUSEHOLD INCOME OF QUINTILES","B19081","63")
seq_number = "59"

def get_dataframe(table_id, table_name, seq_number):
    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    dataDir = inDir + '/group1'

    # documentation file table IDs, names and row location
    docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.xls'
    documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)
# folder having geography file:
    path = inDir + '/geog/g' + end_year + '5' + state.lower() + '.txt'
    data_records = documentation_file[documentation_file["Table Title"]==table_name]
    start_pos = int(data_records["Start Position"].iloc[0])-1
    data_records = documentation_file[(documentation_file["Table ID"]==table_id) & (documentation_file["Line Number"]!='')]
    data_location = {}
    for i,r in data_records.iterrows():
        data_location[r["Line Number"]+start_pos] = r["Table Title"].replace(":","")

    result = []
    with open(path, 'rb') as f:
        for row in f:
            sumlevel = row[8:11].decode('utf-8', 'ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            result.append([sumlevel, logrecno,state.lower()])

    file1_result = pd.DataFrame(result, columns = ["sumlevel", "logrecno","state"])
    file1_result["mkey"] = file1_result["state"] + file1_result["logrecno"]
    file1_result = file1_result.set_index(["mkey"])

    # keep just summary level 150
    print(file1_result.head())
    # load one table (not the geography data..

    # seq_number = "59"
    path = dataDir + '/e' + end_year + '5' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'

    result = []
    dataDir = "/groups/brooksgrp/census/american_community_survey/2013_2017_5year_estimates/raw_data/Tracts_Block_Groups_Only"
    path = dataDir + '/e' + end_year + '5' + state.lower() + '0' + str(seq_number).zfill(3) + '000.txt'
    print(path)

    with open(path, 'rb') as f:
        for row in f:
            to_add = []
            cols = []
            to_add.append(row.decode('utf-8', 'ignore').split(",")[5])
            to_add.append(row.decode('utf-8', 'ignore').split(",")[2])
            cols.append("logrecno")
            cols.append("state")
            for i in data_location.items():
                to_add.append(row.decode('utf-8', 'ignore').split(",")[i[0]])
                cols.append(i[1])
            result.append(to_add)

    file2_result = pd.DataFrame(result, columns = cols)
    print("shape:",file2_result.shape)
    file2_result["mkey"] = file2_result["state"]+file2_result["logrecno"]
    file2_result = file2_result.set_index(["mkey"])

    print(file2_result.head())
    final_df = pd.concat([file2_result, file1_result], axis=1, join = 'inner', sort = True)
    return final_df


print(table_id, table_name, seq_number)
final_file = get_dataframe(table_id, table_name, seq_number)
print("Returned merged file:")
print(final_file.head())

print("\nSummary level 150:")
subfile = final_file[final_file["sumlevel"]=='150']
print(subfile.head())
print(subfile.shape)
#
# filename = datetime.datetime.today().strftime('%Y%m%d') + '_B19001_' + start_year + '_' + end_year + '_data.csv'
# subfile.to_csv("~/"+filename)
# print("File saved",filename)
