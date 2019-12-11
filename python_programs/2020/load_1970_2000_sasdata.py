# AUthor: chirag Jhamb
# This file reads sas7bdat files in folder /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census
# these sas7bdat files were created from /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_programs/2020/load_county_census/stackyrsv07.sas
# The program stacks the files data, puts the FIPS in the same column and outputs it /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/"+dateo+"_1970_to_2000_county_census.csv

import datetime
import pandas as pd
from sas7bdat import SAS7BDAT
dateo = datetime.datetime.today().strftime('%Y%m%d')
# file locations:
# these sas7bdat files were created from /groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_programs/2020/load_county_census/stackyrsv07.sas
data_file_1970 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1970_20190915.sas7bdat"
data_file_1980 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1980_20190915.sas7bdat"
data_file_1990 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1990_20190915.sas7bdat"
data_file_2000 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc2000_20190915.sas7bdat"
output_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/"+dateo+"_1970_to_2000_county_census.csv"

# for fixing the format of fips. For state pass code_length=2 (To convert to SS), for county code_length=3 (to convert to CCC) and for SSCCC pass code_length=5
def fix_fip_code(fip_code, code_length):
    fip_code = str(fip_code).split(".")[0]    # convert to string and remove any decimals
    while len(fip_code)<code_length:
        fip_code = "0" + fip_code
    return fip_code

#given a location, the function reads the file and returns in a dataframe format
def read_sas(file_location):
    with SAS7BDAT(file_location) as reader:
        df = reader.to_data_frame()
    print("\n finished reading",file_location)
    return df

file_dict = {"1970":data_file_1970, "1980":data_file_1980, "1990":data_file_1990, "2000":data_file_2000}
columns_select = {"1970":["m17"], "1980":["t1","t2","t3"], "1990":["P1"], "2000":["P1", "P8", "P7"]}
# create empty df for later inputs:
output_dataframe = pd.DataFrame()
# loop through each file and add fix FIPS code for each
for fl in file_dict.items():
    yr = fl[0]
    df = read_sas(fl[1])
    yr_columns_select = ['FIPS', 'year']
    for j in columns_select[fl[0]]:
        yr_columns_select.extend([i for i in df.columns if i.startswith(j)])
    if yr=="1970":
        df['FIPS'] = df['st70'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['cnty'].astype(int).apply(lambda x: fix_fip_code(x,3))
    elif yr=="1980":
        df["FIPS"] = df["fipsstate"] + df["COUNTY"]
    elif yr=="1990":
        df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['CNTY'].astype(int).apply(lambda x: fix_fip_code(x,3))
    else:
        df["FIPS"] = df["STATE"] + df["COUNTY"]
    df["year"] = yr
    df = df[yr_columns_select]
    output_dataframe = output_dataframe.append(df, ignore_index = True)
# save the output
output_dataframe.to_csv(output_file, index=False)
