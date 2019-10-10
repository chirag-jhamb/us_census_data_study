# Author: Chirag Jhamb, chirag@gwmail.gwu.edu
# this program reads all the sas7bdat files and calculates the total population for the year intervals for each MSA as well as all counties of DMV and outputs it in a csv
from sas7bdat import SAS7BDAT
import pandas as pd
from collections import Counter
import datetime
dateo = datetime.datetime.today().strftime('%Y%m%d')
print("\n",dateo," running appendix table")
#here are all the file paths that are used in the program:
data_file_1970 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1970_20190915.sas7bdat"
data_file_1980 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1980_20190915.sas7bdat"
data_file_1990 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1990_20190915.sas7bdat"
data_file_2000 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc2000_20190915.sas7bdat"
output_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/appendix_1970_2000_"+dateo+".csv"
#MSA file having all MSAs by population and each county
msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"

def fix_fip_code(fip_code, code_length):  # for fixing the format of fips. For state pass code_length=2 (To convert to SS), for county code_length=3 (to convert to CCC) and for SSCCC pass code_length=5
    fip_code = str(fip_code).split(".")[0]    # convert to string and remove any decimals
    while len(fip_code)<code_length:
        fip_code = "0" + fip_code
    return fip_code

def get_top_msa(msa_file, top_n = 10):    # returns the counties in top_n(10 by default) MSAs by population in a dictionary format {MSA_code: list of FIPS of counties in the MSA}
    msa_dataframe = pd.read_csv(msa_file, encoding = "ISO-8859-1")
    msa_cols = ["CSA","CBSA","NAME","LSAD","CENSUS2010POP"]
    all_msas = msa_dataframe[msa_dataframe["LSAD"]=="Metropolitan Statistical Area"][msa_cols]   #select all Metropolitan Statistical Areas
    top_msas = all_msas.sort_values('CENSUS2010POP',ascending = False).head(top_n)     # select top_n MSAs by population
    top_msas_dict = {i:j for i,j in zip(list(top_msas["CSA"]),list(top_msas["NAME"]))}    # create a dictionary of names of MSAs w.r.t. their code (CSA)
    top_msas = list(top_msas["CSA"])
    top_msa_county_data = msa_dataframe.loc[(msa_dataframe["CSA"].isin(top_msas)) & (msa_dataframe["LSAD"]=="County or equivalent")]   # select all counties which are in the top_n MSAs, get rid of all other types (MSAs, CSAs etc)
    top_msa_counties = {}
    for i in top_msas:    #loop over the codes of top_n MSAs and create a dictionary having list of the FIPs of counties in each MSA
        top_msa_counties[i] = [fix_fip_code(i, 5) for i in list(top_msa_county_data.loc[top_msa_county_data["CSA"]==i]["STCOU"])]
    print("Got top 10 MSAs!")
    return top_msa_counties, top_msas_dict

def read_sas(file_location):   #given a location, the function reads the file and returns in a dataframe format
    with SAS7BDAT(file_location) as reader:
        df = reader.to_data_frame()
    print("\n finished reading",file_location)
    return df

def get_population_msa(df, col_name):  #takes dataframe and a column or a list of columns, iterates through all msas and returns the sum of population for each MSA
    cntr = Counter()
    for k,v in top_msa_counties.items():
        for i in v:
            while len(i)!=5:   #fix for fips in a format other than SSCCC
                i = "0" + i
            if isinstance(col_name,list):
                population = df[df["FIPS"]==i][col_name].sum(axis=1)
            else:
                population = df[df["FIPS"]==i][col_name]
            try:   #for missing FIPS
                cntr[k] += int(population)
            except:
                print("--> Missing FIPS detected:",i,population)
    return cntr

def get_population_county_level(df, col_name, msa_code = 548):  #takes dataframe and a column or a list of columns and code of MSA, iterates through all counties in the msa and returns population in ther county. By default claculates for dmv MSA
    cntr = Counter()
    for i in top_msa_counties[msa_code]:
        while len(i)!=5:   #fix for fips in a format other than SSCCC
            i = "0" + i
        if isinstance(col_name,list):
            population = df[df["FIPS"]==i][col_name].sum(axis=1)
        else:
            population = df[df["FIPS"]==i][col_name]
        try:   #for missing FIPS
            cntr[i] = int(population)
        except:
            print("--> Missing FIPS detected:",i,population)
    return cntr


top_msa_counties, top_msas_dict = get_top_msa(msa_file)

# For 1970, read the file, get column that gives total population. Fix FIPS code to get format SSCCC and then calculate total population for MSAs and DMV:
df = read_sas(data_file_1970)
# get all m17 columns in the 1970 data, it has all the age categories. Summing them up gives the total population
m17 = [i for i in df.columns if "m17" in i]
df['FIPS'] = df['st70'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['cnty'].astype(int).apply(lambda x: fix_fip_code(x,3))
msa_population_1970 = get_population_msa(df, m17)
dmv_population_1970 = get_population_county_level(df, m17)

# For 1980, read the file, get column that gives total population. Fix FIPS code to get format SSCCC and then calculate total population for MSAs and DMV:
df = read_sas(data_file_1980)
df["FIPS"] = df["fipsstate"] + df["COUNTY"]
msa_population_1980 = get_population_msa(df, "t3_1")
dmv_population_1980 = get_population_county_level(df, "t3_1")

# For 1990, read the file, get column that gives total population. Fix FIPS code to get format SSCCC and then calculate total population for MSAs and DMV:
df = read_sas(data_file_1990)
df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['CNTY'].astype(int).apply(lambda x: fix_fip_code(x,3))
msa_population_1990 = get_population_msa(df, "P1_1")
dmv_population_1990 = get_population_county_level(df, "P1_1")

# For 2000, read the file, get column that gives total population. Fix FIPS code to get format SSCCC and then calculate total population for MSAs and DMV:
df = read_sas(data_file_2000)
df["FIPS"] = df["STATE"] + df["COUNTY"]
msa_population_2000 = get_population_msa(df, "P1_1")
dmv_population_2000 = get_population_county_level(df, "P1_1")

# Make an empty array, append output values in form a list. The array will become a DataFrame
result = []
print("\nCreating final output DataFrame")
for i in top_msa_counties.keys():  #append each MSA
    result.append([top_msas_dict[i],i,msa_population_1970[i],msa_population_1980[i],msa_population_1990[i],msa_population_2000[i], "MSA"])
msa_code = 548  #DMV area
for i in top_msa_counties[msa_code]:
    result.append([top_msas_dict[msa_code],i,dmv_population_1970[i],dmv_population_1980[i],dmv_population_1990[i],dmv_population_2000[i], "county"])

res = pd.DataFrame(result, columns=["MSA","FIPS","1970","1980","1990","2000", "type"])
print("Writing results to:",output_file)
res.to_csv(output_file, index=False)
