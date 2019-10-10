#Author: chirag jhamb
# this program reads all the sas7bdat files and calculates the total population for the year intervals for each MSA as well as all counties of DMV and outputs it in a csv

from sas7bdat import SAS7BDAT
import pandas as pd
from collections import Counter
#here are all the file paths that are used in the program:
data_file_1970 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1970_20190915.sas7bdat"
data_file_1980 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1980_20190915.sas7bdat"
data_file_1990 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1990_20190915.sas7bdat"
data_file_2000 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc2000_20190915.sas7bdat"
output_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/appendix_1970_2000_20191009.csv"
#MSA file having all MSAs by population and each county
msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"

def format_fip_st(st_fip):  #this function takes state code as input, returns it in the proper format as it should be: SS
    while len(str(st_fip))!=2:
        st_fip = "0"+ str(st_fip)
    return str(st_fip)

def format_fip_cnt(cnt_fip):  #this function takes county code as input, returns it in the proper format as it should be: CCC
    while len(str(cnt_fip))!=3:
        cnt_fip = "0"+ str(cnt_fip)
    return str(cnt_fip)

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
        top_msa_counties[i] = [str(int(i)) for i in list(top_msa_county_data.loc[top_msa_county_data["CSA"]==i]["STCOU"])]
    return top_msa_counties, top_msas_dict
top_msa_counties, top_msas_dict = get_top_msa(msa_file)


str_age = """under 3
3-4
5
6
7-9
10-13
14
15
16
17
18
19
20
21
22-24
25-29
30-34
35-39
40-44
45-49
50-54
55-59
60-61
62-64
65-69
70-74
75 and over"""

m17 = [i for i in df.columns if "m17" in i]
ages = ["male_"+i.replace("-","_").replace(" ","_") for i in str_age.split("\n")] + ["female_"+i.replace("-","_").replace(" ","_") for i in str_age.split("\n")]
age_cols = {i:j for i,j in zip(m17, ages)}

def get_population_msa(df, col_name):  #takes dataframe and a column or a list of columns, iterates through all msas and returns the sum of population for each MSA
    cntr = Counter()
    for k,v in top_msa_counties.items():
        for i in v:
            while len(i)!=5:   #fix for fips in a format other than SSCCC
                i = "0" + i
            population = df[df["FIPS"]==i][col_name].sum(axis=1)
            try:   #for missing FIPS
                cntr[k] += int(population)
            except:
                print("Missing FIPS detected:",i,population)
    return cntr


with SAS7BDAT(data_file_1970) as reader:
    df = reader.to_data_frame()
df['FIPS'] = df['st70'].astype(int).apply(lambda x: format_fip_st(x)) + df['cnty'].astype(int).apply(lambda x: format_fip_cnt(x))
cntr_1970 = Counter()
for k,v in top_msa_counties.items():
    for i in v:
        while len(i)!=5:
            i = "0" + i
        population = df[df["FIPS"]==i][age_cols.keys()].sum(axis=1)
        try:
            cntr_1970[i] =int(population)
        except:
            print(i,population)


with SAS7BDAT(data_file_1980) as reader:
    df = reader.to_data_frame()
df["FIPS"] = df["fipsstate"] + df["COUNTY"]
cntr_1980 = Counter()
for k,v in top_msa_counties.items():
    for i in v:
        while len(i)!=5:
            i = "0" + i
        population = df[df["FIPS"]==i]["t3_1"]
        try:
            cntr_1980[i] = int(population)
        except:
            print(i,population)


with SAS7BDAT(data_file_1990) as reader:
    df = reader.to_data_frame()
df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: format_fip_st(x)) + df['CNTY'].astype(int).apply(lambda x: format_fip_cnt(x))
cntr_1990 = Counter()
for k,v in top_msa_counties.items():
    for i in v:
        while len(i)!=5:
            i = "0" + i
        population = df[df["FIPS"]==i]["P1_1"]
        try:
            cntr_1990[i] = int(population)
        except:
            print(i,population)

with SAS7BDAT(data_file_2000) as reader:
    df = reader.to_data_frame()
df["FIPS"] = df["STATE"] + df["COUNTY"]
cntr_2000 = Counter()
for k,v in top_msa_counties.items():
    for i in v:
        while len(i)!=5:
            i = "0" + i
        population = df[df["FIPS"]==i]["P1_1"]
        try:
            cntr_2000[i] = int(population)
        except:
            print(i,population)

result = []
for k,v in top_msa_counties.items():
    for i in v:
        result.append([top_msas_dict[k],k,i,cntr_1970[i],cntr_1980[i],cntr_1990[i],cntr_2000[i]])

res = pd.DataFrame(result, columns=["MSA","MSA_code","FIPS","1970","1980","1990","2000"])
res.to_csv(output_file,index=False)
