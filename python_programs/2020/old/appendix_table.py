# Author: Chirag Jhamb, chirag@gwmail.gwu.edu
# this program reads all the sas7bdat files and calculates the total population for the year intervals for each MSA as well as all counties of DMV and outputs it in a csv
from sas7bdat import SAS7BDAT
import pandas as pd
from collections import Counter, defaultdict
import datetime, csv
from os import listdir
from os.path import isfile, join

exec(open("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/data_functions.py").read())

top_msa_counties, top_msa_by_state, top_msas_dict = get_top_msa()

county_names = get_county_names()

dateo = datetime.datetime.today().strftime('%Y%m%d')

path_to_acs_files = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/summary_files_msa/"
acs_files = [path_to_acs_files + f for f in listdir(path_to_acs_files) if isfile(join(path_to_acs_files, f))]


rdata = []
data = []

total_population_msa = defaultdict(dict)
for fl in [f for f in acs_files if "20191029" in f]:
    flname = fl.split("/")[-1]
    yr = flname.split("_")[3]
    msacode = float(flname.split("_")[4])

    df = pd.read_csv(fl)
    age_rows_filter = [i for i in df["index"] if "B01001" in i]
    df2 = df[df["index"].isin(age_rows_filter)]
    df2["msa_population"] = df2.iloc[:, 1:-1].apply(pd.to_numeric).sum(axis=1)
    total_population_msa[yr][msacode] = int(df2[df2["index"]=="B01001_1_total"]["msa_population"])

data = {}
total_population_dmv = defaultdict(dict)
for fl in [f for f in acs_files if ("20191029" in f and "47900" in f)]:
    flname = fl.split("/")[-1]
    yr = flname.split("_")[3]
    with open(fl) as fin:
        reader=csv.reader(fin, skipinitialspace=True)
        for row in reader:
            data[row[0]]=row[1:]
        for i,j in zip(data["index"], data["B03002_1_total"]):
            if i!="estimate_year":
                total_population_dmv[yr][i] = int(j)


print("\n",dateo," running appendix table")
#here are all the file paths that are used in the program:
data_file_1970 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1970_20190915.sas7bdat"
data_file_1980 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1980_20190915.sas7bdat"
data_file_1990 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1990_20190915.sas7bdat"
data_file_2000 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc2000_20190915.sas7bdat"
output_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/population_change_1970_2000_"+dateo+".csv"
#MSA file having all MSAs by population and each county
msa_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"

def fix_fip_code(fip_code, code_length):  # for fixing the format of fips. For state pass code_length=2 (To convert to SS), for county code_length=3 (to convert to CCC) and for SSCCC pass code_length=5
    fip_code = str(fip_code).split(".")[0]    # convert to string and remove any decimals
    while len(fip_code)<code_length:
        fip_code = "0" + fip_code
    return fip_code


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

def get_population_county_level(df, col_name, msa_code = 47900):  #takes dataframe and a column or a list of columns and code of MSA, iterates through all counties in the msa and returns population in ther county. By default claculates for dmv MSA
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
    result.append([top_msas_dict[i],i,msa_population_1970[i],msa_population_1980[i],msa_population_1990[i],msa_population_2000[i],total_population_msa['2009'][i],total_population_msa['2013'][i], "MSA"])
msa_code = 47900  #DMV area
for i in top_msa_counties[msa_code]:
    if i[:2]=="24":
        county_name = county_names[i] + ", MD"
    elif i[:2]=="51":
        county_name = county_names[i] + ", VA"
    else:
        county_name = county_names[i]
    result.append([county_name,i,dmv_population_1970[i],dmv_population_1980[i],dmv_population_1990[i],dmv_population_2000[i],total_population_dmv['2009'][i],total_population_dmv['2013'][i], "county"])

def calculate_increase(x,yr1,yr2):
    try:
        return (x[yr2]/x[yr1])-1
    except:
        return "MISSING"

res = pd.DataFrame(result, columns=["Name","FIPS","1970","1980","1990","2000","2009","2013" ,"type"])
years = ["1970","1980","1990","2000","2009","2013"]
for i in range(len(years)-1):
    print(years[i]+"-"+years[i+1])
    res[years[i]+"-"+years[i+1]] = res.apply(lambda x: calculate_increase(x, years[i], years[i+1]) ,axis=1)

print("Writing results to:",output_file)
res.to_csv(output_file, index=False)
