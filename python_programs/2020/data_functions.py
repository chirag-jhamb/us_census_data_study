# Author: Chirag Jhamb, chirag@gwmail.gwu.edu
# file containing functions which are used in multiple files
# to import this file in the code, run: exec(open("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/data_functions.py").read())

import pandas as pd
from collections import defaultdict, Counter
def get_state_code(file_path="/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/fips_state.csv"):
    import csv
    with open(file_path, mode='r') as infile:
        reader = csv.reader(infile)
        state_codes = {rows[0]:rows[1] for rows in reader}
    return state_codes

def fix_fip_code(fip_code, code_length):  # for fixing the format of fips. For state pass code_length=2 (To convert to SS), for county code_length=3 (to convert to CCC) and for SSCCC pass code_length=5
    fip_code = str(fip_code).split(".")[0]    # convert to string and remove any decimals
    while len(fip_code)<code_length:
        fip_code = "0" + fip_code
    return fip_code

def get_fip_codes(geocode_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/all-geocodes-v2016.xlsx", state_code_file = ""):
    if not state_code_file:
        state_codes = get_state_code()
    else:
        state_codes = get_state_code(state_code_file)
    state_df = pd.read_excel(geocode_file, encoding = "ISO-8859-1", header = 4)
    states = state_df[state_df['Summary Level']==50]
    states["State Code (FIPS)"] = states["State Code (FIPS)"].astype(int).apply(lambda x: fix_fip_code(x,2))
    states["FIP"] = states["State Code (FIPS)"] + states["County Code (FIPS)"].astype(int).apply(lambda x: fix_fip_code(x,3))
    states["name"] = states['Area Name (including legal/statistical area description)'] + ", " + states["State Code (FIPS)"].map(state_codes)
    state_dict = states.set_index('FIP')['name'].to_dict()
    return state_dict

msa_file_import = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/msa_definitions/csa-est2018-alldata.csv"
def get_top_msa(msa_file = msa_file_import, top_n = 10):    # returns the counties in top_n(10 by default) MSAs by population in a dictionary format {MSA_code: list of FIPS of counties in the MSA}
    msa_dataframe = pd.read_csv(msa_file, encoding = "ISO-8859-1")
    msa_cols = ["CSA","CBSA","NAME","LSAD","CENSUS2010POP"]
    all_msas = msa_dataframe[msa_dataframe["LSAD"]=="Metropolitan Statistical Area"][msa_cols]   #select all Metropolitan Statistical Areas
    top_msas = all_msas.sort_values('CENSUS2010POP',ascending = False).head(top_n)     # select top_n MSAs by population
    top_msas_dict = {i:j for i,j in zip(list(top_msas["CBSA"]),list(top_msas["NAME"]))}    # create a dictionary of names of MSAs w.r.t. their code (CBSA)
    top_msas = list(top_msas["CBSA"])
    top_msa_county_data = msa_dataframe.loc[(msa_dataframe["CBSA"].isin(top_msas)) & (msa_dataframe["LSAD"]=="County or equivalent")]   # select all counties which are in the top_n MSAs, get rid of all other types (MSAs, CBSAs etc)
    top_msa_counties = {}
    top_msa_by_state = defaultdict(dict)
    for i in top_msas:    #loop over the codes of top_n MSAs and create a dictionary having list of the FIPs of counties in each MSA
        top_msa_counties[i] = [fix_fip_code(i, 5) for i in list(top_msa_county_data.loc[top_msa_county_data["CBSA"]==i]["STCOU"])]
        temp = defaultdict(list)
        for j in top_msa_counties[i]:
            temp[j[:2]].append(j[2:])
        top_msa_by_state[i] = temp
    top_msa_counties[33100].append('12025')  #  YEAR 1997: Dade county (FIPS 12025) is renamed as Miami-Dade county (FIPS 12086)
    print("Got top 10 MSAs!")
    print(top_msa_counties[33100])
    return top_msa_counties, top_msa_by_state, top_msas_dict
# top_msa_counties, top_msa_by_state, top_msas_dict = get_top_msa()
# set([states(str(j[:3])) for j in top_msa_counties[i]])


def get_county_names(geocode_file = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/data_definitions/all-geocodes-v2016.xlsx"):
    county_df = pd.read_excel(geocode_file, encoding = "ISO-8859-1", header = 4)
    county_df = county_df[county_df['Summary Level']==50]
    county_df["State Code (FIPS)"] = county_df["State Code (FIPS)"].astype(int).apply(lambda x: fix_fip_code(x,2))
    county_df["FIP"] = county_df["State Code (FIPS)"] + county_df["County Code (FIPS)"].astype(int).apply(lambda x: fix_fip_code(x,3))
    county_df["name"] = county_df['Area Name (including legal/statistical area description)']
    county_dict = county_df.set_index('FIP')['name'].to_dict()
    return county_dict
