import csv
import pandas as pd
from os import listdir
from os.path import isfile, join
from sas7bdat import SAS7BDAT
exec(open("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/data_functions.py").read())
top_msa_counties, top_msa_by_state, top_msas_dict = get_top_msa()
county_fip_codes = get_fip_codes()

age_gate = False
household_gate = False
# age_201_columns = {'less than 18': ['B01001_3_under_5_years', 'B01001_27_under_5_years', 'B01001_4_5_to_9_years', 'B01001_28_5_to_9_years', 'B01001_5_10_to_14_years', 'B01001_29_10_to_14_years', 'B01001_6_15_to_17_years', 'B01001_30_15_to_17_years'], '18 to 29': ['B01001_7_18_and_19_years', 'B01001_31_18_and_19_years', 'B01001_8_20_years', 'B01001_32_20_years', 'B01001_9_21_years', 'B01001_33_21_years', 'B01001_10_22_to_24_years', 'B01001_34_22_to_24_years', 'B01001_11_25_to_29_years', 'B01001_35_25_to_29_years'], '30 to 44': ['B01001_12_30_to_34_years', 'B01001_36_30_to_34_years', 'B01001_13_35_to_39_years', 'B01001_37_35_to_39_years', 'B01001_14_40_to_44_years', 'B01001_38_40_to_44_years'], '45 to 59': ['B01001_15_45_to_49_years', 'B01001_39_45_to_49_years', 'B01001_16_50_to_54_years', 'B01001_40_50_to_54_years', 'B01001_17_55_to_59_years', 'B01001_41_55_to_59_years'], 'above 59': ['B01001_18_60_and_61_years', 'B01001_42_60_and_61_years', 'B01001_19_62_to_64_years', 'B01001_43_62_to_64_years', 'B01001_20_65_and_66_years', 'B01001_44_65_and_66_years', 'B01001_21_67_to_69_years', 'B01001_45_67_to_69_years', 'B01001_22_70_to_74_years', 'B01001_46_70_to_74_years', 'B01001_23_75_to_79_years', 'B01001_47_75_to_79_years', 'B01001_24_80_to_84_years', 'B01001_48_80_to_84_years', 'B01001_25_85_years_and_over', 'B01001_49_85_years_and_over']}
data_file_1970 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1970_20190915.sas7bdat"
data_file_1980 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1980_20190915.sas7bdat"
data_file_1990 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc1990_20190915.sas7bdat"
data_file_2000 = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/sas_output/2020/dec_county_census/dc2000_20190915.sas7bdat"
file_dict = {"1970":data_file_1970, "1980":data_file_1980, "1990":data_file_1990, "2000":data_file_2000}

age_1970_columns = {'less than 18': ['m17_6', 'm17_33', 'm17_7', 'm17_34', 'm17_8', 'm17_35', 'm17_9', 'm17_36', 'm17_10', 'm17_37', 'm17_5', 'm17_32', 'm17_1', 'm17_28', 'm17_4', 'm17_31', 'm17_2', 'm17_29', 'm17_3', 'm17_30'], '18 to 29': ['m17_11', 'm17_38', 'm17_12', 'm17_39', 'm17_13', 'm17_40', 'm17_14', 'm17_41', 'm17_15', 'm17_42', 'm17_16', 'm17_43'], '30 to 44': ['m17_17', 'm17_44', 'm17_18', 'm17_45', 'm17_19', 'm17_46'], '45 to 59': ['m17_20', 'm17_47', 'm17_21', 'm17_48', 'm17_22', 'm17_49'], 'above 59': ['m17_23', 'm17_50', 'm17_24', 'm17_51', 'm17_25', 'm17_52', 'm17_26', 'm17_53', 'm17_27', 'm17_54']}
age_1980_columns = {'less than 18': ['t15_1', 't15_2', 't15_3', 't15_4', 't15_5', 't15_6', 't15_7', 't15_8', 't15_9', 't15_10', 't15_11', 't15_12', 't15_27', 't15_28', 't15_29', 't15_30', 't15_31', 't15_32', 't15_33', 't15_34', 't15_35', 't15_36', 't15_37', 't15_38'], '18 to 29': ['t15_13', 't15_14', 't15_15', 't15_16', 't15_39', 't15_40', 't15_41', 't15_42'], '30 to 44': ['t15_17', 't15_18', 't15_43', 't15_44'], '45 to 59': ['t15_19', 't15_20', 't15_45', 't15_46'], 'above 59': ['t15_21', 't15_22', 't15_23', 't15_24', 't15_25', 't15_26', 't15_47', 't15_48', 't15_49', 't15_50', 't15_51', 't15_52']}
age_1990_columns = {'less than 18': ['P13_1', 'P13_2', 'P13_3', 'P13_4', 'P13_5', 'P13_6', 'P13_7', 'P13_8', 'P13_9', 'P13_10', 'P13_11', 'P13_12'], '18 to 29': ['P13_13', 'P13_14', 'P13_15', 'P13_16', 'P13_17', 'P13_18'], '30 to 44': ['P13_19', 'P13_20', 'P13_21'], '45 to 59': ['P13_22', 'P13_23', 'P13_24'], 'above 59': ['P13_25', 'P13_26', 'P13_27', 'P13_28', 'P13_29', 'P13_30', 'P13_31']}
age_2000_columns = {'less than 18': ['P8_3', 'P8_4', 'P8_5', 'P8_6', 'P8_7', 'P8_8', 'P8_9', 'P8_10', 'P8_11', 'P8_12', 'P8_13', 'P8_14', 'P8_15', 'P8_16', 'P8_17', 'P8_18', 'P8_19', 'P8_20', 'P8_42', 'P8_43', 'P8_44', 'P8_45', 'P8_46', 'P8_47', 'P8_48', 'P8_49', 'P8_50', 'P8_51', 'P8_52', 'P8_53', 'P8_54', 'P8_55', 'P8_56', 'P8_57', 'P8_58', 'P8_59'], '18 to 29': ['P8_21', 'P8_22', 'P8_23', 'P8_24', 'P8_25', 'P8_26', 'P8_60', 'P8_61', 'P8_62', 'P8_63', 'P8_64', 'P8_65'], '30 to 44': ['P8_27', 'P8_28', 'P8_29', 'P8_66', 'P8_67', 'P8_68'], '45 to 59': ['P8_30', 'P8_31', 'P8_32', 'P8_69', 'P8_70', 'P8_71'], 'above 59': ['P8_33', 'P8_34', 'P8_35', 'P8_36', 'P8_37', 'P8_38', 'P8_39', 'P8_72', 'P8_73', 'P8_74', 'P8_75', 'P8_76', 'P8_77', 'P8_78']}
age_col_dict = {"1970":age_1970_columns, "1980":age_1980_columns, "1990":age_1990_columns, "2000":age_2000_columns}
age_sort = {'less than 18': 5, '30 to 44': 3, '45 to 59': 2, '18 to 29': 4, 'above 59': 1}
age_msa_output_list = []   # "age","Population", "FIP","age_sort", "year"
age_dmv_output_list = []

household_1980 = {"Households with kids":['t20_1', "t20_3" , "t20_5"],"Family Households w/o kids":["t20_2","t20_4", "t20_6"],"Non-famiy hosueholds w/o kids":["t20_7"]}
household_1990 = {"Households with kids":['P19_1', "P19_3" , "P19_5"],"Family Households w/o kids":["P19_2","P19_4", "P19_6"],"Non-famiy hosueholds w/o kids":["P19_7"]}
household_2000 = {"Households with kids":['P10_8', "P10_12" , "P10_15"],"Family Households w/o kids":["P10_9","P10_13", "P10_16"],"Non-famiy hosueholds w/o kids":["P10_2", "P10_17"]}
household_col_dict = {"1980":household_1980, "1990":household_1990, "2000":household_2000}
household_sort = {"Households with kids":3,"Family Households w/o kids":1,"Non-famiy hosueholds w/o kids":2}

race_1980 = {'white alone': ['t12_1', 't14_2'], 'AA alone': ['t12_2', 't14_3'], 'Asian alone': ['t12_6', 't12_7', 't12_8', 't12_9', 't12_10', 't12_11', 't12_12', 't12_13', 't12_14', 't12_15'], 'hispanic or latino': ['t14_1'], 'all others': ['t12_3', 't12_4', 't12_5', 't12_17'], 'total': ['t12_1', 't12_2', 't12_3', 't12_4', 't12_5', 't12_6', 't12_7', 't12_8', 't12_9', 't12_10', 't12_11', 't12_12', 't12_13', 't12_14', 't12_15', 't12_16', 't12_17']}
race_1990 = {'white alone': ['P12_11', 'P12_21'], 'AA alone': ['P12_12', 'P12_22'], 'Asian alone': ['P12_14', 'P12_23'], 'hispanic or latino': ['P12_15', 'P12_21', 'P12_22', 'P12_23', 'P12_24', 'P12_25'], 'all others': ['P12_13', 'P12_15'], 'total': ['P12_11', 'P12_12', 'P12_13', 'P12_14', 'P12_15', 'P12_21', 'P12_22', 'P12_23', 'P12_24', 'P12_25']}
race_2000 = {'white alone': ['P6_2'], 'AA alone': ["P6_3"], 'Asian alone': ["P6_2"], 'hispanic or latino': ['P7_10'], 'all others': ["P6_6","P6_7","P6_8"], 'total': ['P7_1']}
race_col_dict = {"1980":race_1980, "1990":race_1990, "2000":race_1990}
race_sort = {'white alone': 1, 'AA alone': 2, 'Asian alone': 3, 'hispanic or latino': 4, 'all others': 5}

def read_sas(file_location):   #given a location, the function reads the file and returns in a dataframe format
    with SAS7BDAT(file_location) as reader:
        df = reader.to_data_frame()
    print("\n finished reading",file_location)
    return df

def get_population_msa(df, col_dict):
    cntr = {}
    totals = Counter()
    for k,v in top_msa_counties.items():
        population_per_type = Counter()
        for col_names in col_dict.items():
            population = df[df["FIPS"].isin(v)][col_names[1]].values.sum()
            population_per_type[col_names[0]] += int(population)
        cntr[k] = population_per_type
        totals[k] += sum(population_per_type.values())
    return cntr,totals

def get_population_county_level(df, col_dict, msa_code = 548):  #takes dataframe and a column or a list of columns and code of MSA, iterates through all counties in the msa and returns population in ther county. By default claculates for dmv MSA
    cntr = {}
    totals = Counter()
    for i in top_msa_counties[msa_code]:
        population_per_type = Counter()
        for col_names in col_dict.items():
            population = df[df["FIPS"]==i][col_names[1]].values.sum()
            population_per_type[col_names[0]] += int(population)
        cntr[i] = dict(population_per_type)
        totals[i] += sum(population_per_type.values())
    return cntr, totals

def get_population_msa_race(df, col_dict):
    cntr = {}
    totals = Counter()
    for k,v in top_msa_counties.items():
        population_per_type = Counter()
        for col_names in col_dict.items():
            if col_names[0]=="total":
                totals[k] = df[df["FIPS"].isin(v)][col_names[1]].values.sum()
            else:
                population = df[df["FIPS"].isin(v)][col_names[1]].values.sum()
                population_per_type[col_names[0]] += int(population)
        cntr[k] = population_per_type
        # totals[k] += sum(population_per_type.values())
    return cntr,totals

def get_population_county_level_race(df, col_dict, msa_code = 548):  #takes dataframe and a column or a list of columns and code of MSA, iterates through all counties in the msa and returns population in ther county. By default claculates for dmv MSA
    cntr = {}
    totals = Counter()
    for i in top_msa_counties[msa_code]:
        population_per_type = Counter()
        for col_names in col_dict.items():
            if col_names[0]=="total":
                totals[i] = df[df["FIPS"]==i][col_names[1]].values.sum()
            else:
                population = df[df["FIPS"]==i][col_names[1]].values.sum()
                population_per_type[col_names[0]] += int(population)
        cntr[i] = dict(population_per_type)

    return cntr, totals


if (age_gate==True):
    age_output = []
    for fl in file_dict.items():
        yr = fl[0]
        df = read_sas(fl[1])
        if yr=="1970":
            df['FIPS'] = df['st70'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['cnty'].astype(int).apply(lambda x: fix_fip_code(x,3))
        elif yr=="1980":
            df["FIPS"] = df["fipsstate"] + df["COUNTY"]
        elif yr=="1990":
            df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['CNTY'].astype(int).apply(lambda x: fix_fip_code(x,3))
        else:
            df["FIPS"] = df["STATE"] + df["COUNTY"]
        age_msa_population, total_msa_poplulation = get_population_msa(df, age_col_dict[yr])
        age_dmv_population, total_dmv_poplulation = get_population_county_level(df, age_col_dict[yr])
        for i in age_msa_population.items():
            for j in i[1].items():
                age_output.append([j[0],j[1],top_msas_dict[i[0]],age_sort[j[0]],yr,total_msa_poplulation[i[0]], "MSA"])

        for i in age_dmv_population.items():
            for j in i[1].items():
                age_output.append([j[0],j[1],county_fip_codes[i[0]],age_sort[j[0]],yr,total_dmv_poplulation[i[0]], "DMV"])

    pd.DataFrame(age_output, columns = ["age","population","fip","age_sort","year","total_poplulation", "type"]).to_csv("~/age_19_data.csv", index=False)

if household_gate==True:
    household_output = []
    for fl in file_dict.items():
        yr = fl[0]
        if yr!="1970":
            df = read_sas(fl[1])
            if yr=="1970":
                df['FIPS'] = df['st70'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['cnty'].astype(int).apply(lambda x: fix_fip_code(x,3))
            elif yr=="1980":
                df["FIPS"] = df["fipsstate"] + df["COUNTY"]
            elif yr=="1990":
                df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['CNTY'].astype(int).apply(lambda x: fix_fip_code(x,3))
            else:
                df["FIPS"] = df["STATE"] + df["COUNTY"]
            household_msa_population, total_msa_poplulation = get_population_msa(df, household_col_dict[yr])
            household_dmv_population, total_dmv_poplulation = get_population_county_level(df, household_col_dict[yr])
            for i in household_msa_population.items():
                for j in i[1].items():
                    household_output.append([j[0],j[1],top_msas_dict[i[0]],yr,total_msa_poplulation[i[0]], "MSA", household_sort[j[0]]])

            for i in household_dmv_population.items():
                for j in i[1].items():
                    household_output.append([j[0],j[1],county_fip_codes[i[0]],yr,total_dmv_poplulation[i[0]], "DMV", household_sort[j[0]]])

    pd.DataFrame(household_output, columns = ["household_type","population","fip","year","total_poplulation", "type","household_sort"]).to_csv("~/household_type_19_data.csv", index=False)


race_output = []
for fl in file_dict.items():
    yr = fl[0]
    if yr!="1970":
        df = read_sas(fl[1])
        if yr=="1970":
            df['FIPS'] = df['st70'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['cnty'].astype(int).apply(lambda x: fix_fip_code(x,3))
        elif yr=="1980":
            df["FIPS"] = df["fipsstate"] + df["COUNTY"]
        elif yr=="1990":
            df['FIPS'] = df['STATEFP'].astype(int).apply(lambda x: fix_fip_code(x,2)) + df['CNTY'].astype(int).apply(lambda x: fix_fip_code(x,3))
        else:
            df["FIPS"] = df["STATE"] + df["COUNTY"]
        race_msa_population, total_msa_poplulation = get_population_msa_race(df, race_col_dict[yr])
        race_dmv_population, total_dmv_poplulation = get_population_county_level_race(df, race_col_dict[yr])
        for i in race_msa_population.items():
            for j in i[1].items():
                race_output.append([j[0],j[1],top_msas_dict[i[0]],yr,total_msa_poplulation[i[0]], "MSA", race_sort[j[0]]])
        for i in race_dmv_population.items():
            for j in i[1].items():
                race_output.append([j[0],j[1],county_fip_codes[i[0]],yr,total_dmv_poplulation[i[0]], "DMV", race_sort[j[0]]])

pd.DataFrame(race_output, columns = ["race","population","fip","year","total_poplulation", "type","race_sort"]).to_csv("~/race_type_19_data.csv", index=False)
