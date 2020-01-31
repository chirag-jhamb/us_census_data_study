# Author: Chirag Jhamb
# create block group data for  B19013
# parses the file in Tracts_Block_Groups_Only folder
# saves file after adding all the columns of required tables and data for DC
# years: 2013 to 2017


# import geography file: state, county, tract, block group, summary level, logrecno

import pandas as pd
import datetime, csv
# for reading shp files:
import shapefile
# import the file having all the data functions, which contains MSA function that gives FIPS of all counties present in DMV(47900) MSA
exec(open("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_programs/2020/data_functions.py").read())

# function to read and return shp files:
def read_shapefile(shp_path):
	"""
	Read a shapefile into a Pandas dataframe with a 'coords' column holding
	the geometry information. This uses the pyshp package
	"""
	import shapefile
	#read file, parse out the records and shapes
	sf = shapefile.Reader(shp_path)
	fields = [x[0] for x in sf.fields][1:]
	records = sf.records()
	shps = [s.points for s in sf.shapes()]
	#write into a dataframe
	df = pd.DataFrame(columns=fields, data=records)
	df = df.assign(coords=shps)
	return df

def documentation_file_data(table_id, table_name,state):  # reads the documentation file, gets the column name and row location of each detail
    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    if int(end_year) < 2013:
        docFilePath = inDir + '/Sequence_Number_and_Table_Number_Lookup.xls'
    else:
        docFilePath = inDir + '/ACS_5yr_Seq_Table_Number_Lookup.xls'
    dataDir = inDir + '/group1'
    # documentation file table IDs, names and row location
    documentation_file = pd.read_excel(docFilePath, header=0, dtype=object, na_filter=False)
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
            # to avoid the decimal 0.5 in some cases
            if float(r["Line Number"]).is_integer():
                if table_id=="B19013":
                     #to get rid of year in column name, causing multiple columns at merge
                    data_location[r["Line Number"]+start_pos-1] = table_id+ "_"+ r["Table Title"].replace(":","").replace(" ","_").split("(")[0]
                else:
                    data_location[r["Line Number"]+start_pos-1] = table_id+ "_" + r["Table Title"].replace(":","").replace(" ","_")
        # to encounter error when empty cell is parsed
        except ValueError:
            pass
    return data_location, seq_number

def get_block_group_data(state):
    inDir = r'/groups/brooksgrp/census/american_community_survey/' + start_year + '_' + end_year + '_5year_estimates/raw_data'
    # folder having geography file:
    path = inDir + '/geog/g' + end_year + '5' + state.lower() + '.txt'
    # get cblock_group for each logrecno
    result = []
    with open(path, 'rb') as f:
        for row in f:
            state_fips = row[25:27].decode('utf-8', 'ignore')
            county = row[27:30].decode('utf-8', 'ignore')
            sumlevel = row[8:11].decode('utf-8', 'ignore')
            logrecno = row[13:20].decode('utf-8', 'ignore')
            tract = row[40:46].decode('utf-8', 'ignore')
            block_group = row[46:47].decode('utf-8', 'ignore')
            result.append([sumlevel, logrecno,state.lower(),block_group, tract, state_fips, county])
    # convert to DF
    file1_result = pd.DataFrame(result, columns = ["sumlevel", "logrecno","state","blockgrp", "tract","statefips", "countyfips"])
    # set index ready for merge:
    file1_result["mkey"] = file1_result["state"] + file1_result["logrecno"]
    file1_result = file1_result.set_index(["mkey"])
    del file1_result["state"]  # not required
    del file1_result["logrecno"]   # was required only for indexing
    print(file1_result.head())
    return file1_result

def tracts_block_groups_data(seq_number, data_location,state):
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

all_state_dfs = []
state_list = ["dc","va","wv","md"]

for state in state_list:
    all_files_per_year = []
    for year in end_years:
        print(year)
        start_year = str(int(year) - 4)
        end_year = year
        Median_Income_df = ('MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN ' + end_year + ' INFLATION-ADJUSTED DOLLARS)', 'B19013')
		# for total population, avoid UNWEIGHTED SAMPLE COUNT OF THE POPULATION - incorrect data

        total_pop_df = ('TOTAL POPULATION', 'B01003')
        block_group_data = get_block_group_data(state)
        print(block_group_data.head())
        tract_data_file_list = []
        for data_index in [Median_Income_df, total_pop_df]:
            table_name = data_index[0]
            table_id = data_index[1]
            print(table_id, table_name)
            # gets the column name and row location of each detail:
            # df_documentation_file,
            data_location, seq_number = documentation_file_data(table_id, table_name, state)
            print(data_location, seq_number)
            # get other data which is required:
            tract_data_file = tracts_block_groups_data(seq_number, data_location, state)
            print(tract_data_file.head())
            tract_data_file_list.append(tract_data_file)
            # merge then two by their index:
        all_levels_file = pd.concat(tract_data_file_list, axis=1, join = 'inner', sort = True)
        print("Returned merged file:")
        print(all_levels_file.head())
        print("Adding block_group_data to merged file:")
        all_levels_file = pd.concat([block_group_data,all_levels_file], axis=1, join = 'inner', sort = True)
        print(all_levels_file.head())
        print("\nSummary level 150:")
        # subset file to only contain summary_level 150
        subfile = all_levels_file[all_levels_file["sumlevel"]=='150']
        subfile["start_year"] = start_year
        subfile["end_year"] = end_year
        print(subfile.head())
        print(subfile.shape)
        all_files_per_year.append(subfile)

    print("Final concat", len(all_files_per_year), " files")
    years_merged_df = pd.concat(all_files_per_year, axis=0)
    # convert all to values to constant strings, to keep leading zeros in a column when exported to csv
    for i in years_merged_df.columns:
        # except the values which have numbers needed, i.e., table name columns
        if not i.startswith("B"):
            years_merged_df[i] = years_merged_df[i].astype("str")
        else:
            # replace null values "." with None and convert to int
            years_merged_df[i] = years_merged_df[i].replace(".",None).replace("",None).astype(int)
    # create fips column for subsetting
    years_merged_df["FIPS"] = years_merged_df["statefips"] + years_merged_df["countyfips"]
    # convert to int for Subsetting:
    years_merged_df["FIPS"] = years_merged_df["FIPS"].astype("int")
    # import list of FIPS in DMV MSA:
    top_msa_counties, top_msa_by_state, top_msas_dict = get_top_msa()
    dmv_msa_fips = top_msa_counties[47900]
    # subset data to FIPS in DMV MSA only:
    years_merged_df = years_merged_df[years_merged_df["FIPS"].isin(dmv_msa_fips)]
    del years_merged_df["FIPS"]  # not required anymore
    del years_merged_df["sumlevel"]  # not required after subsetting
    # del years_merged_df["state"]  # not required after subsetting
    print(years_merged_df.head())
    print(years_merged_df.shape)
    all_state_dfs.append(years_merged_df)

print("Creating final file")
final_file = pd.concat(all_state_dfs, axis=0)
print(final_file.head())
print(final_file.shape)

# set unique key for future merges
final_file["primary_key"] = final_file["blockgrp"] + final_file["tract"] + final_file["statefips"] + final_file["countyfips"]
########## appending merge LAND AREA:
# file location:
area_filename = "/groups/brooksgrp/maps/united_states/census2010/block_groups/nhgis0020_shape/US_blck_grp_2010.shp"
print("\nReading the area shape file:")
area_data = read_shapefile(area_filename)
# create FIPS to subset data to required counties only
area_data["FIPS"] = area_data["STATEFP10"] + area_data["COUNTYFP10"]
# subset data to FIPS in DMV MSA only:
area_data = area_data[area_data["FIPS"].isin(dmv_msa_fips)]
area_data["primary_key"] = area_data['BLKGRPCE10'] + area_data['TRACTCE10'] + area_data["STATEFP10"] + area_data["COUNTYFP10"]
area_data["land_area"] = area_data["Shape_area"]
# get rid of columns not required
area_data = area_data[["land_area","primary_key"]]
# merge area with other file
final_file = pd.merge(final_file,area_data, on=["primary_key"])
print(final_file.head())
print(final_file.shape)
del final_file["primary_key"]

# convert all to values to constant strings, to keep leading zeros in a column when exported to csv
for i in years_merged_df.columns:
	# except the values which have numbers needed, i.e., table name columns
	if not i.startswith("B"):
		years_merged_df[i] = years_merged_df[i].astype("str")
save_location = "/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/block_group_data/"
filename = save_location + datetime.datetime.today().strftime('%Y%m%d') + '_block_group_tract_data.csv'
final_file.to_csv(filename, index=False, quoting=csv.QUOTE_NONNUMERIC)
print("File saved",filename)
