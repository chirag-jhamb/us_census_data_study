import bs4, sys, urllib.request, os, requests, zipfile, time

def download(src,dst):
    if not os.path.exists(dst):
        print('downloading %s -> %s'% (src,dst), file=sys.stderr)
        urllib.request.urlretrieve(src,dst)


def download_acs(src,dst,data_url,data_dir):
    download(src,dst)
    if not os.path.exists(data_dir):
        os.makedirs(data_dir)

    soup = bs4.BeautifulSoup(requests.get(data_url).content)

    data_zips = []

    for link in soup.find_all('a'):
        if link.get('href') and link.get('href').endswith('zip'):
            fn = link.get('href').split('/')[-1]
            download(data_url+'/'+fn,os.path.join(data_dir,fn))
            #data_zips.append(zipfile.Zipfile(os.path.join(data_dir,fn),'r'))


# download_acs("https://www2.census.gov/programs-surveys/acs/summary_file/2013/documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.xls",
#              "/groups/brooksgrp/census/american_community_survey/2009_2013_5year_estimates/documentation/",
#              "https://www2.census.gov/programs-surveys/acs/summary_file/2013/data/5_year_by_state",
#              "/groups/brooksgrp/census/american_community_survey/2009_2013_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/")


def rununtilcomplete():
    while True:
        try:
            download_acs(
                "https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.xls",
                "/groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/documentation/",
                "https://www2.census.gov/programs-surveys/acs/summary_file/2016/data/5_year_by_state",
                "/groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/")

        except:
            continue

        time.sleep(121)


rununtilcomplete()

os.system('mv /groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/All_Geographies_Not_Tracts_Block_Groups/*_Tracts_Block_Groups_Only.zip /groups/brooksgrp/census/american_community_survey/2012_2016_5year_estimates/raw_data/Tracts_Block_Groups_Only/')

