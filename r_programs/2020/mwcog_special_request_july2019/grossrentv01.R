##################################################################
#
# this program makes a picture of median gross rent for dc-area
# counties, 1980 to the present
#
# data are downloaded from nhgis special for this
#
# received speciail request from COG
#
# june 30, 2019
#
# grossrentv01.R
#
##########################################################################


##### A. start up stuff #################################################


##### B. load data #######################################################


d1980 <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/special_request_analysis/rental_rate_data_cog/nhgis0022_csv/nhgis0022_ds104_1980_urb_area.csv")
d1990 <- read.csv("")




stop()
           if state == 'DC':
                data = data.query('COUNTY in ["001"]')
            elif state == 'MD':
                data = data.query('COUNTY in ["009","017","021","031","033"]')
            elif state == 'VA':
                data = data.query('COUNTY in ["510","013","043","047","059","600","610","061",'
                                  '"630", "107","683","685","153","157","177","179","187"]')
            elif state == 'WV':
                data = data.query('COUNTY in ["037"]')
        except:
