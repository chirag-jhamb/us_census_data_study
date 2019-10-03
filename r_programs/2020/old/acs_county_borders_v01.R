#####################################################################

# this program makes the county borders for washington metro area
# january 18, 2019
# acs_county_borders_v01.R 

##############################################################################

##### A. start-up and set-up

library(RColorBrewer)
library(sp)
library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(grid)
library(gtable)
library(data.table)
library(cowplot)

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

groupDir <- "/groups/brooksgrp"

out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/")


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# read the county boundaries data
cborders1960 <- st_read(dsn = paste0(groupDir,"/maps/united_states/census1960/counties/"), layer = "US_county_1960_conflated")


# make a state variable
cborders1960$fipsstate <- substr(cborders1960$NHGISST,1,2)

# make a county variable
cborders1960$fipscounty <- substr(cborders1960$GISJOIN,5,7)


# cborders1960c <- cborders1960[which((cborders1960$fipscounty %in% c("001") & cborders1960$fipsstate %in% c("11"))|
#                                       (cborders1960$fipscounty %in% c("009","017","021","031","033") & cborders1960$fipsstate %in% c("24"))|
#                                       (cborders1960$fipscounty %in% c("037") & cborders1960$fipsstate %in% c("54"))|
#                                       (cborders1960$fipscounty %in% c("013","043","047","059","061","107","153","157","177","179","187",
#                                                                       "510","600","610","630","683","685")
#                                        & cborders1960$fipsstate %in% c("51"))),]
# 
# 
# 
# # draw the plot with county names in it without any demographic data
# p1 <- ggplot(cborders1960c) + 
#   #geom_sf(colour = "white")+
#   geom_sf(fill = "transparent", color = "grey50", size = 0.5)+
#   annotate("text", x = -77.33, y = 38.85, label = "Fairfax County", size=4)+
#   annotate("text", x = -77.2, y = 39.15, label = "Montgomery County", size=4)+
#   annotate("text", x = -77.03, y = 38.93, label = "DC", size=4)+
#   annotate("text", x = -76.88, y = 38.75, label = "Prince George's County", size=4)+
#   annotate("text", x = -77.47, y = 39.01, label = "Falls Church City", size=4)+
#   annotate("text", x = -77.35, y = 38.6, label = "Arlington County", size=4)+
#   annotate("text", x = -77.1, y = 38.525, label = "City of Alexandria", size=4)+
#   #annotate("segment", x = -76.78, xend = -76.75, y = 39.03, yend = 39.14, colour = "black")+
#   annotate("segment", x = -77.18, xend = -77.45, y = 38.89, yend = 39, colour = "grey")+
#   annotate("segment", x = -77.13, xend = -77.35, y = 38.88, yend = 38.61, colour = "grey")+
#   annotate("segment", x = -77.07, xend = -77.12, y = 38.815, yend = 38.532, colour = "grey")+
#   coord_sf(crs = st_crs(4326))+
#   #coord_sf(ndiscr = F)+
#   #coord_sf() +
#   theme_map() +
#   theme(legend.position = "bottom",legend.background = element_rect(color = NA))


#p1



#ggsave(paste0(out_dir,dateo,"_county_borders_washington_metro_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


plot_county_borders <- function(df,sfips,cfips){

df_c <- df[which((df$fipscounty %in% c(cfips) & df$fipsstate %in% c(sfips))),]

#print(head(df_c))

# draw the plot with county names in it without any demographic data
p1 <- ggplot(df_c) + 
  #geom_sf(colour = "white")+
  geom_sf(fill = "transparent", color = "grey50", size = 0.5)+
  coord_sf(crs = st_crs(4326))+
  theme_map() +
  theme(legend.position = "bottom",legend.background = element_rect(color = NA))


print(p1)

ggsave(paste0(out_dir,dateo,"_county_borders_",sfips,"_",cfips,".jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


}

# provide column names for which we want absolute value plot on county level
state_fips <- c("11", "24", "51", "54")

# call the funtion to create plot for each variable
for (sfips in state_fips){
  #print(sfips)
  if(sfips=="11"){
    for(cfips in c("001")){
      plot_county_borders(cborders1960,sfips,cfips)
    }
  } else if (sfips=="51"){
    for(cfips in c("013","043","047","059","061","107","153","157","177","179","187","510","610","630")){
      plot_county_borders(cborders1960,sfips,cfips)
    }
  }else if (sfips=="24"){
    for(cfips in c("009","017","021","031","033")){
      plot_county_borders(cborders1960,sfips,cfips)
    }
  } else{
    for(cfips in c("037")){
      plot_county_borders(cborders1960,sfips,cfips)
    }
  }
}


plot_state_borders <- function(df,sfips,cfips){
  
  df_c <- df[which((df$fipscounty %in% c(cfips) & df$fipsstate %in% c(sfips))),]
  
  #print(head(df_c))
  
  # draw the plot with county names in it without any demographic data
  p1 <- ggplot(df_c) + 
    #geom_sf(colour = "white")+
    geom_sf(fill = "transparent", color = "grey50", size = 0.5)+
    coord_sf(crs = st_crs(4326))+
    theme_map() +
    theme(legend.position = "bottom",legend.background = element_rect(color = NA))
  
  
  print(p1)
  
  ggsave(paste0(out_dir,dateo,"_county_borders_",sfips,".jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))
  
  
}



# provide column names for which we want absolute value plot on county level
state_fips <- c("11", "24", "51", "54")

# call the funtion to create plot for each variable
for (sfips in state_fips){
  #print(sfips)
  if(sfips=="11"){
      cfips <-  c("001")
      plot_state_borders(cborders1960,sfips,cfips)
  } else if (sfips=="51"){
      cfips <- c("013","043","047","059","061","107","153","157","177","179","187","510","610","630")
      plot_state_borders(cborders1960,sfips,cfips)
  }else if (sfips=="24"){
      cfips <- c("009","017","021","031","033")
      plot_state_borders(cborders1960,sfips,cfips)
  } else{
      cfips <- c("037")
      plot_state_borders(cborders1960,sfips,cfips)
  }
}
