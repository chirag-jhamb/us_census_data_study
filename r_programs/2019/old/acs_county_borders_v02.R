#####################################################################

# this program makes the county borders for washington metro area
# january 28, 2019
# acs_county_borders_v02.R 

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

out_dir <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2019/introduction/")


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
cborders1960 <- st_read(dsn = paste0(groupDir,"/maps/united_states/census2010/counties/"), layer = "cnty_2010_20140313")


# make a state variable
# cborders1960$STATE <- substr(cborders1960$NHGISST,1,2)
# 
# # make a county variable
# cborders1960$fipscounty <- substr(cborders1960$GISJOIN,5,7)


# cborders1960c <- cborders1960[which((cborders1960$COUNTY %in% c("001") & cborders1960$STATE %in% c("11"))|
#                                       (cborders1960$COUNTY %in% c("009","017","021","031","033") & cborders1960$STATE %in% c("24"))|
#                                       (cborders1960$COUNTY %in% c("037") & cborders1960$STATE %in% c("54"))|
#                                       (cborders1960$COUNTY %in% c("013","043","047","059","061","107","153","157","177","179","187",
#                                                                       "510","600","610","630","683","685")
#                                        & cborders1960$STATE %in% c("51"))),]



cborders1960_urban <- cborders1960 %>% filter((COUNTY=="001" & STATE=="11")|(COUNTY %in% c("013","510") & STATE=="51"))

cborders1960_suburban <- cborders1960 %>% filter((COUNTY %in% c("031","033") & STATE=="24")|
                                                   (COUNTY %in% c("059","600","610") & STATE=="51"))

cborders1960_exurban <- cborders1960 %>% filter((COUNTY %in% c("009","017","021") & STATE=="24")|
                                                   (COUNTY %in% c("043","047","061","107","153","157","177","179",
                                                                      "187","630","683","685") & STATE=="51")|
                                                   (COUNTY=="037" & STATE=="54"))

get_city_in_name <- function(df){
  ind <- df$LSAD %in% "city"
  df1 <- df
  st_geometry(df1) <- NULL
  df1$NAME <- as.character(df1$NAME)
  df1[ind,"NAME"] <- paste0(df1[ind,"NAME"]," city")
  df1 <- df1 %>% dplyr::select(NAME)
  colnames(df1) <- "NEW_NAME"
  
  df$NAME <- as.character(df$NAME)
  
  df3 <- cbind(as.data.frame(df),df1)
  
  return(df3)
}


cborders1960_urban_new <- get_city_in_name(cborders1960_urban)
cborders1960_suburban_new <- get_city_in_name(cborders1960_suburban)
cborders1960_exurban_new <- get_city_in_name(cborders1960_exurban)





# draw the plot with county names in it without any demographic data
p1 <- ggplot() +
  geom_sf(data = cborders1960_urban_new, fill="#1f78b4", size=0.1)+
  geom_sf(data = cborders1960_suburban_new, fill="#a6cee3",size=0.1)+
#  geom_sf_label(data = cborders1960_urban, aes(label = NAME))+
 # geom_sf_label(data = cborders1960_suburban, aes(label = NAME))+
  geom_sf_text(data = cborders1960_urban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_suburban_new, aes(label = NEW_NAME))+
  # geom_sf(colour = "white")+
  # geom_sf(fill = "transparent", color = "grey50", size = 0.5)+
  # annotate("text", x = -77.33, y = 38.85, label = "Fairfax County", size=4)+
  # annotate("text", x = -77.2, y = 39.15, label = "Montgomery County", size=4)+
  # annotate("text", x = -77.03, y = 38.93, label = "DC", size=4)+
  # annotate("text", x = -76.88, y = 38.75, label = "Prince George's County", size=4)+
  # annotate("text", x = -77.47, y = 39.01, label = "Falls Church City", size=4)+
  # annotate("text", x = -77.35, y = 38.6, label = "Arlington County", size=4)+
  # annotate("text", x = -77.1, y = 38.525, label = "City of Alexandria", size=4)+
  # #annotate("segment", x = -76.78, xend = -76.75, y = 39.03, yend = 39.14, colour = "black")+
  # annotate("segment", x = -77.18, xend = -77.45, y = 38.89, yend = 39, colour = "grey")+
  # annotate("segment", x = -77.13, xend = -77.35, y = 38.88, yend = 38.61, colour = "grey")+
  # annotate("segment", x = -77.07, xend = -77.12, y = 38.815, yend = 38.532, colour = "grey")+
  coord_sf(crs = st_crs(4326))+
  #coord_sf(ndiscr = F)+
  #coord_sf() +
  theme_map() +
  theme(legend.position = "bottom",legend.background = element_rect(color = NA))


p1

ggsave(paste0(out_dir,"i1.m2_",dateo,"_county_borders_washington_urban_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))


# draw the plot with county names in it without any demographic data
p1 <- ggplot() +
  geom_sf(data = cborders1960_urban_new, fill="#1f78b4", size=0.1)+
  geom_sf(data = cborders1960_suburban_new, fill="#a6cee3",size=0.1)+
  geom_sf(data = cborders1960_exurban_new,fill="#b2df8a", size=0.1)+
  #geom_sf_label(data = cborders1960_urban, aes(label = NAME))+
  #geom_sf_label(data = cborders1960_suburban, aes(label = NAME))+
  #geom_sf_label(data = cborders1960_exurban, aes(label = NAME))+
  geom_sf_text(data = cborders1960_urban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_suburban_new, aes(label = NEW_NAME))+
  geom_sf_text(data = cborders1960_exurban_new, aes(label = NEW_NAME))+
  #geom_sf(colour = "white")+
  # geom_sf(fill = "transparent", color = "grey50", size = 0.5)+
  # annotate("text", x = -77.33, y = 38.85, label = "Fairfax County", size=4)+
  # annotate("text", x = -77.2, y = 39.15, label = "Montgomery County", size=4)+
  # annotate("text", x = -77.03, y = 38.93, label = "DC", size=4)+
  # annotate("text", x = -76.88, y = 38.75, label = "Prince George's County", size=4)+
  # annotate("text", x = -77.47, y = 39.01, label = "Falls Church City", size=4)+
  # annotate("text", x = -77.35, y = 38.6, label = "Arlington County", size=4)+
  # annotate("text", x = -77.1, y = 38.525, label = "City of Alexandria", size=4)+
  # #annotate("segment", x = -76.78, xend = -76.75, y = 39.03, yend = 39.14, colour = "black")+
  # annotate("segment", x = -77.18, xend = -77.45, y = 38.89, yend = 39, colour = "grey")+
# annotate("segment", x = -77.13, xend = -77.35, y = 38.88, yend = 38.61, colour = "grey")+
# annotate("segment", x = -77.07, xend = -77.12, y = 38.815, yend = 38.532, colour = "grey")+
coord_sf(crs = st_crs(4326))+
  #coord_sf(ndiscr = F)+
  #coord_sf() +
  theme_map() +
  theme(legend.position = "bottom",legend.background = element_rect(color = NA))


p1

ggsave(paste0(out_dir,"i1.m1_",dateo,"_county_borders_washington_metro_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))



# f (requireNamespace("sf", quietly = TRUE)) {
#   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#   ggplot(nc) +
#     geom_sf(aes(fill = AREA))
#   
#   # If not supplied, coord_sf() will take the CRS from the first layer
#   # and automatically transform all other layers to use that CRS. This
#   # ensures that all data will correctly line up
#   nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
#   ggplot() +
#     geom_sf(data = nc) +
#     geom_sf(data = nc_3857, colour = "red", fill = NA)
#   
#   # Unfortunately if you plot other types of feature you'll need to use
#   # show.legend to tell ggplot2 what type of legend to use
#   nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
#   ggplot(nc_3857) +
#     geom_sf(colour = "white") +
#     geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")
#   
#   # You can also use layers with x and y aesthetics: these are
#   # assumed to already be in the common CRS.
#   ggplot(nc) +
#     geom_sf() +
#     annotate("point", x = -80, y = 35, colour = "red", size = 4)
#   
#   # Thanks to the power of sf, a geom_sf nicely handles varying projections
#   # setting the aspect ratio correctly.
#   library(maps)
#   world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
#   ggplot() + geom_sf(data = world1)
#   
#   world2 <- sf::st_transform(
#     world1,
#     "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
#   )
#   ggplot() + geom_sf(data = world2)
#   
#   # To add labels, use geom_sf_label().
#   ggplot(nc_3857[1:3, ]) +
#     geom_sf(aes(fill = AREA)) +
#     geom_sf_label(aes(label = NAME))
# }







# draw the plot with county names in it without any demographic data
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
# 
# 
# p1



#ggsave(paste0(out_dir,dateo,"_county_borders_washington_metro_area.jpg"), plot = p1, dpi = 300, width = 16, height = 11, units = c("in"))