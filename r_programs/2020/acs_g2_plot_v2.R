# Plot g2:  Stacked area graph showing population by level
# Author: Chirag Jhamb
################################################################################
# import libraries required:
library(data.table)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape)
library(splitstackshape)
library(sf)
library(stringr)
library(tidyr)
### GATES
gate_intro_g2 = TRUE
gate_intro_g3 = TRUE
gate_ch01_g1 = TRUE
#for proper numerical representation in graphs:
options(scipen=5)
# date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo

df <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20191101_meeting/20191212_dataset_all_years.csv")
dmv_subset <- subset(df, level=="county_level")
# list of CBSAs to plot
sub_cbsa <- c(12060,14460,26420,33100,37980,47900)
# MSA level dataset:
msa_sub <- subset(df,level=="msa_level")
msa_sub <- msa_sub[msa_sub$CBSA %in% sub_cbsa,]
# msa_sub <- msa_sub[c("total_population","NAME", "level")]
dmv_subset$COUNTY <- substr(dmv_subset$FIPS, 3,5)
dmv_subset$STATE <- substr(dmv_subset$FIPS, 0,2)
dmv_subset <- dmv_subset %>% mutate("area_type"=ifelse((COUNTY %in% c("001") & STATE %in% c("11"))|
                                                             (COUNTY %in% c("013","510") & STATE %in% c("51")),"Urban",
                                                           ifelse((COUNTY %in% c("033","031") & STATE %in% c("24"))|
                                                                    (COUNTY %in% c("059","600","610") & STATE %in% c("51")),"Suburban",
                                                                  "Exurban")))

# dmv_subset_graph <- dmv_subset[c("area_type","year","total_population")]

dmv_subset_graph <- dmv_subset %>% group_by(area_type, year) %>% summarise(total_population = sum(total_population))
groupDir <- "/groups/brooksgrp"
out_dir_intro <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/introduction/introduction_")
out_dir_ch01 <- paste0(groupDir,"/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/2020_report/ch01/ch01_")
if (gate_intro_g2){
  p <- ggplot() +
        geom_area(data = dmv_subset_graph,
                  mapping = aes(x = year, y =total_population,fill=area_type))
  save_path <- paste0(out_dir_intro,"g2_",dateo,".jpg")
  ggsave(save_path, plot = p, dpi = 300, width = 16, height = 11, units = c("in"))
}
# compare DC MSA population growth to other areas

if (gate_intro_g3){
  intro_g3 <- ggplot() +geom_line(data = msa_sub[which(msa_sub$CBSA != 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(NAME))) +
            geom_line(data = msa_sub[which(msa_sub$CBSA == 47900),],
    	      mapping = aes(x = year, y = total_population, color = as.factor(NAME)),
	      size = 1.5)
  save_path <- paste0(out_dir_intro,"g3_",dateo,".jpg")
  ggsave(save_path, plot = intro_g3, dpi = 300, width = 16, height = 11, units = c("in"))
}

if (gate_ch01_g1){
  dmv_race_subset_df <- dmv_subset[c("white_alone" ,"AA_alone" ,"hispanic_or_latino","total_population","area_type","year")]
  dmv_race_subset <- dmv_race_subset_df %>% group_by(area_type, year) %>% summarise(total_population = sum(total_population))
  to_add_cols <- c("white_alone" ,"AA_alone" ,"hispanic_or_latino")
  # 1970 data missing, so delete 1970 to avoid NULL values:
  dmv_race_subset <- subset(dmv_race_subset, year!=1970)

  for (k in to_add_cols){
    df_temp <- dmv_race_subset_df %>% group_by(area_type, year) %>% summarise(tempo = sum(!!sym(k)))
    colnames(df_temp)[which(colnames(df_temp) == "tempo")] <- k
    dmv_race_subset <- merge(dmv_race_subset, df_temp, all.x=TRUE)
    print(dim(dmv_race_subset))
    dmv_race_subset[k] <- 100*dmv_race_subset[k]/dmv_race_subset$total_population
    print("\n")
  }

  areas <- c("Urban","Suburban","Exurban")
  for (ar in areas){
    area_race_df <- subset(dmv_race_subset, area_type==ar)
    df <- area_race_df %>%
      select(year, white_alone,AA_alone,hispanic_or_latino) %>%
      gather(key = "variable", value = "value", -year)

    race_g1 <- ggplot(df, aes(x = year, y = value)) +
      geom_line(aes(color = variable)) +
      scale_color_manual(values = c("red", "blue", "black"))+scale_y_continuous(breaks=c(20,40,60,80), labels=c(20,40,60,80),limits=c(0, 90))
    save_path <- paste0(out_dir_ch01,"g1_",ar,"_",dateo,".jpg")
    print(save_path)
    ggsave(save_path, plot =race_g1, dpi = 300, width = 16, height = 11, units = c("in"))
  }
}
