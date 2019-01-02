#Title: Script for aggregating FP txt files for publication to EDI
#Author: Mary Lofton
#Date: 13DEC18

rm(list=ls())

# load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Load in column names for .txt files 
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt/20180410_FCR_50.txt", n_max = 0))

# Load in txt files
## NEED TO FIGURE OUT HOW TO ADD FILENAME AS A COLUMN SO CAN SPLIT UP BY CAST
fp_casts <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", pattern = paste0("*.txt")) %>%
  map_df(~ data_frame(x = .x), .id = "cast")

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", pattern = paste0("*.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", .), 
                    col_types = cols(.default = "c"), col_names = col_names, skip = 2), .id = "cast")

fp2 <- left_join(raw_fp, fp_casts, by = c("cast")) %>%
  rowwise() %>% 
  mutate(Reservoir = unlist(strsplit(x, split='_', fixed=TRUE))[2],
         Site = unlist(strsplit(x, split='_', fixed=TRUE))[3],
         Site = unlist(strsplit(Site, split='.', fixed=TRUE))[1],
         Reservoir = ifelse(x == "20180730_FCR50.txt","FCR",Reservoir),
         Site = ifelse(x == "20180730_FCR50.txt","50",Site)) %>%
  ungroup()
fp2$Site <- as.numeric(fp2$Site)

check <- subset(fp2, is.na(fp2$Site))
unique(fp2$Reservoir)
unique(fp2$Site)
  
# Rename and select useful columns; drop metrics we don't like such as cell count;
# eliminate shallow depths because of quenching
fp3 <- fp2 %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae`), Bluegreens_ugL = as.numeric(`Bluegreen`),
         BrownAlgae_ugL = as.numeric(`Diatoms`), MixedAlgae_ugL = as.numeric(`Cryptophyta`), YellowSubstances_ugL = as.numeric(`Yellow substances`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission_perc = as.numeric(`Transmission`), Depth_m = as.numeric(`Depth`), Temp_C = as.numeric(`Temp. Sample`),
         RFU_525nm = as.numeric(`LED 3 [525 nm]`), RFU_570nm = as.numeric(`LED 4 [570 nm]`), RFU_610nm = as.numeric(`LED 5 [610 nm]`),
         RFU_370nm = as.numeric(`LED 6 [370 nm]`), RFU_590nm = as.numeric(`LED 7 [590 nm]`), RFU_470nm = as.numeric(`LED 8 [470 nm]`),
         Pressure_unit = as.numeric(`Pressure`)) %>%
  select(x,cast, Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission_perc, Depth_m, Temp_C, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(Depth_m >= 0.2) 

#eliminate upcasts 
fp_downcasts <- fp3[0,]
upcasts <- c("20160904_GWR_50_c.txt","20180907_BVR_50.txt")

for (i in 1:length(unique(fp3$cast))){
  
  if(unique(fp3$cast)[i] %in% upcasts){}else{
  profile = subset(fp3, cast == unique(fp3$cast)[i])
  
  bottom <- max(profile$Depth_m)
  
  idx <- which( profile$Depth_m == bottom ) 
  if( length( idx ) > 0L ) profile <- profile[ seq_len( idx ) , ]
  
  fp_downcasts <- bind_rows(fp_downcasts, profile)
  }
  
}

# #look at casts
# for (i in 1:length(unique(fp_downcasts$cast))){
#   profile = subset(fp_downcasts, cast == unique(fp_downcasts$cast)[i])
#   castname = profile$x[1]
#   profile2 = profile %>%
#     select(Depth_m, GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, TotalConc_ugL)%>%
#     gather(GreenAlgae_ugL:TotalConc_ugL, key = spectral_group, value = ugL)
#   profile_plot <- ggplot(data = profile2, aes(x = ugL, y = Depth_m, group = spectral_group, colour = spectral_group))+
#     geom_path(size = 1)+
#     scale_y_reverse()+
#     ggtitle(castname)+
#     theme_bw()
#   filename = paste0("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/check_plots/",castname,".png")
#   ggsave(filename = filename, plot = profile_plot, device = "png")
#   
# }


  


# Add in 2017 data (different format)
fp2017 <- read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_recal_2017.txt") %>%
  mutate(DateTime = datetime, GreenAlgae_ugL = as.numeric(green_ugL), Bluegreens_ugL = as.numeric(cyano_ugL),
         BrownAlgae_ugL = as.numeric(diatom_ugL), MixedAlgae_ugL = as.numeric(crypto_ugL), YellowSubstances_ugL = as.numeric(yellow_sub_ugL),
         TotalConc_ugL = as.numeric(total_conc_ugL), Transmission_perc = as.numeric(`transmission_%`), Depth_m = depth_m, Temp_C = as.numeric(temp_sample_C),
         RFU_525nm = as.numeric(LED3_525_nm), RFU_570nm = as.numeric(LED4_570_nm), RFU_610nm = as.numeric(LED5_610_nm),
         RFU_370nm = as.numeric(LED6_370_nm), RFU_590nm = as.numeric(LED7_590_nm), RFU_470nm = as.numeric(LED8_470_nm),
         Pressure_unit = as.numeric(pressure_bar),
         Site = ifelse(Reservoir == "BVR",50,Site)) %>%
  select(Reservoir, Site, DateTime, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, MixedAlgae_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission_perc, Depth_m, Temp_C, RFU_525nm, RFU_570nm, RFU_610nm,
         RFU_370nm, RFU_590nm, RFU_470nm, filename) %>%
  filter(Depth_m >= 0.2)  # cut-off for quenching

fp2017$DateTime <- force_tz(fp2017$DateTime, tzone = "America/New_York")

fp2017$DateTime[1]
fp2017$DateTime[20362]

#isolate by cast
fp17v2 <- fp2017 %>%
  mutate(Date = date(DateTime),
         Hour = hour(DateTime)) 

casts <- fp17v2 %>%
  select(Reservoir, Site, Date, Hour) %>%
  distinct() %>%
  mutate(cast = c(1:224))

fp17v3 <- left_join(fp17v2, casts, by = c("Reservoir","Site","Date","Hour"))

#eliminate upcasts 
fp2017_downcasts <- fp17v3[0,]

for (i in 1:length(unique(fp17v3$cast))){
  profile = subset(fp17v3, cast == unique(fp17v3$cast)[i])
  
  bottom <- max(profile$Depth_m)
  
  idx <- which( profile$Depth_m == bottom ) 
  if( length( idx ) > 0L ) profile <- profile[ seq_len( idx ) , ]
  
  fp2017_downcasts <- bind_rows(fp2017_downcasts, profile)
  
}

# #check wonky casts
# wonky1 <- fp2017_downcasts %>%
#   filter(Reservoir == "FCR" & Site == 50 & Date == "2017-05-29")
# #this is weird...repeated values of date and/or depth but different values of phytos
# #may just need to trash this day
# 
# wonky2 <- fp2017_downcasts %>%
#   filter(Reservoir == "FCR" & Site == 50 & Date == "2017-07-05")
# #same as wonky1
# 
# wonky3 <- fp2017_downcasts %>%
#   filter(Reservoir == "FCR" & Site == 50 & Date == "2017-08-21")
# 
# #sadly think I'll just need to eliminate all three of these

#eliminate wonky casts
fp2017_final <- fp2017_downcasts
# fp2017_final <- fp2017_downcasts %>%
#   filter(cast != 38 & cast != 80 & cast != 161)

# #look at casts
# for (i in 1:length(unique(fp2017_downcasts$cast))){
#   profile = subset(fp2017_downcasts, cast == unique(fp2017_downcasts$cast)[i])
#   castname = paste(profile$Reservoir[1],profile$Site[1],profile$Date[1],profile$Hour[1],sep = "_")
#   profile2 = profile %>%
#     select(Depth_m, GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, TotalConc_ugL)%>%
#     gather(GreenAlgae_ugL:TotalConc_ugL, key = spectral_group, value = ugL)
#   profile_plot <- ggplot(data = profile2, aes(x = ugL, y = Depth_m, group = spectral_group, colour = spectral_group))+
#     geom_path(size = 1)+
#     scale_y_reverse()+
#     ggtitle(castname)+
#     theme_bw()
#   filename = paste0("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/check_plots/",castname,".png")
#   ggsave(filename = filename, plot = profile_plot, device = "png")
# 
# }

#merge two datasets
colnames(fp_downcasts)
colnames(fp2017_final)

fp_merge <- fp_downcasts %>%
  select(-x, -cast) %>%
  mutate(Site = as.numeric(Site))

fp2017_merge <- fp2017_final %>%
  select(-filename, -Date, -Hour, -cast)

fp_final <- bind_rows(fp_merge, fp2017_merge) %>%
  arrange(Reservoir, Site, DateTime)

attr(fp_final$DateTime, "tzone") <- "America/New_York"
head(fp_final$DateTime)
tail(fp_final$DateTime)

fp_final <- fp_final[,c(1,2,3,11,4,5,6,7,8,9,10,12,13,14,15,16,17,18)]
colnames(fp_final)[11] <- "Transmission"
colnames(fp_final)[12] <- "Temp_degC"

fp <- fp_final %>%
  mutate(Flag_GreenAlgae = ifelse(year(DateTime) == 2017,1,0),
         Flag_BluereenAlgae = ifelse(year(DateTime) == 2017,1,0),
         Flag_BrownAlgae = ifelse(year(DateTime) == 2017,1,0),
         Flag_MixedAlgae = ifelse(year(DateTime) == 2017,1,0),
         Flag_TotalConc = ifelse(year(DateTime) == 2017,1,0),
         Flag_Temp = ifelse(year(DateTime) == 2017 | year(DateTime) == 2018,2,0),
         Temp_degC = ifelse(year(DateTime) == 2017 | year(DateTime) == 2018,NA,Temp_degC))

fp$Flag_Transmission <- 0
fp$Flag_525nm <- 0
fp$Flag_570nm <- 0
fp$Flag_610nm <- 0
fp$Flag_370nm <- 0
fp$Flag_590nm <- 0
fp$Flag_470nm <- 0
colnames(fp)[20] <- "Flag_BluegreenAlgae"

write.csv(fp, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/FluoroProbe.csv", row.names = FALSE)
fp <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/FluoroProbe.csv")

##look at temperature data

#all but 2017
for (i in 1:length(unique(fp_downcasts$cast))){
  profile = subset(fp_downcasts, cast == unique(fp_downcasts$cast)[i])
  castname = profile$x[1]
  profile_plot <- ggplot(data = profile, aes(x = Temp_C, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/check_plots_temp/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")

}

#2017
for (i in 1:length(unique(fp2017_downcasts$cast))){
  profile = subset(fp2017_downcasts, cast == unique(fp2017_downcasts$cast)[i])
  castname = paste(profile$Reservoir[1],profile$Site[1],profile$Date[1],profile$Hour[1],sep = "_")
  profile_plot <- ggplot(data = profile, aes(x = Temp_C, y = Depth_m))+
    geom_path(size = 1)+
    scale_y_reverse()+
    ggtitle(castname)+
    theme_bw()
  filename = paste0("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/check_plots_temp/",castname,".png")
  ggsave(filename = filename, plot = profile_plot, device = "png")

}

#take away is the sensor needs recalibrated and we should back-correct the temp. data 
#before publishing



  