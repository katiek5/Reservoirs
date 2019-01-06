# Title: Fluoroprobe Heatmaps
# Author: Ryan McClure & Mary Lofton
# Date last updated: 08AUG18
# Description: Makes heatmaps of fluoroprobe data

#Note: currently this script plots DOY on the x-axis and so can only plot 1 year at a time

rm(list=ls())

########WHAT RESERVOIR ARE YOU WORKING WITH?########
Reservoir = "CCR" #choose from FCR, BVR, CCR
####################################################

########WHAT YEAR WOULD YOU LIKE TO PLOT?###########
plot_year = 2018 #choose from 2014-2018
####################################################

# load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, akima, reshape2, 
               gridExtra, grid, colorRamps,RColorBrewer, rLakeAnalyzer, cowplot)


# Load .txt files for appropriate reservoir 

#NOTE: this script is not currently set up to handle upstream sites in FCR
col_names <- names(read_tsv("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt/20180410_FCR_50.txt", n_max = 0))

raw_fp <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", pattern = paste0("*_",Reservoir,"_50.txt")) %>% 
  map_df(~ read_tsv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/FP_txt", .), col_types = cols(.default = "c"), col_names = col_names, skip = 2))

fp <- raw_fp %>%
  mutate(DateTime = `Date/Time`, GreenAlgae_ugL = as.numeric(`Green Algae`), Bluegreens_ugL = as.numeric(`Bluegreen`),
         Browns_ugL = as.numeric(`Diatoms`), Mixed_ugL = as.numeric(`Cryptophyta`), YellowSubstances_ugL = as.numeric(`Yellow substances`),
         TotalConc_ugL = as.numeric(`Total conc.`), Transmission_perc = as.numeric(`Transmission`), Depth_m = `Depth`) %>%
  select(DateTime, GreenAlgae_ugL, Bluegreens_ugL, Browns_ugL, Mixed_ugL, YellowSubstances_ugL,
         TotalConc_ugL, Transmission_perc, Depth_m) %>%
  mutate(DateTime = as.POSIXct(as_datetime(DateTime, tz = "", format = "%m/%d/%Y %I:%M:%S %p"))) %>%
  filter(year(DateTime) == plot_year)%>%
  mutate(Date = date(DateTime), DOY = yday(DateTime))

# filter out depths in the fp cast that are closest to specified values.

if (Reservoir == "FCR"){
  
  depths = seq(0.1, 9.7, by = 0.3)
  df.final<-data.frame()
  
  for (i in 1:length(depths)){
    
fp_layer<-fp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[i])))

# Bind each of the data layers together.
df.final = bind_rows(df.final, fp_layer)

}


} else if (Reservoir == "BVR"){
  
  depths = seq(0.1, 10.3, by = 0.3)
  df.final<-data.frame()
  
  for (i in 1:length(depths)){
    
    fp_layer<-fp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
    
    # Bind each of the data layers together.
    df.final = bind_rows(df.final, fp_layer)
    
  }
  
} else if(Reservoir == "CCR"){
  
  depths = seq(0.1, 19.9, by = 0.3)
  df.final<-data.frame()
  
  for (i in 1:length(depths)){
    
    fp_layer<-fp %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
    
    # Bind each of the data layers together.
    df.final = bind_rows(df.final, fp_layer)
    
  }
  
}

# Re-arrange the data frame by date
fp_new <- arrange(df.final, Date)

# Round each extracted depth to the nearest 10th. 
fp_new$Depth_m <- round(as.numeric(fp_new$Depth_m), digits = 0.5)

# Select and make each fp variable a separate dataframe
# I have done this for the heatmap plotting purposes. 
green <- select(fp_new, DateTime, Depth_m, GreenAlgae_ugL, Date, DOY)
bluegreen <- select(fp_new, DateTime, Depth_m, Bluegreens_ugL, Date, DOY)
brown <- select(fp_new, DateTime, Depth_m, Browns_ugL, Date, DOY)
mixed <- select(fp_new, DateTime, Depth_m, Mixed_ugL, Date, DOY)
yellow <- select(fp_new, DateTime, Depth_m, YellowSubstances_ugL, Date, DOY)
total <- select(fp_new, DateTime, Depth_m, TotalConc_ugL, Date, DOY)
trans <- select(fp_new, DateTime, Depth_m, Transmission_perc, Date, DOY)


# Complete data interpolation for the heatmaps
# interative processes here

#green algae
##NOTE: the interp function WILL NOT WORK if your vectors are not numeric or have NAs or Infs
interp_green <- interp(x=green$DOY, y = green$Depth_m, z = green$GreenAlgae_ugL,
                      xo = seq(min(green$DOY), max(green$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = T, linear = T, duplicate = "strip")
interp_green <- interp2xyz(interp_green, data.frame=T)

#Bluegreen algae
interp_bluegreen <- interp(x=bluegreen$DOY, y = bluegreen$Depth_m, z = bluegreen$Bluegreens_ugL,
                      xo = seq(min(bluegreen$DOY), max(bluegreen$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_bluegreen <- interp2xyz(interp_bluegreen, data.frame=T)

#Browns
interp_brown <- interp(x=brown$DOY, y = brown$Depth_m, z = brown$Browns_ugL,
                      xo = seq(min(brown$DOY), max(brown$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_brown <- interp2xyz(interp_brown, data.frame=T)

#Mixed
interp_mixed <- interp(x=mixed$DOY, y = mixed$Depth_m, z = mixed$Mixed_ugL,
                      xo = seq(min(mixed$DOY), max(mixed$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_mixed <- interp2xyz(interp_mixed, data.frame=T)

#Yellow substances
interp_yellow <- interp(x=yellow$DOY, y = yellow$Depth_m, z = yellow$YellowSubstances_ugL,
                      xo = seq(min(yellow$DOY), max(yellow$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_yellow <- interp2xyz(interp_yellow, data.frame=T)

#Total conc.
interp_total <- interp(x=total$DOY, y = total$Depth_m, z = total$TotalConc_ugL,
                      xo = seq(min(total$DOY), max(total$DOY), by = .1), 
                      yo = seq(0.1, 19.9, by = 0.01),
                      extrap = F, linear = T, duplicate = "strip")
interp_total <- interp2xyz(interp_total, data.frame=T)

#Transmission
interp_trans <- interp(x=trans$DOY, y = trans$Depth_m, z = trans$Transmission_perc,
                       xo = seq(min(trans$DOY), max(trans$DOY), by = .1), 
                       yo = seq(0.1, 19.9, by = 0.01),
                       extrap = F, linear = T, duplicate = "strip")
interp_trans <- interp2xyz(interp_trans, data.frame=T)

# Plotting #

# Create a pdf so the plots can all be saved in one giant bin!

#Green Algae
p1 <- ggplot(interp_green, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir, " Green Algae Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()

#Bluegreens
p2 <- ggplot(interp_bluegreen, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir, " Cyanobacteria Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p2

#Browns
p3 <- ggplot(interp_brown, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir, " Brown Algae Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p3

#Mixed
p4 <- ggplot(interp_mixed, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir, " 'MIXED' Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p4

#Yellow substances
p5 <- ggplot(interp_yellow, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir," Yellow Substances Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p5

#Total concentration
p6 <- ggplot(interp_total, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir," Total Phytoplankton Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p6

#Transmission
p7 <- ggplot(interp_trans, aes(x=x, y=y))+
  geom_raster(aes(fill=z))+
  scale_y_reverse(expand = c(0,0))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Day of year", y = "Depth (m)", title = paste0(Reservoir, " Transmission % Heatmap"),fill=expression(paste(mu,g/L)))+
  theme_bw()
#p7

# # create a grid that stacks all the heatmaps together. 
# grid.newpage()
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),
#                 ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6),
#                 ggplotGrob(p7),
#                 size = "first"))
# # end the make-pdf function. 
# dev.off()

final_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7, ncol = 1) # rel_heights values control title margins
ggsave(plot=final_plot, file= paste0("./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/",Reservoir,"_50_FP_2018.pdf"),
       h=30, w=10, units="in", dpi=300,scale = 1)

#multi-year plots
fp_edi <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLFluoroProbe/FluoroProbe.csv")%>%
  filter(Reservoir == "CCR" & Site == "50")

allyears <- fp_edi %>%
  filter(Depth_m <= 10)%>%
  mutate(Year = as.factor(year(DateTime)),
         DOY = yday(DateTime),
         Date = date(DateTime))%>%
  group_by(Date,Year, DOY) %>%
  summarize(Total = mean(TotalConc_ugL, na.rm = TRUE),
            GreenAlgae = mean(GreenAlgae_ugL, na.rm = TRUE),
            BluegreenAlgae = mean(Bluegreens_ugL, na.rm = TRUE),
            BrownAlgae = mean(BrownAlgae_ugL, na.rm = TRUE),
            MixedAlgae = mean(MixedAlgae_ugL, na.rm = TRUE)) %>%
  gather(Total:MixedAlgae, key = "spectral_group", value = "ugL")


plot_all <- ggplot(data = subset(allyears, Year == 2018 & spectral_group != "Total" & spectral_group != "MixedAlgae"), aes(x = DOY, y = ugL, group = spectral_group, colour = spectral_group))+
  geom_line(size = 1)+
  scale_colour_manual(values = c("darkcyan","chocolate1","chartreuse4"))+
  xlab("Day of Year")+
  ylab("micrograms per liter")+
  ggtitle("2018")+
  # ylim(c(0,5))+
  # xlim(c(125,275))+
  theme_bw()
plot_all
ggsave(plot_all, filename = "./Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/CCR_epi_2018.png",
       h = 3, w = 8, units = "in")

