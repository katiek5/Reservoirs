# Title: Prepare FCR inflow data for publication to EDI
# Author: Mary Lofton
# Date 18DEC18

#install.packages('pacman') #installs pacman package, making it easier to load in packages
pacman::p_load(tidyverse, lubridate, magrittr, ggplot2) #installs and loads in necessary packages for script

#plotting theme
mytheme = theme(axis.title = element_text(size = 16),
                axis.text = element_text(size = 16))

##Data from pressure transducer
# Load in files with names starting with FCR_inf_15min, should only be .csv files
inflow_pressure <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", pattern = "FCR_15min_Inf*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Inflow_CSV", .), col_types = cols(.default = "c"), skip = 28))
inflow_pressure = inflow_pressure[,-1] #limits data to necessary columns

##A wee bit o' data wrangling to get column names and formats in good shape
inflow_pressure <- inflow_pressure %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Pressure_psi = `Pressure(psi)`,
         Temp_C = `Temperature(degC)`) %>%
  mutate(Pressure_psi = as.double(Pressure_psi),
         Temp_C = as.double(Temp_C))

##Preliminary visualization of raw pressure data from inflow transducer
plot_inflow <- inflow_pressure %>%
  mutate(Date = date(DateTime))

daily_flow <- group_by(plot_inflow, Date) %>% summarize(daily_pressure_avg = mean(Pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_flow, aes(x = Date, y = daily_pressure_avg))+
  geom_point()+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
rawplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure.png", rawplot, device = "png")

pressure_hist = ggplot(data = daily_flow, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_histogram.png", pressure_hist, device = "png")

pressure_boxplot = ggplot(data = daily_flow, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow pressure (psi)")+
  theme_bw()
pressure_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_inflow_pressure_boxplot.png", pressure_boxplot, device = "png")

##Read in catwalk pressure data
pressure <- read_csv("./Data/DataNotYetUploadedToEDI/WVWA_DO_sondes/FCR_DOsonde_2012to2017.csv")
pressure_a4d <- dir(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", pattern = "FCR_BV*") %>% 
  map_df(~ read_csv(file.path(path = "./Data/DataNotYetUploadedToEDI/Raw_inflow/Barometric_CSV", .), col_types = cols(.default = "c"), skip = 28))
pressure_a4d = pressure_a4d[,-c(1,3)]

##Data wrangling to get columns in correct format and combine data from senvu.net and Aqua4Plus
pressure = pressure %>%
  select(Date, `Barometric Pressure Pressure (PSI)`) %>%
  mutate(DateTime = parse_date_time(Date, 'ymd HMS',tz = "EST")) %>%
  rename(Baro_pressure_psi = `Barometric Pressure Pressure (PSI)`) %>%
  select(-Date) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

pressure_a4d <- pressure_a4d %>%
  rename(Date = `Date/Time`) %>%
  mutate(DateTime = parse_date_time(Date, 'dmy HMS',tz = "EST")) %>%
  select(-Date) %>%
  rename(Baro_pressure_psi = `Pressure(psi)`) %>%
  mutate(Baro_pressure_psi = as.double(Baro_pressure_psi))

baro_pressure <- bind_rows(pressure, pressure_a4d)
baro_pressure = baro_pressure %>%
  filter(!is.na(Baro_pressure_psi)) %>%
  arrange(DateTime) %>%
  mutate(DateTime = parse_date_time(DateTime, 'ymd HMS',tz = "EST"))

baro_pressure <- distinct(baro_pressure)


##Preliminary visualization of raw pressure data from catwalk transducer
plot_catwalk <- baro_pressure %>%
  mutate(Date = date(DateTime))

daily_catwalk <- group_by(plot_catwalk, Date) %>% summarize(daily_pressure_avg = mean(Baro_pressure_psi)) %>% mutate(Year = as.factor(year(Date)))

rawplot = ggplot(data = daily_catwalk)+
  geom_point(aes(x = Date, y = daily_pressure_avg))+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
rawplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_baro_pressure.png", rawplot, device = "png")

pressure_hist = ggplot(data = daily_catwalk, aes(x = daily_pressure_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_histogram.png", pressure_hist, device = "png")

pressure_boxplot = ggplot(data = daily_catwalk, aes(x = Year, y = daily_pressure_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. catwalk pressure (psi)")+
  theme_bw()
pressure_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/raw_catwalk_pressure_boxplot.png", pressure_boxplot, device = "png")

##correction to inflow pressure to down-correct data after 18APR16
#for some reasons the DateTimes are one hour off? I am confused about this but whatever
#just setting the downcorrect cutoff to be an hour off to compensate
inflow_pressure1 <- inflow_pressure %>%
  mutate(Pressure_psi =  ifelse(DateTime >= "2016-04-18 15:15:00 EST", (Pressure_psi - 0.14), Pressure_psi)) %>%
  rename(Pressure_psi_downcorrect = Pressure_psi) %>%
  select(-Temp_C)

downcorrect <- bind_cols(inflow_pressure, inflow_pressure1) %>%
  select(-DateTime1)

##ARGH!! DATETIMES ARE NOT PLAYING NICELY!! cannot figure it out so just wrote to .csv and read in again
inflow_pressure$DateTime <- as.POSIXct(inflow_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")
baro_pressure$DateTime <- as.POSIXct(baro_pressure$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

downcorrect_final <- downcorrect %>%
  select(-Pressure_psi) %>%
  rename(Pressure_psi = Pressure_psi_downcorrect)
downcorrect_final$DateTime <- as.POSIXct(downcorrect_final$DateTime, format = "%Y-%m-%d %I:%M:%S %p")

write.csv(downcorrect_final, "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")
write.csv(baro_pressure, "./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")

##OK - round 2. let's see how the datetimes play together
baro <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")
inflow <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")

#correct datetime wonkiness from 2013-09-04 10:30 AM to 2014-02-05 11:00 AM
inflow$DateTime[24304:39090] = inflow$DateTime[24304:39090] - (6*60+43)

#merge inflow and barometric pressures to do differencing
diff = left_join(baro, inflow, by = "DateTime") %>%
  select(-c(1,4))
diff <- distinct(diff)

#eliminating pressure and temperature data we know is bad
diff <- diff %>%
  mutate(Baro_pressure_psi = ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",NA,Baro_pressure_psi),
         Pressure_psi = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",NA,Pressure_psi),
         Temp_C = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",NA,Temp_C)) %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi)
  
#interpolate missing data - DO NOT DO THIS FOR EDI PUBLICATION FILES!####################
x_2014 <- diff$DateTime[35864:39597]
y_2014 <- diff$Baro_pressure_psi[35864:39597]
inter_2014 = as.data.frame(approx(y_2014, method = "linear", n = 3734))
diff$Baro_pressure_psi[35864:39597] <- inter_2014$y

x_2017 <- diff$DateTime[169318:171456]
y_2017 <- diff$Pressure_psi[169318:171456]
inter_2017 = as.data.frame(approx(y_2017, method = "linear", n = 2139))
diff$Pressure_psi[169318:171456] <- inter_2017$y
#########################################################################################

diff <- diff %>%
  mutate(Pressure_psia = Pressure_psi - Baro_pressure_psi)

#visualizing all pressure types with corrections included

plot_both <- diff %>%
  mutate(Date = date(DateTime)) 

daily_pressure <- group_by(plot_both, Date) %>% 
  summarize(daily_pressure_avg = mean(Pressure_psi),
            daily_baro_pressure_avg = mean(Baro_pressure_psi),
            daily_psia = mean(Pressure_psia)) %>%
  mutate(Year = as.factor(year(Date))) %>%
  gather('daily_pressure_avg','daily_baro_pressure_avg', 'daily_psia',
         key = 'pressure_type',value = 'psi') 

daily_pressure <- daily_pressure %>%
  mutate(pressure_type = ifelse(pressure_type == "daily_pressure_avg","inflow",ifelse(pressure_type == "daily_baro_pressure_avg","barometric","corrected")))

both_pressures = ggplot(data = daily_pressure, aes(x = Date, y = psi, group = pressure_type, colour = pressure_type))+
  geom_point()+
  theme_bw()
both_pressures

ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/all_pressure_types.png", both_pressures, device = "png")

#create vector of corrected pressure to match nomenclature from RPM's script
diff <- diff %>%
  filter(!is.na(Pressure_psia)) %>%
  filter(Pressure_psia >= 0)
flow2 <- diff$Pressure_psia 
  
### CALCULATE THE FLOW RATES AT INFLOW ### #(MEL 2018-07-06)
#################################################################################################
flow3 <- flow2*0.70324961490205 - 0.1603375 + 0.03048  # Response: Height above the weir (m)
#pressure*conversion factor for head in m - distance from tip of transducer to lip of weir + distance from tip of transducer to pressure sensor (eq converted to meters)
flow4 <- (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147) # Flow CFS - MEL: I have not changed this; should be rating curve with area of weir
flow_final <- flow4*0.028316847                                  # Flow CMS - just a conversion factor from cfs to cms
#################################################################################################

#creating columns for EDI
diff$Reservoir <- "FCR" #creates reservoir column to match other data sets
diff$Site <- 100  #creates site column to match other data sets
diff$Flow_cms <- flow_final #creates column for flow

##visualization of inflow
plot_inflow <- diff %>%
  mutate(Date = date(DateTime))

daily_inflow <- group_by(plot_inflow, Date) %>% 
  summarize(daily_flow_avg = mean(Flow_cms, na.rm = TRUE)) %>% 
  mutate(Year = as.factor(year(Date)),
         Month = month(Date))

inflow2 = ggplot(subset(daily_inflow, Year == 2018 & Month == 10), aes(x = Date, y = daily_flow_avg))+
  geom_line(size = 1)+
  ylim(0,0.15)+
  ylab("Avg. daily flow (cms)")+
  theme_bw()+
  mytheme
inflow2
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_Michael.png", inflow2, device = "png")

inflow_hist = ggplot(data = daily_inflow, aes(x = daily_flow_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. inflow (cms)")+
  xlim(0,0.5)+
  theme_bw()
inflow_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_histogram.png", inflow_hist, device = "png")

inflow_boxplot = ggplot(data = daily_inflow, aes(x = Year, y = daily_flow_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. inflow (cms)")+
  ylim(0,0.3)+
  theme_bw()+
  mytheme
inflow_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_boxplot.png", inflow_boxplot, device = "png")

##visualization of temp
plot_temp <- diff %>%
  mutate(Date = date(DateTime))

daily_temp <- group_by(plot_temp, Date) %>% 
  summarize(daily_temp_avg = mean(Temp_C, na.rm = TRUE)) %>% 
  mutate(Year = as.factor(year(Date)),
         Month = month(Date))

temp2 = ggplot(daily_temp, aes(x = Date, y = daily_temp_avg))+
  geom_line(size = 1)+
  ylab("Avg. daily temp (C)")+
  theme_bw()
temp2
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp.png", temp2, device = "png")

temp_hist = ggplot(data = daily_temp, aes(x = daily_temp_avg, group = Year, fill = Year))+
  geom_density(alpha=0.5)+
  xlab("Daily avg. temp (C)")+
  theme_bw()
temp_hist
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp_histogram.png", inflow_hist, device = "png")

temp_boxplot = ggplot(data = daily_temp, aes(x = Year, y = daily_temp_avg, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Daily avg. temp (C)")+
  theme_bw()
temp_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/inflow_temp_boxplot.png", inflow_boxplot, device = "png")


#final data wrangling 
Inflow_Final <- diff[,c(6,7,2,4,1,5,8,3)] #orders columns
Inflow_Final <- Inflow_Final[order(Inflow_Final$DateTime),] #orders file by date

Inflow_Final <- Inflow_Final %>%
  mutate(Flow_cms = ifelse(Flow_cms <= 0, NA, Flow_cms))

#add flags
Inflow_Final <- Inflow_Final %>%
  mutate(Flag_Pressure_psi = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5,ifelse(DateTime >= "2016-04-18 15:15:00 EST",1,0)),
         Flag_Baro_pressure_psi = ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",2,0),
         Flag_Temp = ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5,0),
         Flag_Flow = ifelse(DateTime <= "2014-04-28 05:45:00" & DateTime >= "2014-03-20 09:00:00",2,
                            ifelse(DateTime <= "2017-11-13 10:45:00" & DateTime >= "2017-10-15 06:00:00",5,
                                   ifelse(DateTime >= "2016-04-18 15:15:00 EST" & Pressure_psia <= 0.18 & is.na(Flow_cms),13,
                                          ifelse(DateTime >= "2016-04-18 15:15:00 EST",1,
                                                 ifelse(Pressure_psia <= 0.18 & is.na(Flow_cms),3,0))))))

Inflow_Final$Flag_Pressure_psia <- 0

Inflow_Final <- Inflow_Final[,c(1,2,3,4,5,6,7,8,9,10,13,12,11)]
Inflow_Final <- Inflow_Final[-1,]



#write to file for working lab copy - USE THIS IF YOU HAVE RUN LINES 152-162
write.csv(Inflow_Final, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_working.csv', row.names=F) 

#write to file for EDI copy - if you have NOT run lines 152-162
Inflow_Final <- Inflow_Final %>%
  filter(DateTime <= "2018-12-31 23:45:00" & DateTime >= "2013-05-15 12:15:00")

# Write to CSV
write.csv(Inflow_Final, './Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_for_EDI_2013_2018.csv', row.names=F) 



##BONUS!! :)
#calculating water residence time
wrt <- read_csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow_downcorrected_02OCT18.csv') %>%
  mutate(date = date(DateTime))%>%
  filter(date >= "2016-05-30" & date <= "2016-07-11") %>% #SELECT YOUR DATE RANGE HERE
  mutate(wtr_res_time = 3.1E5/(Flow_cms*60*60*24))

plot_wrt <- ggplot(data = wrt, aes(x = DateTime, y = wtr_res_time))+
  geom_line(size = 1)+
  theme_bw()
plot_wrt

hist_wrt <- ggplot(data = wrt, aes(x = wtr_res_time))+
  geom_histogram(aes(y=..density..))+
  geom_density(alpha = 0.2, fill = "blue")+
  theme_bw()
hist_wrt

mean(wrt$wtr_res_time)
sd(wrt$wtr_res_time)

##plot BVR wtr level
lvl <- read_csv("./Data/DataNotYetUploadedToEDI/Raw_inflow/BVR_wtr_lvl.csv") %>%
  mutate(DateTime = parse_date_time(DateTime, 'ymd HMS',tz = "EST"))

plot_lvl <- ggplot(data = lvl, aes(x = DateTime, y = BVR_wtr_lvl_ft))+
  geom_line(size = 1, col = "blue")+
  geom_hline(yintercept = 0, col = "red", size = 1)+
  xlab("")+
  ylab("Beaverdam Water Level (ft)")+
  theme_bw()+
  mytheme
plot_lvl
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/BVR_lvl.png", plot_lvl, device = "png")

lvl$Year <- as.factor(year(lvl$DateTime))

lvl_boxplot = ggplot(data = lvl, aes(x = DateTime, y = BVR_wtr_lvl_ft, group = Year, fill = Year))+
  geom_boxplot()+
  #geom_jitter(alpha = 0.1)+
  ylab("Beaverdam Water Level (ft)")+
  theme_bw()+
  xlab("")+
  mytheme
lvl_boxplot
ggsave(filename = "./Data/DataNotYetUploadedToEDI/Raw_inflow/BVR_lvl_boxplot.png", lvl_boxplot, device = "png")

##plots for Whitney
inf <- read_csv('./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLInflow/inflow.csv') %>%
  filter(year(DateTime) == 2018 & month(DateTime) == 10)

Sept <- ggplot(data = inf, aes(x = DateTime, y = Flow_cms))+
  geom_line(size = 1)+
  xlab("")+
  ylab("Flow (cms)")+
  theme_bw()
Sept
