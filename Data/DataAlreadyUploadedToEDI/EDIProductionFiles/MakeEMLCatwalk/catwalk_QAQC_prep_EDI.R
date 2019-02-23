
library(tidyverse)

download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','Catwalk.csv')

# get the catdata into the right shape and format
catheader<-read.csv("Catwalk.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("Catwalk.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting
catdata$TIMESTAMP <- as.POSIXct(catdata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S" )
# format as numeric
for(j in 5:ncol(catdata)){
  catdata[,j]<-as.numeric(levels(catdata[,j]))[catdata[,j]]#need to set all columns to numeric values
}
# remove NANs at beginning
catdata <- catdata[catdata$TIMESTAMP>"2018-07-05 14:50:00",]
catdata <- catdata %>% select(-EXO_Date, -EXO_Time)
catdata <- catdata %>% select(-RECORD, -BattV, -Ptemp_C, everything())
catdata$Reservoir <- "FCR"
catdata$Site <- "50"
catdata <- catdata %>% select(Reservoir, Site,everything() )

colnames(catdata) <- c("Reservoir", "Site", "DateTime", "ThermistorTemp_C_surface", "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5", "ThermistorTemp_C_6",
                     "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9" ,"RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                     "RDOsat_percent_9",  "RDOTemp_C_9","EXOTemp_C_1", "EXOCond_uScm_1", "EXOSpCond_uScm_1",
                     "EXOTDS_mgL_1", "EXODOsat_percent_1","EXODO_mgL_1", "EXOChla_RFU_1","EXOChla_ugL_1" ,"EXOBGAPC_RFU_1", 
                     "EXOBGAPC_ugL_1" , "EXOfDOM_RFU_1", "EXOfDOM_QSU_1", "EXO_pressure", "EXO_depth",
                     "EXO_battery", "EXO_cablepower", "EXO_wiper", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C"  )

catdata$Flag_All <- "0"
catdata$Flag_DO_1 <- '0'
catdata$Flag_DO_5 <- '0'
catdata$Flag_DO_9 <- '0'
catdata$Flag_Chla <- '0'
catdata$Flag_Phyco <- '0'
catdata$Flag_TDS <- '0'



# replace data with NA and create flag for day that it was up for cleaning
# 2018-11-19 13:10:00 EDT, 2018-11-19 13:30:00 EDT, All_Cat, sensor string up for cleaning, 1
# and set the Flag_all to 1
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-19 13:40:00', 4:31] <- 'NA'
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-19 13:40:00', 40] <- '1'

# and DO values which take longer to re-adjust back to the environment
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-19 14:50:00', 14:15] <- 'NA'
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-19 14:50:00', 42] <- '1'
# 9m is less straight forward, not sure where to stop NA replace
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-20 15:00:00', 17:18] <- 'NA'
catdata[catdata$DateTime >'2018-11-19 13:00:00' & catdata$DateTime <'2018-11-20 15:00:00', 43] <- '1'

# now the Dec 17 day
catdata[catdata$DateTime >'2018-12-17 11:20:00' & catdata$DateTime <'2018-12-17 11:50:00', 4:31] <- 'NA'
catdata[catdata$DateTime >'2018-12-17 11:20:00' & catdata$DateTime <'2018-12-17 11:50:00', 40] <- '1'

# RDO at 5m has longer time period of adjustment
catdata[catdata$DateTime >'2018-12-17 11:20:00' & catdata$DateTime <'2018-12-17 14:00:00', 14:15] <- 'NA'
catdata[catdata$DateTime >'2018-12-17 11:20:00' & catdata$DateTime <'2018-12-17 14:00:00', 42] <- '1'

# RDO 9m has not difference before and after this time period--maybe it wasn't brought up

# after October, remove EXOChl_ugL_1 values that are above 4X the standard deviation as these are caused by fouling
#sd(catdata$EXOChla_ugL_1, na.rm= TRUE)*4, so anything greater than 15 in the late season goes
#sd(catdata$EXOChla_RFU_1, na.rm= TRUE)*4 #so anything over 4 in the late season goes
catdata$EXOChla_ugL_1 <- as.numeric(catdata$EXOChla_ugL_1)
catdata$EXOChla_RFU_1 <- as.numeric(catdata$EXOChla_RFU_1)

# first change the flags for any days that have fouling and need NAs
catdata <- catdata %>%
  mutate(Flag_Chla = ifelse(DateTime > '2018-10-01 00:00:00' & EXOChla_ugL_1 > 15, 4, Flag_Chla))
# now change the fouled data to NA for chla in ugL and RFU
plot(catdata$Date, catdata$EXOChla_ugL_1)
catdata <- catdata %>%
  mutate(EXOChla_ugL_1 = ifelse(DateTime > '2018-10-01 00:00:00' & EXOChla_ugL_1 > 15, NA, EXOChla_ugL_1))
plot(catdata$Date, catdata$EXOChla_ugL_1)
length(catdata$DateTime > '2018-10-01 00:00:00' & catdata$EXOChla_ugL_1 > 15)
length(catdata$DateTime > '2018-10-01 00:00:00' & catdata$EXOChla_RFU_1 > 10)
# these two are the same length so the same number of datapoints are taken out for both parameters
# the RFU data has fouling as well but I'm not sure what the cut off should be
# the 4X the sd method does not seem appropriate here based on the pattern of data
plot(catdata$Date, catdata$EXOChla_RFU_1, type = 'l')
# first the flags
catdata <- catdata %>%
  mutate(Flag_Chla = ifelse(DateTime > '2018-10-01 00:00:00' & EXOChla_RFU_1 > 10, 4, Flag_Chla))
catdata <- catdata %>%
  mutate(EXOChla_RFU_1 = ifelse(DateTime > '2018-11-01 00:00:00' & EXOChla_RFU_1 > 10, NA, EXOChla_RFU_1))
plot(catdata$Date, catdata$EXOChla_RFU_1, type = 'l')


# phyco data has a major outlier in December
# what level to cut off at?
# sd(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)*4
plot(catdata$Date, catdata$EXOBGAPC_RFU_1)
plot(catdata$Date, catdata$EXOBGAPC_ugL_1)
# first the flags
catdata <- catdata %>% 
  mutate(Flag_Phyco = ifelse(DateTime > '2018-11-01' & EXOBGAPC_RFU_1>0.5 & EXOBGAPC_RFU_1<1, 5, Flag_Phyco))%>%
  mutate(Flag_Phyco = ifelse(DateTime > '2018-11-01' & EXOBGAPC_RFU_1 >1, 4, Flag_Phyco))
  
# now the data
catdata <- catdata %>%
  mutate(EXOBGAPC_RFU_1 = ifelse(DateTime > '2018-11-01' & EXOBGAPC_RFU_1 >1, NA, EXOBGAPC_RFU_1)) %>%
  mutate(EXOBGAPC_ugL_1 = ifelse(DateTime > '2018-11-01' & EXOBGAPC_ugL_1 >1, NA, EXOBGAPC_RFU_1)) 
  
plot(catdata$Date, catdata$EXOBGAPC_RFU_1)
plot(catdata$Date, catdata$EXOBGAPC_ugL_1)


# now fix the negative DO values
catdata <- catdata %>%  #RDO at 5m
  mutate(Flag_DO_5 = ifelse(RDO_mgL_5 < 0, 3, Flag_DO_5)) %>%
  mutate(RDO_mgL_5 = ifelse(RDO_mgL_5 < 0, 0, RDO_mgL_5)) %>%
  mutate(RDOsat_percent_5 = ifelse(RDOsat_percent_5 < 0, 0, RDOsat_percent_5))

catdata <- catdata %>%
  mutate(Flag_DO_9 = ifelse(RDO_mgL_9 < 0, 3, Flag_DO_9)) %>%
  mutate(RDO_mgL_9 = ifelse(RDO_mgL_9 < 0, 0, RDO_mgL_9)) %>%
  mutate(RDOsat_percent_9 = ifelse(RDOsat_percent_9 < 0, 0, RDOsat_percent_9))

catdata <- catdata %>%
  mutate(Flag_DO_1 = ifelse(EXODO_mgL_1 < 0, 3, Flag_DO_1)) %>%
  mutate(EXODO_mgL_1 = ifelse(EXODO_mgL_1 < 0, 0, EXODO_mgL_1)) %>%
  mutate(EXODOsat_percent_1 = ifelse(EXODOsat_percent_1 <0, 0, EXODOsat_percent_1))

# TDS has some wonky values at the beginning of the dataset
plot(catdata$Date, catdata$EXOTDS_mgL_1)
sep <- catdata[catdata$DateTime>"2018-09-01" & catdata$DateTime<"2018-10-01",]
plot(sep$DateTime, sep$EXOTDS_mgL_1)

# change values to NA before Sep
# first the flag
catdata <- catdata %>%
  mutate(Flag_TDS = ifelse(DateTime < "2018-09-01", 1, Flag_TDS))
catdata <- catdata %>%
  mutate(EXOTDS_mgL_1 =ifelse(DateTime < "2018-09-01", NA, EXOTDS_mgL_1) )
plot(catdata$Date, catdata$EXOTDS_mgL_1)

# limit to 2018 data only
catdata <- catdata[catdata$DateTime<'2019-01-01 00:00:00',]


write.csv(catdata, "Catwalk_cleanedEDI.csv", row.names = FALSE)

###############################################################################################################################
# summarize down to daily means for exploratory graphing
# the thermistor dataset
therm_daily <- catdata %>% select(DateTime, ThermistorTemp_C_surface:RDOTemp_C_9)

therm_daily <- therm_daily %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>% # create a column of just the date
  select(-DateTime) %>%
  select(Date, everything())
  
# struggled with a for loop, did it brute force :(
therm_daily$ThermistorTemp_C_surface <- as.numeric(therm_daily$ThermistorTemp_C_surface)
therm_daily$ThermistorTemp_C_1 <- as.numeric(therm_daily$ThermistorTemp_C_1)
therm_daily$ThermistorTemp_C_2 <- as.numeric(therm_daily$ThermistorTemp_C_2)
therm_daily$ThermistorTemp_C_3 <- as.numeric(therm_daily$ThermistorTemp_C_3)
therm_daily$ThermistorTemp_C_4 <- as.numeric(therm_daily$ThermistorTemp_C_4)
therm_daily$ThermistorTemp_C_5 <- as.numeric(therm_daily$ThermistorTemp_C_5)
therm_daily$ThermistorTemp_C_6 <- as.numeric(therm_daily$ThermistorTemp_C_6)
therm_daily$ThermistorTemp_C_7 <- as.numeric(therm_daily$ThermistorTemp_C_7)
therm_daily$ThermistorTemp_C_8 <- as.numeric(therm_daily$ThermistorTemp_C_8)
therm_daily$ThermistorTemp_C_9 <- as.numeric(therm_daily$ThermistorTemp_C_9)
therm_daily$RDO_mgL_5 <- as.numeric(therm_daily$RDO_mgL_5)
therm_daily$RDOsat_percent_5 <- as.numeric(therm_daily$RDOsat_percent_5)
therm_daily$RDOTemp_C_5 <- as.numeric(therm_daily$RDOTemp_C_5)
therm_daily$RDO_mgL_9 <- as.numeric(therm_daily$RDO_mgL_9)
therm_daily$RDOsat_percent_9 <- as.numeric(therm_daily$RDOsat_percent_9)
therm_daily$RDOTemp_C_9 <- as.numeric(therm_daily$RDOTemp_C_9)

attach(therm_daily)
plot(Date, ThermistorTemp_C_1, type = 'l')
plot(Date, ThermistorTemp_C_2, type = 'l')
plot(Date, ThermistorTemp_C_3, type = 'l')
plot(Date, ThermistorTemp_C_4, type = 'l')
plot(Date, ThermistorTemp_C_5, type = 'l')
plot(Date, ThermistorTemp_C_6, type = 'l')
plot(Date, ThermistorTemp_C_7, type = 'l')
plot(Date, ThermistorTemp_C_8, type = 'l')
plot(Date, ThermistorTemp_C_9, type = 'l')
plot(Date, RDO_mgL_5, type = 'l')
plot(Date, RDOsat_percent_5, type = 'l')
plot(Date, RDOTemp_C_5, type = 'l')
plot(Date, RDO_mgL_9, type = 'l') 
plot(Date, RDOsat_percent_9, type = 'l')
plot(Date, RDOTemp_C_9, type = 'l')
detach(therm_daily)


therm_daily_mean <- therm_daily %>%
  group_by(Date) %>%
  summarise_all(mean)



# the exo dataset
exo <- catdata %>% select(DateTime, EXOTemp_C_1:EXOfDOM_QSU_1)
exo <- exo %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%# create a column of just the date
  select(-DateTime)      

exo <- exo %>%
  mutate(EXOTemp_C_1 = as.numeric(EXOTemp_C_1)) %>%
  mutate(EXOCond_uScm_1 = as.numeric(EXOCond_uScm_1)) %>%
  mutate(EXOSpCond_uScm_1 = as.numeric(EXOSpCond_uScm_1)) %>%
  mutate(EXOTDS_mgL_1 = as.numeric(EXOTDS_mgL_1)) %>%
  mutate(EXODOsat_percent_1 = as.numeric(EXODOsat_percent_1)) %>%
  mutate(EXODO_mgL_1 = as.numeric(EXODO_mgL_1)) %>%
  mutate(EXOChla_RFU_1 = as.numeric(EXOChla_RFU_1)) %>%
  mutate(EXOChla_ugL_1 = as.numeric(EXOChla_ugL_1)) %>%
  mutate(EXOBGAPC_RFU_1 = as.numeric(EXOBGAPC_RFU_1)) %>%
  mutate(EXOBGAPC_ugL_1 = as.numeric(EXOBGAPC_ugL_1)) %>%
  mutate(EXOfDOM_RFU_1 = as.numeric(EXOfDOM_RFU_1)) %>%
  mutate(EXOfDOM_QSU_1 = as.numeric(EXOfDOM_QSU_1))
attach(exo)
plot(Date, EXOChla_ugL_1, type = 'l')
plot(Date, EXOTemp_C_1)
plot(Date, EXOCond_uScm_1, type = 'l')
plot(Date, EXOSpCond_uScm_1, type = 'l')
plot(Date, EXOTDS_mgL_1, type = 'l') # some weird values at the beginning?
plot(Date, EXODOsat_percent_1, type = 'l')
plot(Date, EXODO_mgL_1,type = 'l', col = 'black')
plot(Date, EXOChla_RFU_1)
plot(Date, EXOChla_ugL_1, type = 'l')
plot(Date, EXOBGAPC_RFU_1) #major outlier beginning of december
Nov <- exo[exo$Date>"2018-11-01",]
plot(Nov$Date, Nov$EXOBGAPC_RFU_1)
plot(Date, EXOBGAPC_ugL_1, type = 'l') #major outlier beginning of december
plot(Date, EXOfDOM_RFU_1)
plot(Date, EXOfDOM_QSU_1, type = 'l')
detach(exo)




exo_daily <- exo %>%
group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(mean)








