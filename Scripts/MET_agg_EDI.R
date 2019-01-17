###EDI MET STATION File

##Tasks Left:
#1. Relative paths
#2. Flag 1
#3. Flag 3 check
#4. Flag 4 check
#5. Flag overlap check
#6. Plots
#7. Uploading Large Files to Github

###packages needed
library("lubridate")
install.packages("tidyverse")
library(tidyverse)

###Loading in 3 datasets and aggregating: 
#a. Past Met data, manual downloads
setwd("/Users/bethany1/Desktop/MET_EDI/") #need to make into a relative path for EDI folder..

Met_past=read.csv("AllRawMetData_20181119.csv", sep = ",") #loads in data from FCR_GLM repository
Met_past$TIMESTAMP=ymd_hms(Met_past$TIMESTAMP, tz="Etc/GMT+4")

#b. Current Met data, loaded to Github by Carina
Met_now=read.csv("https://raw.githubusercontent.com/CareyLabVT/SCCData/carina-data/FCRmet.csv", skip = 4, header = F) #loads in data from SCC_data repository for latest push
if(length(names(Met_now))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
  Met_now$V17<-NULL #remove extra column
}
#renames and reformats dataset for easy bind
names(Met_now) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
Met_now$TIMESTAMP=ymd_hms(Met_now$TIMESTAMP, tz="Etc/GMT+4")
Met_now=Met_now[-c(which(is.na(Met_now$TIMESTAMP))),] #checks for NA from failed parse, and deletes from dataset
Met_now$PAR_Tot_Tot=as.numeric(Met_now$PAR_Tot_Tot) #matching str to past data
#str(Met_now); str(Met_past) #checks structure for matching

Met_agg<-rbind(Met_past,Met_now) #binds past and current data from Met station
Met_agg = Met_agg[!duplicated(Met_agg$TIMESTAMP),] #takes out duplicated values by timestamp

Met= Met_agg
Met$TIMESTAMP=ymd_hms(Met$TIMESTAMP, tz="Etc/GMT+4") #formats timestamp as double check; resulted in 1 failed parse

##Add rows for EDI and flagging
Met$Site=50 #add site
Met$Reservoir= "FCR"#add reservoir
Met$Flag= 0 #add flags
Met$Note = NA #add notes for flags

#check record for gaps
for(i in 2:length(Met$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(Met$RECORD[i]-Met$RECORD[i-1]>1){
    print(c(Met$TIMESTAMP[i-1],Met$TIMESTAMP[i]))
  }
}

#c. load in maintenance txt file
RemoveMet=read.table("/Users/bethany1/Desktop/MET_EDI/MET_MaintenanceLog.txt", sep = ",", header = T)
#set flags for flag 1 using maintenance log

###############Data Cleaning in order of columns#####################
###BattV
#flag 2
Met$Flag=ifelse(Met$BattV = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$BattV = NA & Met$Flag == 2, "BattV NA preexisting", Met$Note)

#flag overlap check
Met$Flag=ifelse(Met$BattV = NA & Met$Flag > 0, 99, Met$Flag)

plot(Met$TIMESTAMP, Met$BattV, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Panel Temp
#flag 2
Met$Flag=ifelse(Met$PTemp_C = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$PTemp_C = NA & Met$Flag == 2, "PanelTemp NA preexisting", Met$Note)

#flag overlap check
Met$Flag=ifelse(Met$PTemp_C = NA & Met$Flag > 0, 99, Met$Flag)

plot(Met$TIMESTAMP, Met$PTemp_C, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###PAR Avg + Total
#Should have same flag and NA removal for flag 1

#PAR flag 2
Met$Flag=ifelse(Met$PAR_Den_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$PAR_Den_Avg = NA & Met$Flag == 2, "PAR Avg NA preexisting", Met$Note)

Met$Flag=ifelse(Met$PAR_Den_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$PAR_Den_Avg = NA & Met$Flag == 2, "PAR Tot NA preexisting", Met$Note)

#flag overlap check
Met$Flag=ifelse(Met$PAR_Den_Avg = NA & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(Met$PAR_Tot_Tot = NA & Met$Flag > 0, 99, Met$Flag)

plot(Met$TIMESTAMP, Met$PAR_Den_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$PAR_Tot_Tot, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Barometric Pressure
#BP flag #2
Met$Flag=ifelse(Met$BP_kPa_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$BP_kPa_Avg = NA & Met$Flag == 2, "BP NA preexisting", Met$Note)

#remove negative values flag 3
Met$Flag=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, "BP set to 0", Met$Note)
Met$BP_kPa_Avg=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, 0, Met$BP_kPa_Avg)

plot(Met$TIMESTAMP, Met$BP_kPa_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###AirTemp
#AirTemp flag 2
Met$Flag=ifelse(Met$AirTC_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$AirTC_Avg = NA & Met$Flag == 2, "AirTemp NA preexisting", Met$Note)

#AirTemp flag 4
#remove > 40.56
Met$Flag=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag == 0, 4, Met$Flag)
Met$Note=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag == 4, "AirTemp set to NA over max temp", Met$Note)
Met$AirTC_Avg=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag == 0, NA, Met$AirTC_Avg)

#plots
plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Relative Humidity
#create if statement for flagging, need this to check if there are existing flags
if(Met$Flag>0 & Met$RH<0) {print("Flag overlap")} 
Met$Flag=ifelse(Met$RH < 0 & Met$Flag > 0, 5, Met$Flag)
Met$Flag=ifelse(Met$RH > 100 & Met$Flag > 0, 5, Met$Flag)
#need better solution to check flags..

#flag 2
Met$Flag=ifelse(Met$RH = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$RH = NA & Met$Flag == 2, "RH NA preexisting", Met$Note)

#set neg val to 0, insert flag, attach note for flag
Met$Flag=ifelse(Met$RH < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$RH < 0 & Met$Flag == 3, "RH set to 0", Met$Note)
Met$RH=ifelse(Met$RH < 0 & Met$Flag == 3, 0, Met$RH)

#set val to 100, insert flag, attach note for flag
Met$Flag=ifelse(Met$RH > 100 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$RH > 100 & Met$Flag == 3, "RH set to 100", Met$Note)
Met$RH=ifelse(Met$RH > 100 & Met$Flag == 3, 100, Met$RH)

plot(Met$TIMESTAMP, Met$RH, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Rainfall
#flag 2
Met$Flag=ifelse(Met$Rain_mm_Tot = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$Rain_mm_Tot = NA & Met$Flag == 2, "Rainfall NA preexisting", Met$Note)

#remove negative values
Met$Flag=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$Rain_mm_Tot=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, 0, Met$Rain_mm_Tot)

plot(Met$TIMESTAMP, Met$RH, type = 'h')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Wind speed + direction
#flag 2
Met$Flag=ifelse(Met$WS_ms_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$WS_ms_Avg = NA & Met$Flag == 2, "WS Avg NA preexisting", Met$Note)

Met$Flag=ifelse(Met$WindDir = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$WindDir = NA & Met$Flag == 2, "WindDir NA preexisting", Met$Note)

#remove negative values
Met$Flag=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$WS_ms_Avg=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, 0, Met$WS_ms_Avg)

Met$Flag=ifelse(Met$WindDir < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$WindDir < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$WindDir=ifelse(Met$WindDir < 0 & Met$Flag == 3, 0, Met$WindDir)

plot(Met$TIMESTAMP, Met$WS_ms_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$WindDir, type = 'p')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Short wave radiation
#Flag 2
Met$Flag=ifelse(Met$SR01Up_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$SR01Up_Avg = NA & Met$Flag == 2, "SR Up Avg NA preexisting", Met$Note)
Met$Flag=ifelse(Met$SR01Dn_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$SR01Dn_Avg = NA & Met$Flag == 2, "SR Dn Avg NA preexisting", Met$Note)

plot(Met$TIMESTAMP, Met$SR01Up_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$SR01Dn_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Long wave radiation
#flag 2 
Met$Flag=ifelse(Met$IR01UpCo_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$IR01UpCo_Avg = NA & Met$Flag == 2, "IR Up Avg NA preexisting", Met$Note)
Met$Flag=ifelse(Met$IR01DnCo_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$IR01DnCo_Avg = NA & Met$Flag == 2, "IR Dn Avg NA preexisting", Met$Note)

plot(Met$TIMESTAMP, Met$IR01UpCo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$IR01DnCo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

###Albedo
#flag 2
Met$Flag=ifelse(Met$Albedo_Avg = NA & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(Met$Albedo_Avg = NA & Met$Flag == 2, "Albedo Avg NA preexisting", Met$Note)

plot(Met$TIMESTAMP, Met$Albedo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

#fix column names and order
Met_final=Met[,c(18:19,1:17,20:21)] #fixes order, does not fix names yet
names(Met_final) = c("Site", "Reservoir", "DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                   "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                   "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
                   "ShortwaveRadiationDown_Average_W_m2", "InfaredRadiationUp_Average_W_m2",
                   "InfaredRadiationDown_Average_W_m2", "Albedo_Average_W_m2", "Flag", "Notes") #finalized column names
