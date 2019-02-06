###EDI MET STATION File

##Tasks/Questions Left:
#1. Create flag + notes col for all relevant columns c(5:17)
#2. Relative paths + Uploading Large Files to Github
#3. Flag 1 - 4
#4. Airtemp correction
#5. Move EDI names to earlier in code
#6. Finalize data

###packages needed
library("lubridate")
##install.packages("lubridate"); install.packages("tidyverse")
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
Met = Met[Met$TIMESTAMP< "2019-01-01 00:00:00",] #all data before 2019

#order data by timestamp
#dplyr::arrange(Met, TIMESTAMP)
Met=Met[order(Met$TIMESTAMP),]

#check record for gaps
for(i in 2:length(Met$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(Met$RECORD[i]-Met$RECORD[i-1]>1){
    print(c(Met$TIMESTAMP[i-1],Met$TIMESTAMP[i]))
  }
}
#sum(is.na(Met$TIMESTAMP)) #checking NAs, looks good

##Add rows for EDI and flagging
Met$Site=50 #add site
Met$Reservoir= "FCR"#add reservoir

#EDI Column names
names(Met) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
                     "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
                     "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
                     "ShortwaveRadiationDown_Average_W_m2", "InfaredRadiationUp_Average_W_m2",
                     "InfaredRadiationDown_Average_W_m2", "Albedo_Average_W_m2", "Site", "Reservoir") #finalized column names


#c. load in maintenance txt file
RemoveMet=read.table("/Users/bethany1/Desktop/MET_EDI/MET_MaintenanceLog.txt", sep = ",", header = T)
#str(RemoveMet)
RemoveMet$TIMESTAMP_start=ymd_hms(RemoveMet$TIMESTAMP_start, tz="Etc/GMT+4")
RemoveMet$TIMESTAMP_end=ymd_hms(RemoveMet$TIMESTAMP_end, tz="Etc/GMT+4")
#set flags for flag 1 using maintenance log below


#### Flag creation ####
#create flag + notes columns for data columns c(5:17)
#### All Flags ####
for(i in 5:17) { #for loop to create new columns in data frame
  df[,i] <- 0
  df[i+1] <- NA
}


#Flag 1 & 4
#for loop inserts flags and notes, then sets relevant data to NA
for (i in 1:nrow(RemoveMet)){ #makes i # of rows in Maintenance log
  #plug in flag and notes from timeframe
  if(RemoveMet$colnumber[i]==8){ #set Air Temp flags
    Met$Flag_AirTemp[Met$TIMESTAMP>=RemoveMet$TIMESTAMP_start[i] & Met$TIMESTAMP<=RemoveMet$TIMESTAMP_end[i]]=RemoveMet$flag[i]
    Met$Notes_AirTemp[Met$TIMESTAMP>=RemoveMet$TIMESTAMP_start[i] & Met$TIMESTAMP<=RemoveMet$TIMESTAMP_end[i]]=RemoveMet$notes[i]
  } 
  #set all other flags
  else {Met$Flag[Met$TIMESTAMP>=RemoveMet$TIMESTAMP_start[i] & Met$TIMESTAMP<=RemoveMet$TIMESTAMP_end[i]]=RemoveMet$flag[i]
  Met$Notes[Met$TIMESTAMP>=RemoveMet$TIMESTAMP_start[i] & Met$TIMESTAMP<=RemoveMet$TIMESTAMP_end[i]]=RemoveMet$notes[i] 
  } 
  #if flag == 1, set parameter to NA
  if(RemoveMet$flag[i]==1)
  {Met[Met$TIMESTAMP>=RemoveMet$TIMESTAMP_start[i] & Met$TIMESTAMP<=RemoveMet$TIMESTAMP_end[i],
       RemoveMet$colnumber[i]] = NA}
  
}

###############Data Cleaning in order of columns#####################
#skips airtemp
###BattV
#flag 2 overlap check
Met$Flag=ifelse(is.na(Met$BattV) & Met$Flag > 0, 99, Met$Flag) #inserting error flag
##length(which(Met$Flag==99)); plot(Met$TIMESTAMP, Met$Flag, type = 'p') # # of error flags; plot to check for error flags looking good

#flag 2
Met$Flag=ifelse(is.na(Met$BattV) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$BattV) & Met$Flag == 2, "BattV NA preexisting", Met$Note)

###Panel Temp
#flag 2 overlap check
Met$Flag=ifelse(is.na(Met$PTemp_C) & Met$Flag > 0, 99, Met$Flag) #error flag
##length(which(Met$Flag==99))

#flag 2
Met$Flag=ifelse(is.na(Met$PTemp_C) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$PTemp_C) & Met$Flag == 2, "PanelTemp NA preexisting", Met$Note)

###PAR Avg + Total
#Should have same flag and NA removal for flag 1
#flag 2 overlap check
Met$Flag=ifelse(is.na(Met$PAR_Den_Avg) & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(is.na(Met$PAR_Tot_Tot) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)); plot(Met$TIMESTAMP, Met$Flag, type = 'p') #good to go

#PAR flag 2
Met$Flag=ifelse(is.na(Met$PAR_Den_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$PAR_Den_Avg) & Met$Flag == 2, "PAR Avg NA preexisting", Met$Note)

Met$Flag=ifelse(is.na(Met$PAR_Den_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$PAR_Den_Avg) & Met$Flag == 2, "PAR Tot NA preexisting", Met$Note)

###Barometric Pressure
#flag 2 over lap check
Met$Flag=ifelse(is.na(Met$BP_kPa_Avg) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good

#BP flag #2
Met$Flag=ifelse(is.na(Met$BP_kPa_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$BP_kPa_Avg) & Met$Flag == 2, "BP NA preexisting", Met$Note)

#flag 3 overlap check
Met$Flag=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good

#remove negative values flag 3
Met$Flag=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, "BP set to 0", Met$Note)
Met$BP_kPa_Avg=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, 0, Met$BP_kPa_Avg)

###Relative Humidity
#flag 2 check
Met$Flag=ifelse(is.na(Met$RH) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 2
Met$Flag=ifelse(is.na(Met$RH) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$RH) & Met$Flag == 2, "RH NA preexisting", Met$Note)

#flag 3 check
Met$Flag=ifelse(Met$RH < 0 & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(Met$RH < 100 & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 3 set neg val to 0, insert flag, attach note for flag
Met$Flag=ifelse(Met$RH < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$RH < 0 & Met$Flag == 3, "RH set to 0", Met$Note)
Met$RH=ifelse(Met$RH < 0 & Met$Flag == 3, 0, Met$RH)

#set val to 100, insert flag, attach note for flag
Met$Flag=ifelse(Met$RH > 100 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$RH > 100 & Met$Flag == 3, "RH set to 100", Met$Note)
Met$RH=ifelse(Met$RH > 100 & Met$Flag == 3, 100, Met$RH)

###Rainfall
#flag 2 check
Met$Flag=ifelse(is.na(Met$Rain_mm_Tot) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 2
Met$Flag=ifelse(is.na(Met$Rain_mm_Tot) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$Rain_mm_Tot) & Met$Flag == 2, "Rainfall NA preexisting", Met$Note)

#flag 3 check
Met$Flag=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 3 remove negative values
Met$Flag=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$Rain_mm_Tot=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, 0, Met$Rain_mm_Tot)

###Wind speed + direction
#flag 2 check
Met$Flag=ifelse(is.na(Met$WS_ms_Avg) & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(is.na(Met$WindDir) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 2
Met$Flag=ifelse(is.na(Met$WS_ms_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$WS_ms_Avg) & Met$Flag == 2, "WS Avg NA preexisting", Met$Note)

Met$Flag=ifelse(is.na(Met$WindDir) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$WindDir) & Met$Flag == 2, "WindDir NA preexisting", Met$Note)

#flag 3 check
Met$Flag=ifelse(Met$WS_ms_Avg < 0 & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(Met$WindDir < 0 & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 3 remove negative values
Met$Flag=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$WS_ms_Avg=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, 0, Met$WS_ms_Avg)

Met$Flag=ifelse(Met$WindDir < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$WindDir < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$WindDir=ifelse(Met$WindDir < 0 & Met$Flag == 3, 0, Met$WindDir)

###Short wave radiation and Albedo
#note: When SR up = NA, so does Albedo
Met_prealb=Met
#flag 2 check
Met$Flag=ifelse(is.na(Met$SR01Up_Avg) & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(is.na(Met$Albedo_Avg) & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(is.na(Met$SR01Dn_Avg) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#Flag 2
Met$Flag=ifelse(is.na(Met$SR01Dn_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$SR01Dn_Avg) & Met$Flag == 2, "SR Dn Avg NA preexisting", Met$Note)
Met$Flag=ifelse(is.na(Met$Albedo_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$Albedo_Avg) & Met$Flag == 2, "Albedo Avg NA preexisting", Met$Note)
Met$Flag=ifelse(is.na(Met$SR01Up_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$SR01Up_Avg) & Met$Flag == 2, "SR Up Avg and Albedo NA preexisting", Met$Note)

###Long wave radiation
#flag 2 check
Met$Flag=ifelse(is.na(Met$IR01UpCo_Avg) & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(is.na(Met$IR01DnCo_Avg) & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 2 
Met$Flag=ifelse(is.na(Met$IR01UpCo_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$IR01UpCo_Avg) & Met$Flag == 2, "IR Up Avg NA preexisting", Met$Note)
Met$Flag=ifelse(is.na(Met$IR01DnCo_Avg) & Met$Flag == 0, 2, Met$Flag)
Met$Note=ifelse(is.na(Met$IR01DnCo_Avg) & Met$Flag == 2, "IR Dn Avg NA preexisting", Met$Note)

#flag 3 check
Met$Flag=ifelse(Met$IR01UpCo_Avg < 0 & Met$Flag > 0, 99, Met$Flag)
Met$Flag=ifelse(Met$IR01DnCo_Avg < 0 & Met$Flag > 0, 99, Met$Flag)
#length(which(Met$Flag==99)) #good to go

#flag 3 remove negative values
Met$Flag=ifelse(Met$IR01UpCo_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$IR01UpCo_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$IR01UpCo_Avg=ifelse(Met$IR01UpCo_Avg < 0 & Met$Flag == 3, 0, Met$IR01UpCo_Avg)

Met$Flag=ifelse(Met$IR01DnCo_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$IR01DnCo_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$IR01DnCo_Avg=ifelse(Met$IR01DnCo_Avg < 0 & Met$Flag == 3, 0, Met$IR01DnCo_Avg)

#Plot for flags 2&3
plot(Met$Flag, type = 'h')
#plot(Met$TIMESTAMP, Met$Flag, type = 'p')
########### AirTemp ###########

#AirTemp flag 2
Met$Flag_AirTemp=ifelse(is.na(Met$AirTC_Avg) & Met$Flag_AirTemp == 0, 2, Met$Flag_AirTemp)
Met$Note_AirTemp=ifelse(is.na(Met$AirTC_Avg) & Met$Flag_AirTemp == 2, "AirTemp NA preexisting", Met$Note_AirTemp)

#flag 4 overlap check
Met$Flag_AirTemp=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag_AirTemp > 0, 99, Met$Flag_AirTemp)
#length(which(Met$Flag==99)) #good to go

plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l')

#AirTemp flag 4
#remove > 40.56
Met$Flag_AirTemp=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag_AirTemp == 0, 4, Met$Flag_AirTemp)
Met$Note_AirTemp=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag_AirTemp == 4, "AirTemp set to NA over max temp", Met$Note_AirTemp)
Met$AirTC_Avg=ifelse(Met$AirTC_Avg > 40.56 & Met$Flag_AirTemp == 4, NA, Met$AirTC_Avg)

#plots
plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l')

# ####Air Temp vs. Panel Temp
# #air temp red, p temp black
# # 1. PTemp vs. Air Temp whole time series
# plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l', col='red')
# lines(Met$TIMESTAMP, Met$PTemp_C, type = 'l')
# 
# # 2. 1:1 Ptemp vs. Air temp whole time series
# lm_Met=lm(Met$PTemp_C ~ Met$AirTC_Avg)
# plot(Met$PTemp_C, Met$AirTC_Avg)
# abline(lm_Met, col = "blue")
# print(lm_Met$coefficients)
# 
# # 3. Ptemp vs. Air Temp 2016 
# Met_16=Met[year(Met$TIMESTAMP) == 2016,]
# plot(Met_16$TIMESTAMP, Met_16$AirTC_Avg, type = 'l', col='red')
# lines(Met_16$TIMESTAMP, Met_16$PTemp_C, type = 'l')
# 
# # 4. 1:1 Ptemp vs. AirTemp 2016
# lm_Met16=lm(Met_16$PTemp_C ~ Met_16$AirTC_Avg)
# plot(Met_16$PTemp_C, Met_16$AirTC_Avg)
# abline(lm_Met16, col = "blue")
# print(lm_Met16$coefficients)
# 
# # 5. Ptemp vs. Air Temp after 2017 
# Met_hot=Met[Met$TIMESTAMP > "2016-12-31 23:59:00",]
# plot(Met_hot$TIMESTAMP, Met_hot$AirTC_Avg, type = 'l', col='red')
# lines(Met_hot$TIMESTAMP, Met_hot$PTemp_C, type = 'l')
# 
# # 6. 1:1 Ptemp vs. AirTemp after 2017
# lm_Methot=lm(Met_hot$PTemp_C ~ Met_hot$AirTC_Avg)
# plot(Met_hot$PTemp_C, Met_hot$AirTC_Avg)
# abline(lm_Methot, col = "blue")
# print(lm_Methot$coefficients)
# 
# # 7. Ptemp vs. Air Temp 2018 plus line noting cleaning time
# Met_18=Met[year(Met$TIMESTAMP) == 2018,]
# plot(Met_18$TIMESTAMP, Met_18$AirTC_Avg, type = 'l', col='red')
# lines(Met_18$TIMESTAMP, Met_18$PTemp_C, type = 'l')
# abline(v=ymd_hms("2018-09-03 11:40:00"), col="blue", lwd = 2)
# 
# lm_Met18=lm(Met_18$PTemp_C ~ Met_18$AirTC_Avg)
# plot(Met_18$PTemp_C, Met_18$AirTC_Avg)
# abline(lm_Met18, col = "blue")
# print(lm_Met18$coefficients)
# 
# # 8. 1:1 Ptemp vs. AirTemp Jan 2018 - Sep 2 2018
# Met_early18=Met[Met$TIMESTAMP > "2017-12-31 23:59:00"& Met$TIMESTAMP < "2018-09-03 00:00:00",]
# lm_Metearly18=lm(Met_early18$PTemp_C ~ Met_early18$AirTC_Avg)
# plot(Met_early18$PTemp_C, Met_early18$AirTC_Avg)
# abline(lm_Metearly18, col = "blue")
# print(lm_Metearly18$coefficients)
# 
# # 9. 1:1 Ptemp vs. AirTemp Sep 3 2018 - Dec 2018
# Met_late18=Met[Met$TIMESTAMP > "2018-09-03 11:40:00"& Met$TIMESTAMP < "2019-01-01 00:00:00",]
# lm_Metlate18=lm(Met_late18$PTemp_C ~ Met_late18$AirTC_Avg)
# plot(Met_late18$PTemp_C, Met_late18$AirTC_Avg)
# abline(lm_Metlate18, col = "blue")
# print(lm_Metlate18$coefficients)



### Alternative Flagging ####

Met$Flag1= 0; Met$Note1= NA
Met$Flag2= 0; Met$Note2= NA
Met$Flag3= 0; Met$Note3= NA
Met$Flag4= 0 ;Met$Note4= NA

#flags for flag 2
#pre-exisiting NAs

for (i in nrow(Met)) {
  if(sum(is.na(Met[i,c(1:17)]))>0){ #checks if there are any NAs in row
    Met$Flag2[i]=2 #adds flag if there are NAs
    Met$Note2[i] = paste0("Sample not collected:", colnames(Met[which(is.na(Met[i,c(1:17)]))]))
  }
}


#3 = negative values set to 0 or percent greater than 100 set to 100
for (i in nrow(Met)) {
  if(length(which(Met[i,c(3,5:7,9:17)]<0))>0){ #checks for any relevant columns with data <0
    Met$Flag3[i]=3 #adds flag if there are NAs
    Met$Note3[i] = paste0("Negative value set to 0:", colnames(Met[which(is.na(Met[i,c(1:17)]))]))
  }
  if(Met[i,9]>100){
    Met$Flag3[i]=3
    Met$Note3[i]= paste0(Met$Note3[i], "and ", colnames(Met[9]))
  }
}


#######Plots For Days ######
plot(Met$TIMESTAMP, Met$BattV, type = 'l')
plot(Met$TIMESTAMP, Met$PTemp_C, type = 'l')
plot(Met$TIMESTAMP, Met$PAR_Den_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$PAR_Tot_Tot, type = 'l')
plot(Met$TIMESTAMP, Met$BP_kPa_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$AirTC_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$RH, type = 'l')
plot(Met$TIMESTAMP, Met$Rain_mm_Tot, type = 'h')
plot(Met$TIMESTAMP, Met$WS_ms_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$WindDir, type = 'p')
plot(Met$TIMESTAMP, Met$SR01Up_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$SR01Dn_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Albedo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$IR01UpCo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$IR01DnCo_Avg, type = 'l')
plot(Met$TIMESTAMP, Met$Flag, type = 'p')

Met_final=Met[,c(18:19,1:17, 20:35)] #final column order