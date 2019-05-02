###EDI MET STATION File

##Tasks/Questions Left:
#1a. Uploading Large Files to Github
#1b. (opt) Relative paths in smarter way?
#2. PAR TOT weirdness at end of 2018
#Trim PAR TOT by highest value in 2016
#3. Inf Rad fix
#4. BP check low dates with weather reports
#5. SW rad up highest val 
#6. make table for flags to publish with data
#7. Inf Rad DN with sin curve equation, by DOY

###packages needed
library("lubridate")
##install.packages("lubridate"); install.packages("tidyverse")
library(tidyverse)

###Loading in 3 datasets and aggregating: 
#a. Past Met data, manual downloads
RawMet_1516=read.csv('https://raw.githubusercontent.com/CareyLabVT/FCR-GLM/master/RawMetData_2015_2016.csv',header = T) #2015-2016 data
RawMet_17=read.csv('https://raw.githubusercontent.com/CareyLabVT/FCR-GLM/master/RawMetData_2017.csv',header = T) #2017 data
RawMet_18=read.csv('https://raw.githubusercontent.com/CareyLabVT/FCR-GLM/master/RawMetData_2018.csv',header = T) #2018 data
mytempdata = rbind(RawMet_1516, RawMet_17) #merges first 3 years
Met_past = rbind(mytempdata, RawMet_18) #merges 2018 with data
Met_past$TIMESTAMP=ymd_hms(Met_past$TIMESTAMP, tz="Etc/GMT+4") #formats to be same

#### b. Current Met data, loaded to Github by Carina ####
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

#### Aggregated data set for QA/QC ####
Met= Met_agg
Met$TIMESTAMP=ymd_hms(Met$TIMESTAMP, tz="Etc/GMT+4") #formats timestamp as double check; resulted in 1 failed parse
Met = Met[year(Met$TIMESTAMP)<2019,] #all data before 2019

#order data by timestamp
#dplyr::arrange(Met, TIMESTAMP) #tidyverse version
Met=Met[order(Met$TIMESTAMP),]

#check record for gaps
for(i in 2:length(Met$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(Met$RECORD[i]-Met$RECORD[i-1]>1){
    print(c(Met$TIMESTAMP[i-1],Met$TIMESTAMP[i]))
  }
}

#EDI Column names
names(Met) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
               "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
               "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
               "ShortwaveRadiationDown_Average_W_m2", "InfaredRadiationUp_Average_W_m2",
               "InfaredRadiationDown_Average_W_m2", "Albedo_Average_W_m2") #finalized column names
Met$Site=50 #add site
Met$Reservoir= "FCR"#add reservoir

Met_raw=Met
#### c. load in maintenance txt file #### 
RemoveMet=read.table("https://raw.githubusercontent.com/CareyLabVT/SCCData/carina-data/MET_MaintenanceLog.txt", sep = ",", header = T)
#str(RemoveMet)
RemoveMet$TIMESTAMP_start=ymd_hms(RemoveMet$TIMESTAMP_start, tz="Etc/GMT+4") #setting time zone
RemoveMet$TIMESTAMP_end=ymd_hms(RemoveMet$TIMESTAMP_end, tz="Etc/GMT+4") #setting time zone
RemoveMet$notes=as.character(RemoveMet$notes)

#### Flag creation ####
#create flag + notes columns for data columns c(5:17)
#set flag 2
for(i in 5:17) { #for loop to create new columns in data frame
  Met[,paste0("Flag_",colnames(Met[i]))] <- 0 #creates flag column + name of variable
  Met[,paste0("Note_",colnames(Met[i]))] <- NA #creates note column + names of variable
  Met[c(which(is.na(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-2 #puts in flag 2
  Met[c(which(is.na(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Sample not collected" #note for flag 2
}

##Air TEMP cleaning
#using lm_Panel2015 to clean airtemp.
#create lm for 2015
MetAir_2015=Met[Met$DateTime<"2016-01-01 00:00:00",c(1,4,8)]
lm_Panel2015=lm(MetAir_2015$AirTemp_Average_C ~ MetAir_2015$CR3000Panel_temp_C)
summary(lm_Panel2015)
#if Air - Panel > 3 sd(lm_Panel2015) then replace with PanelTemp in lm equation
Met$Flag_AirTemp_Average_C=ifelse((Met$AirTemp_Average_C - (1.6278+(0.9008*Met$CR3000Panel_temp_C)))>(3*sd(lm_Panel2015$residuals)), 4, Met$Flag_AirTemp_Average_C)
Met$Note_AirTemp_Average_C=ifelse((Met$AirTemp_Average_C - (1.6278+(0.9008*Met$CR3000Panel_temp_C)))>(3*sd(lm_Panel2015$residuals)),"Substituted value calculated from Panel Temp and linear model", Met$Note_AirTemp_Average_C)
Met$AirTemp_Average_C=ifelse((Met$AirTemp_Average_C - (1.6278+(0.9008*Met$CR3000Panel_temp_C)))>(3*sd(lm_Panel2015$residuals)),(1.6278+(0.9008*Met$CR3000Panel_temp_C)), Met$AirTemp_Average_C)

#fix infrared radiation voltage reading, must be after airtemp correction
Met$Flag_InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<100,4,Met$Flag_InfaredRadiationUp_Average_W_m2)
Met$Note_InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<100,"Value corrected from Voltage with InfRadUp equation as decribed in metadata",Met$Note_InfaredRadiationUp_Average_W_m2)
Met$InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<100,Met$InfaredRadiationUp_Average_W_m2+5.67*10^-8*(Met$AirTemp_Average_C+273.15)^4,Met$InfaredRadiationUp_Average_W_m2)

Met$Flag_InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<250,4,Met$Flag_InfaredRadiationDown_Average_W_m2)
Met$Note_InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<250,"Value corrected from Voltage with InfRadDn equation as decribed in metadata",Met$Note_InfaredRadiationDown_Average_W_m2)
Met$InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<250,5.67*10^-8*(Met$AirTemp_Average_C+273.15)^4,Met$InfaredRadiationDown_Average_W_m2)

#Inf outliers, must go after corrections
Met$Flag_InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<150,4,Met$Flag_InfaredRadiationUp_Average_W_m2)
Met$Note_InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<150,"Outlier set to NA",Met$Note_InfaredRadiationUp_Average_W_m2)
Met$InfaredRadiationUp_Average_W_m2=ifelse(Met$InfaredRadiationUp_Average_W_m2<150,NA,Met$InfaredRadiationUp_Average_W_m2)

# Met$Flag_InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<200,4,Met$Flag_InfaredRadiationDown_Average_W_m2)
# Met$Note_InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<200,"Outlier set to NA",Met$Note_InfaredRadiationDown_Average_W_m2)
# Met$InfaredRadiationDown_Average_W_m2=ifelse(Met$InfaredRadiationDown_Average_W_m2<200,NA,Met$InfaredRadiationDown_Average_W_m2)

##BP low outliers
Met$Flag_BP_Average_kPa=ifelse(Met$BP_Average_kPa<99,4,Met$Flag_BP_Average_kPa)
Met$Note_BP_Average_kPa=ifelse(Met$BP_Average_kPa<99,"Outlier set to NA",Met$Note_BP_Average_kPa)
Met$BP_Average_kPa=ifelse(Met$BP_Average_kPa<99,NA,Met$BP_Average_kPa)

## fix PAR_Tot
#Met$PAR_Total_mmol_m2=ifelse(Met$PAR_Total_mmol_m2>500, Met$PAR_Total_mmol_m2/1000, Met$PAR_Total_mmol_m2) #seeing if this makes PAR into reasonable values. 
#it does not..

Met$Flag_PAR_Total_mmol_m2=ifelse(Met$PAR_Total_mmol_m2>200, 4, Met$Flag_PAR_Total_mmol_m2)
#what should the note say?
Met$Note_PAR_Total_mmol_m2=ifelse(Met$PAR_Total_mmol_m2>200, "Outlier set to NA", Met$Note_PAR_Total_mmol_m2)
Met$PAR_Total_mmol_m2=ifelse(Met$PAR_Total_mmol_m2>200, NA, Met$PAR_Total_mmol_m2)

#Remove SW Rad outliers
Met$Flag_ShortwaveRadiationUp_Average_W_m2=ifelse(Met$ShortwaveRadiationUp_Average_W_m2>1600, 4, Met$Flag_ShortwaveRadiationUp_Average_W_m2)
Met$Note_ShortwaveRadiationUp_Average_W_m2=ifelse(Met$ShortwaveRadiationUp_Average_W_m2>1600, "Outlier set to NA", Met$Note_ShortwaveRadiationUp_Average_W_m2)
Met$ShortwaveRadiationUp_Average_W_m2=ifelse(Met$ShortwaveRadiationUp_Average_W_m2>1600, NA, Met$ShortwaveRadiationUp_Average_W_m2)

Met$Flag_ShortwaveRadiationDown_Average_W_m2=ifelse(Met$ShortwaveRadiationDown_Average_W_m2>300, 4, Met$Flag_ShortwaveRadiationDown_Average_W_m2)
Met$Note_ShortwaveRadiationDown_Average_W_m2=ifelse(Met$ShortwaveRadiationDown_Average_W_m2>300, "Outlier set to NA", Met$Note_ShortwaveRadiationDown_Average_W_m2)
Met$ShortwaveRadiationDown_Average_W_m2=ifelse(Met$ShortwaveRadiationDown_Average_W_m2>300, NA, Met$ShortwaveRadiationDown_Average_W_m2)

#fix albedo
Met$Flag_Albedo_Average_W_m2=ifelse(Met$Albedo_Average_W_m2>1000, 4, Met$Flag_Albedo_Average_W_m2)
Met$Note_Albedo_Average_W_m2=ifelse(Met$Albedo_Average_W_m2>1000, "Outliers set to NA", Met$Note_Albedo_Average_W_m2)
Met$Albedo_Average_W_m2=ifelse(Met$Albedo_Average_W_m2>1000, NA, Met$Albedo_Average_W_m2)

Met$Flag_Albedo_Average_W_m2=ifelse(is.na(Met$ShortwaveRadiationUp_Average_W_m2)|is.na(Met$ShortwaveRadiationDown_Average_W_m2), 4, Met$Flag_Albedo_Average_W_m2)
Met$Note_Albedo_Average_W_m2=ifelse(is.na(Met$ShortwaveRadiationUp_Average_W_m2)|is.na(Met$ShortwaveRadiationDown_Average_W_m2), "Set to NA because Shortwave = NA", Met$Note_Albedo_Average_W_m2)
Met$Albedo_Average_W_m2=ifelse(is.na(Met$ShortwaveRadiationUp_Average_W_m2)|is.na(Met$ShortwaveRadiationDown_Average_W_m2), NA, Met$Albedo_Average_W_m2)

#set flag 3
for(i in 5:17) { #for loop to create new columns in data frame
  Met[c(which(is.infinite(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-3 #puts in flag 3
  Met[c(which(is.infinite(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Infinite value set to NA" #note for flag 3
  Met[c(which(is.infinite(Met[,i]))),i] <- NA #set infinite vals to NA
  
  if(i!=8) { #flag 3 for negative values for everything except air temp
    Met[c(which((Met[,i]<0))),paste0("Flag_",colnames(Met[i]))] <- 3
    Met[c(which((Met[,i]<0))),paste0("Note_",colnames(Met[i]))] <- "Negative value set to 0"
    Met[c(which((Met[,i]<0))),i] <- 0 #replaces value with 0
  }
  if(i==9) { #flag for RH over 100
    Met[c(which((Met[,i]>100))),paste0("Flag_",colnames(Met[i]))] <- 3
    Met[c(which((Met[,i]>100))),paste0("Note_",colnames(Met[i]))] <- "Value set to 100"
    Met[c(which((Met[,i]>100))),i] <- 100 #replaces value with 100
  }
}
#create loop putting in maintenance flags 1 + 4
for(j in 1:nrow(RemoveMet)){
  # #if statement to only write in flag 4 if there are no other flags
  if(RemoveMet$flag[j]==4){
    Met[c(which(Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3]& (Met[,paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]==0))), paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[j]#same as above, but for notes
    Met[c(which(Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3] & (Met[,paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]==0))), paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]<-RemoveMet$flag[j]#when met timestamp is between remove timestamp
    #and met column derived from remove column
    #matching time frame, inserting flag
  }
  #if flag == 1, set parameter to NA, overwrites any other flag
  if(RemoveMet$flag[j]==1){
    Met[c(which((Met[,1]>=RemoveMet[j,2]) & (Met[,1]<=RemoveMet[j,3]))),paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))] <- RemoveMet$flag[j] #when met timestamp is between remove timestamp
    #and met column derived from remove column
    #matching time frame, inserting flag
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[j]#same as above, but for notes
    
    Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], colnames(Met[RemoveMet$colnumber[j]])] = NA
  } #replaces value of var with NA
}

#######Plots For Days ######
#plots to check for any wonkiness
x11(); par(mfrow=c(2,2))
plot(Met$DateTime, Met$CR3000_Batt_V, type = 'l')
plot(Met$DateTime, Met$CR3000Panel_temp_C, type = 'l')
#PAR
plot(Met_raw$DateTime, Met_raw$PAR_Average_umol_s_m2, col="red", type='l')
plot(Met$DateTime, Met$PAR_Average_umol_s_m2, type = 'l')
plot(Met_raw$DateTime, Met_raw$PAR_Total_mmol_m2, col="red", type='l')
plot(Met$DateTime, Met$PAR_Total_mmol_m2, type = 'l')
#BP
plot(Met_raw$DateTime, Met_raw$BP_Average_kPa, col="red", type='l')
plot(Met$DateTime, Met$BP_Average_kPa, type = 'l')
#Air Temp
plot(Met_raw$DateTime, Met_raw$AirTemp_Average_C, col="red", type='l')
points(Met$DateTime, Met$AirTemp_Average_C, type = 'l')
#RH
plot(Met_raw$DateTime, Met_raw$RH_percent, col="red", type='l')
points(Met$DateTime, Met$RH_percent, type = 'l')
#Rain
plot(Met_raw$DateTime, Met_raw$Rain_Total_mm, col="red", type='h')
points(Met$DateTime, Met$Rain_Total_mm, type = 'h')
#Wind
plot(Met$DateTime, Met$WindSpeed_Average_m_s, type = 'l')
hist(Met$WindDir_degrees)
#SW Rad
plot(Met_raw$DateTime, Met_raw$ShortwaveRadiationUp_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$ShortwaveRadiationUp_Average_W_m2, type = 'l')
plot(Met_raw$DateTime, Met_raw$ShortwaveRadiationDown_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$ShortwaveRadiationDown_Average_W_m2, type = 'l')
#Albedo
plot(Met_raw$DateTime, Met_raw$Albedo_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$Albedo_Average_W_m2, type = 'l')
#InfRad
plot(Met_raw$DateTime, Met_raw$InfaredRadiationUp_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$InfaredRadiationUp_Average_W_m2, type = 'l')
plot(Met_raw$DateTime, Met_raw$InfaredRadiationDown_Average_W_m2, col="red", type='l')
plot(Met$DateTime, Met$InfaredRadiationDown_Average_W_m2, type = 'l')


#prints table of flag frequency
for(i in 5:17) {
  print(colnames(Met[i]))
  print(table(Met[,paste0("Flag_",colnames(Met[i]))])) }

#Met unique values for notes
for (u in 20:45) {
  print(colnames(Met[u]))
  print(unique(Met[,u]))
}

Met_final=Met[,c(18:19,1:17, 20:45)] #final column order
setwd('./Data/DataNotYetUploadedtoEDI/Raw_Met/')
#write.csv(etc)


#BONUS CODE#
#justification for using panel temp lm from 2015 to substitue erroneous data
########### AirTemp lm ###########

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

####NDLAS comparison####
# NLDAS=read.csv("https://raw.githubusercontent.com/CareyLabVT/FCR-GLM/master/NLDASData/FCR_GLM_met_NLDAS2_Dec14_Dec18.csv", header = T)
# N_AirTemp=NLDAS[,c(6,3)]
# N_AirTemp$time=ymd_hms(N_AirTemp$time)
# Met_air=Met[,c(1,4,8)]
# names(Met_air)<- c("time", "Panel_temp","AirTemp_Average_C")
# #does NLDAS use GMT -4? or EST? What time zone is it??????????
# #for now, assuming GMT -4 for simplicity's sake
# compare<-merge(N_AirTemp, Met_air, by="time")
# 
# #Met Air vs. NLDAS
# x11()
# par(mfrow=c(1,2))
# plot(compare$time, compare$AirTemp_Average_C, ylim=c(-15,60))
# points(compare$time, compare$AirTemp, col="blue")
# legend("topleft", legend=c("MetStation","NLDAS"), col=c("black", "blue"), pch=1)
# plot(compare$AirTemp_Average_C,compare$AirTemp)
# abline(0,1, col="red")
# lm_NLDAS=lm(compare$AirTemp_Average_C ~ compare$AirTemp)
# abline(lm_NLDAS, col="blue")
# legend("bottomright", legend=c("1:1","lm"), col=c("red", "blue"), lty = 1)
# mean(lm_NLDAS$residuals)
# range(lm_NLDAS$residuals)
# sd(lm_NLDAS$residuals)
# 
# #Panel vs. Air
# x11()
# par(mfrow=c(1,2))
# plot(compare$time, compare$AirTemp_Average_C, ylim=c(-15,60))
# points(compare$time, compare$Panel_temp, col="green")
# legend("topleft", legend=c("MetStation AirTemp","MetStation Panel"), col=c("black", "green"), pch=1)
# plot(compare$Panel_temp,compare$AirTemp)
# abline(0,1, col="red")
# lm_Panel=lm(compare$AirTemp_Average_C ~ compare$Panel_temp)
# abline(lm_Panel, col="green")
# legend("bottomright", legend=c("1:1","lm"), col=c("red", "green"), lty = 1)
# mean(lm_Panel$residuals)
# range(lm_Panel$residuals)
# sd(lm_Panel$residuals)
# 
# #NLDAS vs. Panel??
# legend("topleft", legend=c("MetStation Panel","NLDAS"), col=c("green", "blue"), pch=1)
# plot(compare$Panel_temp,compare$AirTemp)
# abline(0,1, col="red")
# lm_Panel=lm(compare$AirTemp_Average_C ~ compare$Panel_temp)
# abline(lm_Panel, col="blue")
# 
# compare_2015=compare[compare$time<"2016-01-01 00:00:00",]
# 
# #Met Air vs. NLDAS 2015
# x11()
# par(mfrow=c(1,2))
# plot(compare_2015$time, compare_2015$AirTemp_Average_C, ylim=c(-15,60))
# points(compare_2015$time, compare_2015$AirTemp, col="blue")
# legend("topleft", legend=c("MetStation","NLDAS"), col=c("black", "blue"), pch=1)
# plot(compare_2015$AirTemp_Average_C,compare_2015$AirTemp)
# abline(0,1, col="red")
# lm_NLDAS=lm(compare_2015$AirTemp_Average_C ~ compare_2015$AirTemp)
# abline(lm_NLDAS, col="blue")
# legend("bottomright", legend=c("1:1","lm"), col=c("red", "blue"), lty = 1)
# mean(lm_NLDAS$residuals)
# range(lm_NLDAS$residuals)
# sd(lm_NLDAS$residuals)
# 
# #Panel vs. Air 2015
# x11()
# par(mfrow=c(1,2))
# plot(compare_2015$time, compare_2015$AirTemp_Average_C, ylim=c(-15,60))
# points(compare_2015$time, compare_2015$Panel_temp, col="green")
# legend("topleft", legend=c("MetStation AirTemp","MetStation Panel"), col=c("black", "green"), pch=1)
# plot(compare_2015$Panel_temp,compare_2015$AirTemp)
# abline(0,1, col="red")
# lm_Panel2015=lm(compare_2015$AirTemp_Average_C ~ compare_2015$Panel_temp)
# abline(lm_Panel, col="green")
# legend("bottomright", legend=c("1:1","lm"), col=c("red", "green"), lty = 1)
# mean(lm_Panel$residuals)
# range(lm_Panel$residuals)
# sd(lm_Panel$residuals)


#fitting sin curve
Data <- read.table(file="900days.txt", header=TRUE, sep="")
Met_1516=Met[year(Met$DateTime)>2017,]
Time <- yday(Met_1516$DateTime) 
infdown <- Met_1516$InfaredRadiationDown_Average_W_m2

xc<-cos(2*pi*Time/366)
xs<-sin(2*pi*Time/366)
fit.lm <- lm(infdown~xc+xs)

# access the fitted series (for plotting)
fit <- fitted(fit.lm)  

# find predictions for original time series
pred <- predict(fit.lm, newdata=data.frame(Time=Time))    

x11()
plot(infdown ~ Time, xlim=c(1, 900))
lines(fit, col="red")
lines(Time, pred, col="blue")

#How do I use this equation to correct the data? Having trouble figuring it out. 
Met$InfaredRadiationDown_Average_W_m2=ifelse((Met$InfaredRadiationDown_Average_W_m2 - (1.6278+(0.9008*Met$CR3000Panel_temp_C)))>(3*sd(lm_Panel2015$residuals)),(1.6278+(0.9008*Met$CR3000Panel_temp_C)), Met$InfaredRadiationDown_Average_W_m2)

#met_inf=
#if met_inf is outside range of lm during this doy, then correct based on this equation

