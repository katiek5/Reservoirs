###EDI MET STATION File

##Tasks/Questions Left:
#1. Relative paths + Uploading Large Files to Github
#2. Flag 1 & 4
#3. Airtemp correction + flagging
#4. Finalize data

###packages needed
library("lubridate")
##install.packages("lubridate"); install.packages("tidyverse")
library(tidyverse)

###Loading in 3 datasets and aggregating: 
#a. Past Met data, manual downloads
setwd("/Users/bethany1/Desktop/MET_EDI/") #need to make into a relative path for EDI folder..

#ADD IN FCR MET COMPILE SCRIPT
Met_past=read.csv("AllRawMetData_20181119.csv", sep = ",") #loads in data from FCR_GLM repository
Met_past$TIMESTAMP=ymd_hms(Met_past$TIMESTAMP, tz="Etc/GMT+4")

# manualMet<-"https://github.com/CareyLabVT/FCR-GLM/tree/master/MetStationData/"
# 
# metfiles<-list.files(path='https://github.com/CareyLabVT/FCR-GLM/tree/master/MetStationData/') #creates a list of all met station files within the working directory
# #sorted automatically by date. All files in here should be of the format:
# #"CR3000_FCRmet_YYYYMMDD.dat"
# 
# obs<-read.csv(file=metfiles[1],skip=4,header=FALSE) #read in first file
# names(obs) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
# units = c("TS","RN","Volts","Deg C","umol/s/m^2","mmol/m^2","kPa","Deg C","%","mm","meters/second","degrees","W/m^2","W/m^2","W/m^2","W/m^2","W/m^2") #creates list of units, skipped in line above
# obs$TIMESTAMP = ymd_hms(obs$TIMESTAMP, tz="Etc/GMT+4") #CCC what do you think of this?
# 
# for(i in 2:length(metfiles)){ #reads in all files within folder in Github
#   temp<-read.csv(file=metfiles[i],skip=4,header=FALSE)
#   if(length(names(temp))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
#     temp$V17<-NULL #remove extra column
#   }
#   names(temp) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
#   temp$TIMESTAMP = ymd_hms(temp$TIMESTAMP, tz="Etc/GMT+4") #NOTE TO BETHANY to add lubridate here!!! #cayelan, how is this?
#   obs<-rbind(obs,temp)
#   #print(i)
# }


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

#EDI Column names
names(Met) = c("DateTime","Record", "CR3000_Batt_V", "CR3000Panel_temp_C", 
               "PAR_Average_umol_s_m2", "PAR_Total_mmol_m2", "BP_Average_kPa", "AirTemp_Average_C", 
               "RH_percent", "Rain_Total_mm", "WindSpeed_Average_m_s", "WindDir_degrees", "ShortwaveRadiationUp_Average_W_m2",
               "ShortwaveRadiationDown_Average_W_m2", "InfaredRadiationUp_Average_W_m2",
               "InfaredRadiationDown_Average_W_m2", "Albedo_Average_W_m2") #finalized column names
Met$Site=50 #add site
Met$Reservoir= "FCR"#add reservoir


#c. load in maintenance txt file
RemoveMet=read.table("https://raw.githubusercontent.com/CareyLabVT/SCCData/carina-data/MET_MaintenanceLog.txt", sep = ",", header = T)
#str(RemoveMet)
RemoveMet$TIMESTAMP_start=ymd_hms(RemoveMet$TIMESTAMP_start, tz="Etc/GMT+4") #setting time zone
RemoveMet$TIMESTAMP_end=ymd_hms(RemoveMet$TIMESTAMP_end, tz="Etc/GMT+4") #setting time zone

#### Flag creation ####
#create flag + notes columns for data columns c(5:17)
#set 2 + 3 flags

for(i in 5:17) { #for loop to create new columns in data frame
  Met[,paste0("Flag_",colnames(Met[i]))] <- 0 #creates flag column + name of variable
  Met[,paste0("Note_",colnames(Met[i]))] <- NA #creates note column + names of variable
  Met[c(which(is.na(Met[,i]))),paste0("Flag_",colnames(Met[i]))] <-2 #puts in flag 2
  Met[c(which(is.na(Met[,i]))),paste0("Note_",colnames(Met[i]))] <- "Sample not collected" #note for flag 2
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
  #create table of flag frequency
  print(colnames(Met[i]))
  print(table(Met[,paste0("Flag_",colnames(Met[i]))]))
}

#create loop putting in maintenance flags 1 + 4
for(j in 1:nrow(RemoveMet)){
  # #if statement to only write in flag 4 if there are no other flags
  # if(RemoveMet$flag[j]==4){
  #   if(Met[,paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))]==0){
  # Met[c(which(Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3])), #when met timestamp is between remove timestamp
  #     paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))] #and met column derived from remove column
  #      <-RemoveMet$flag[j] #matching time frame, inserting flag
  # Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], #same as above, but for notes
  #           paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[i] 
  # }}
  #if flag == 1, set parameter to NA, overwrites any other flag
  #if(RemoveMet$flag[j]==1)
    Met[c(which((Met[,1]>=RemoveMet[j,2]) & (Met[,1]<=RemoveMet[j,3]))), #when met timestamp is between remove timestamp
         paste0("Flag_",colnames(Met[RemoveMet$colnumber[j]]))] #and met column derived from remove column
    <- RemoveMet$flag[j] #matching time frame, inserting flag
  # Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], #same as above, but for notes
  #     paste0("Note_",colnames(Met[RemoveMet$colnumber[j]]))]=RemoveMet$notes[i] 
  # Met[Met[,1]>=RemoveMet[j,2] & Met[,1]<=RemoveMet[j,3], colnames(Met[RemoveMet$colnumber[j]])] = NA
  #} #replaces value of var with NA
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



#######Plots For Days ######
#plots to check for any wonkiness
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

Met_final=Met[,c(18:19,1:17, 20:45)] #final column order
#write.csv(etc)
