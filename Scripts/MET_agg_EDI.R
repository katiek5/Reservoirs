###EDI MET STATION File
##Let's get the data together and looking good
#Probably should put this in a Git workflow friendly format.. oh well.

#packages needed
library("lubridate")
install.packages("tidyverse")
library(tidyverse)

setwd("/Users/bethany1/Desktop/MET_EDI/") #need to make into a relative path for EDI folder..

Met_past=read.csv("AllRawMetData_20181119.csv", sep = ",") #loads in data from FCR_GLM repository
Met_past$TIMESTAMP=ymd_hms(Met_past$TIMESTAMP, tz="Etc/GMT+4")

Met_now=read.csv("https://raw.githubusercontent.com/CareyLabVT/SCCData/carina-data/FCRmet.csv", skip = 4, header = F) #loads in data from SCC_data repository for latest push
if(length(names(Met_now))>17){ #removes NR01TK_Avg column, which was downloaded on some but not all days
  Met_now$V17<-NULL #remove extra column
}
#renames and reformats dataset for easy bind
names(Met_now) = c("TIMESTAMP","RECORD","BattV","PTemp_C","PAR_Den_Avg","PAR_Tot_Tot","BP_kPa_Avg","AirTC_Avg","RH","Rain_mm_Tot","WS_ms_Avg","WindDir","SR01Up_Avg","SR01Dn_Avg","IR01UpCo_Avg","IR01DnCo_Avg","Albedo_Avg")
Met_now$TIMESTAMP=ymd_hms(Met_now$TIMESTAMP, tz="Etc/GMT+4")
Met_now=Met_now[-c(which(is.na(Met_now$TIMESTAMP))),] #checks for NA from failed parse, and deletes from dataset
Met_now$PAR_Tot_Tot=as.numeric(Met_now$PAR_Tot_Tot)
#str(Met_now); str(Met_past) #checks structure for matching

Met_agg<-rbind(Met_past,Met_now) #binds past and current data from Met station

Met= Met_agg
Met$TIMESTAMP=ymd_hms(Met$TIMESTAMP, tz="Etc/GMT+4") #formats timestamp
#resulted in 1 failed parse

Met = Met[!duplicated(Met$TIMESTAMP),] #takes out duplicated values by timestamp

length(which(is.na(Met))) #check for rows that are only NA
Met=Met[-c(which(is.na(Met))),] #doesn't seem to actually work..

##Add rows
Met$Site=50 #add site
Met$Reservoir= "FCR"#add reservoir
Met$Flag= 0 #add flags
Met$Note = NA #add notes for flags
Met2=Met #Backup dataset to not run above lines of code
Met=Met2
###Time to do the hard part. Cleaning up the data.###

#check record
for(i in 2:length(Met$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(Met$RECORD[i]-Met$RECORD[i-1]>1){
    print(c(Met$TIMESTAMP[i-1],Met$TIMESTAMP[i]))
  }
}

###PAR

##Barometric Pressure
#remove negative values
Met$Flag=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$BP_kPa_Avg=ifelse(Met$BP_kPa_Avg < 0 & Met$Flag == 3, 0, Met$BP_kPa_Avg)

###AirTemp
#remove >

###Relative Humidity
#create if statement for flagging, need this to check if there are existing flags
if(Met$Flag>0 & Met$RH<0) {print("Flag overlap")} 
Met$Flag=ifelse(Met$RH < 0 & Met$Flag > 0, 5, Met$Flag)
Met$Flag=ifelse(Met$RH > 100 & Met$Flag > 0, 5, Met$Flag)
#need better solution to check flags..

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
#remove negative values
Met$Flag=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$Rain_mm_Tot=ifelse(Met$Rain_mm_Tot < 0 & Met$Flag == 3, 0, Met$Rain_mm_Tot)

###Wind speed + direction
#remove negative values
Met$Flag=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 0, 3, Met$Flag)
Met$Note=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, "Rainfall set to 0", Met$Note)
Met$WS_ms_Avg=ifelse(Met$WS_ms_Avg < 0 & Met$Flag == 3, 0, Met$WS_ms_Avg)

###Short wave radiation

###Long wave radiation

###Albedo???

###Remove maintenance datetime

#fix column names and order
Met=Met[,c(18:20,1:17)] #fixes order, does not fix names yet
names(Met_now) = c("Site", "Reservoir", "DateTime","Record") #fill out rest of col names when finalized
