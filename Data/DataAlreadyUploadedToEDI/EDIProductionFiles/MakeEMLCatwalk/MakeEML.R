# Install devtools
#install.packages("devtools")

# Load devtools
library(devtools)
library(tidyverse)
library(lubridate)


# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)
wd <- setwd("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk")
cat <- read.csv("./Catwalk_cleanedEDI.csv")

# Import templates for an example dataset licensed under CC0, with 2 tables located in at "path"
#import_templates(path = "C:/Users/wwoel/Desktop/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk",
#                license = "CCBY",
#                 data.files = "Catwalk_cleanedEDI")

view_unit_dictionary()
define_catvars("C:/Users/wwoel/Desktop/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk")

make_eml(path = "C:/Users/wwoel/Desktop/Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLCatwalk",
         dataset.title = "Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, conductivity, specific conductivity, total dissolved solids, chlorophyll a, phycocyanin, and fluorescent dissolved organic matter at discrete depths in Falling Creek Reservoir, Virginia, USA in 2018",
         data.files = "Catwalk_cleanedEDI",
         data.files.description = "Catwalk Sensor String",
         temporal.coverage = c("2018-07-05", "2018-12-18"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "ongoing",
         user.id = c("carylab1", "ccarey"),
         package.id = "edi.271.2", #### this is the one that I need to change!!!
         affiliation = c("EDI", "EDI"))


