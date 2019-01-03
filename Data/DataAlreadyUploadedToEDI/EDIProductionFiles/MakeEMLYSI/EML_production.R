# Steps for setting up EML metadata ####
install_github("EDIorg/EMLassemblyline", force=T)
library(EMLassemblyline)
library(devtools)
# Import templates for dataset licensed under CCBY, with 2 tables.
import_templates(path = "C:/Users/Owner/Dropbox/MakeEMLYSI",
                 license = "CCBY",
                 data.files = c("Secchi_depth",
                                "YSI_PAR_profiles"))

define_catvars(path = "C:/Users/Owner/Dropbox/MakeEMLYSI")
################################


# Run this function
make_eml(path = "C:/Users/Owner/Dropbox/MakeEMLYSI",
         dataset.title = "Secchi depth data and discrete depth profiles of photosynthetically active radiation, temperature, dissolved oxygen, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2018",
         data.files = c("Secchi_depth",
                        "YSI_PAR_profiles"),
         data.files.description = c("Secchi depth data from five reservoirs in southwestern Virginia", 
                                    "Discrete depths of water temperature, dissolved oxygen, conductivity, photosynthetically active radiation, redox potential, and pH in five southwestern Virginia reservoirs"),
         temporal.coverage = c("2013-08-30", "2018-12-17"),
         geographic.description = "Southwestern Virginia, USA, North America",
         maintenance.description = "ongoing", 
         affiliation = "EDI",
         user.id = "ccarey",
         package.id = "edi.198.6")

