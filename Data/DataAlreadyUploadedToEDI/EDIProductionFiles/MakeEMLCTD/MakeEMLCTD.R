# Install and load devtools
install.packages("devtools")
library(devtools)

# Install and load EMLassemblyline
install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Import Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import_templates(path = "C:/Users/Owner/Dropbox/MakeEMLCTD", 
                 license = "CCBY", 
                 data.files = "CTD_Meta_13_18_final.csv")

# Define Categorical Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
define_catvars(path = "C:/Users/Owner/Dropbox/MakeEMLCTD")

# Make the EML for EDI ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_eml(path = "C:/Users/Owner/Dropbox/MakeEMLCTD",
         dataset.title = "Time series of high-frequency profiles of depth, temperature, dissolved oxygen, conductivity, specific conductivity, chlorophyll a, turbidity, pH, oxidation-reduction potential, photosynthetic active radiation, and descent rate for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in Southwestern Virginia, USA 2013-2018",
         data.files = "CTD_Meta_13_18_final.csv",
         data.files.description = "Reservoir CTD dataset",
         data.files.quote.character = "\"",
         temporal.coverage = c("2013-03-07", "2018-12-17"),
         maintenance.description = "ongoing",
         affiliation = "EDI",
         user.id = "ccarey",
         package.id = "edi.200.6")
