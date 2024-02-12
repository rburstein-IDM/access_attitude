## Roy B
## Run all scripts for Manuscript 
##"Drivers of Zero-Dose Childhood Vaccination Status in the Democratic Republic of Congo: Quantifying the Relative Impact of Geographic Accessibility and Attitudes toward Vaccination"


# Set working Directory
if(Sys.info()[['user']]=='royb') 
  setwd('C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/code/ecv_access_intent_manu')

# Set up environment
source('0_init.R')

# load and clean data
source('1_prep_data.R')

# pull numbers, tables, and plots
source('2_descriptives_and_plots.R')

# run regressions
source('3_regressions.R')

