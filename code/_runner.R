##########
# Install all required packages
# Runs all scripts required for complete SITT-MAT analysis
##########

# Clear environment
rm(list=ls())

#####################
# Setup environment #
#####################
renv::restore()

#############################################################
# Run all code in data pipeline (R files starting with a 0) #
#############################################################

files = list.files("code", pattern="^0\\d.+\\.R")

for(file in files){
  source(paste0(getwd(),"/code/",file))
}
