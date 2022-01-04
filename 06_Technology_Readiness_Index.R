#######################################################################################################################################

# Te

################################################################ Set Up ###############################################################

# Packages
library(data.table)
library(tidyr)
library(writexl)
library(xtable)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
load("./01_Input/00_clean_data_field.RData")


########################################## User Classification according to TRI 2.0 ##########################################################
# Cf. Parasuraman and Colby 2015

# Extracting relevant questions for TRI classification
quest_clean[.()]
