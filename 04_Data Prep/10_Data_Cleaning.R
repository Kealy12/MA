#############################################################################################################################

# Data Cleaning

########################################################### Set Up ##########################################################

#Loading packages 
library(data.table)
library(tidyr)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Questionnaire
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")





# Write to Excel #
sheets <- list("xxx" = clean_questionnaire)
write_xlsx(sheets, "./../xx/xxxx.xlsx")

# Clean Environment
rm(list = ls())
gc()