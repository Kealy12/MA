#############################################################################################################################

# Data Cleaning

########################################################### Set Up ##########################################################

#Loading packages 
library(data.table)
library(writexl)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Questionnaire
quest_raw_UK <- fread("./01_Input/raw_data_field_UK_2022_02_24.csv")

head(quest_raw_UK)

########################################################### Delete Obsolete Data ##########################################################

# unnecessary Columns: Quality always -77, lastpage always 6028435. external_lfdn always 0, tester always 0, duration irrelevant
quest_clean_UK <- quest_raw_UK[, c("quality", "lastpage", "external_lfdn", "tester", "duration") := NULL]

# uniqueness checks
quest_clean_UK[, unique(browser)]
dim(quest_clean_UK)

########################################################### Write to Excel ##########################################################

# Save
save(quest_clean_UK, file = "./01_Input/00_clean_data_field_UK.RData")

# Write to Excel 
sheets <- list("quest_clean_UK" = quest_clean_UK)
write_xlsx(sheets, "./01_Input/00_clean_data_field_UK.xlsx")

# Clean Environment
rm(list = ls())
gc()
