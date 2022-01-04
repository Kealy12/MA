#############################################################################################################################

# Data Cleaning

########################################################### Set Up ##########################################################

#Loading packages 
library(data.table)
library(writexl)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Questionnaire
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")


########################################################### Delete Obsolete Data ##########################################################

# Found 1 URL ID, which answered twice with different answers
quest_raw[duplicated(p_0001), p_0001]
duplicate <- quest_raw[p_0001 == "11C78052-F53A-4D93-B76D-E30968E61633"]

# unnecessary Columns: Quality always -77, lastpage always 6028435. external_lfdn always 0, tester always 0, duration irrelevant
quest_clean <- quest_raw[, c("quality", "lastpage", "external_lfdn", "tester", "duration") := NULL]

# uniqueness checks
quest_clean[, unique(browser)]

########################################################### Write to Excel ##########################################################

# Write to Excel 
sheets <- list("quest_clean" = quest_clean)
write_xlsx(sheets, "./../01_Input/00_clean_data_field.xlsx")

# Clean Environment
rm(list = ls())
gc()