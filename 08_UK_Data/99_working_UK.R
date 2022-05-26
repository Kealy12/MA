# Packages
library(data.table)
library(dplyr)
library(tidyr)
library(writexl)
library(xtable)
library(apaTables)
library(ggplot2)
library(GGally)
library(poLCA)
library(RColorBrewer)
library(extrafont)
library(ggrepel)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
quest_raw <- fread("./01_Input/raw_data_field_UK_final.csv")
load("./01_Input/00_clean_data_field_UK.RData")

# AGE
# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
age_UK <- quest_clean_UK[, c("v_285")]
age_UK <- age_UK[, v_285 := v_285 + 14]
colnames(age_UK) <- c("Age_UK")
age_UK[, mean(Age_UK)]

age_UK[, "one" := ifelse(Age_UK>= 16 & Age_UK <=34, 1,0)]
age_UK[, "two" := ifelse(Age_UK>= 35 & Age_UK <=49, 1,0)]
age_UK[, "three" := ifelse(Age_UK>= 50 & Age_UK <=64, 1,0)]
age_UK[, "four" := ifelse(Age_UK>= 65 & Age_UK <=79, 1,0)]

age_UK[one == 1,.N ]
age_UK[two == 1,.N ]
age_UK[three == 1,.N ]
age_UK[four == 1,.N ]

# EDUCATION
# v_186
edu_UK <- quest_clean_UK[, c("v_186")]
table(edu_UK)

# EMPLOYMENT
# v_188
empl_UK <- quest_clean_UK[, c("v_188")]
table(empl_UK)


#### Clean Environment ####
rm(list = ls())


