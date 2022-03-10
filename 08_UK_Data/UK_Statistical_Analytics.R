#######################################################################################################################################

# Statistical Analysis of UK Data Set

############################################# Set Up ###############################################################

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
load("./01_Input/00_clean_data_field_UK.RData")
load("./../01_Input/01_RData/00_clean_data_field.RData")

# Loading Theme
source("./../04_Data_Prep/99_APA_Theme.R")

############################################# Usage Intention Analysis  ###############################################################

#### Usage intention GER ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_clean[v_132 == 0, v_132 := NA]
quest_clean[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_clean[, Usage_Intention_GER := round(rowMeans(quest_clean[, .(v_132, v_133)], na.rm = T), 2)]
usage_int_GER <- quest_clean[, .(Usage_Intention_GER)]

#### Usage intention UK ####
quest_clean_UK[v_132 == 0, v_132 := NA]
quest_clean_UK[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_clean_UK[, Usage_Intention_UK := round(rowMeans(quest_clean_UK[, .(v_132, v_133)], na.rm = T), 2)]
usage_int_UK <- quest_clean_UK[, .(Usage_Intention_UK)]

quest_clean_UK
quest_clean


#### T-Test ####
usage_test <- cbind(usage_int_GER, usage_int_UK)
usage_test <- melt(usage_test)
t.test(value ~ variable, data = usage_test, alternative = "two.sided" , var.equal=F)

# p-value < 0.05 -> Significant difference 










################################## Cleaning ###################################
rm(list = ls())



