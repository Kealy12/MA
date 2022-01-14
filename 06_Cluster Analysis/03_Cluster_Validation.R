#######################################################################################################################################

# Validating 4 Cluster Solution with Data from the questionnaire

################################################################ Set Up ###############################################################

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

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all.RData")
load("./../01_Input/01_RData/lca_4class_analysis.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/tri_segments_4C_summary.RData")


# Binding questionnaire to tri cluster data 
quest_tri_extended <- cbind(quest_clean, tri_comp_all)
quest_tri_extended

# Filter out respondents who had invalid answers on TRI questions, LCA only worked on non-NA answers
quest_tri_extended <- quest_tri_extended[complete.cases(quest_tri_extended)]

# Map clusters to remaining data
quest_tri_extended <- cbind(quest_tri_extended, tri_comp_all_noNA[, "Predicted Class (4Cs)"])
setnames(quest_tri_extended, "Predicted Class (4Cs)", "Cluster")

########################################## Validation of Clusters ##########################################################

#### Demographic Data ####
# Age Distribution, Gender, Mean Household Income per Month, Level of Education

quest_tri_extended
tri_segments_4C_summary



# Clean Environment
rm(list = ls())