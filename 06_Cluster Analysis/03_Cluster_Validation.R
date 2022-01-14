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

# Setting names
quest_tri_extended$Cluster <- as.character(quest_tri_extended$Cluster)
quest_tri_extended[ Cluster == "3", Cluster := "Explorers"]
quest_tri_extended[ Cluster == "4", Cluster := "Hesitators"]
quest_tri_extended[ Cluster == "2", Cluster := "Avoiders"]
quest_tri_extended[ Cluster == "1", Cluster := "Pioneers"]

# Setting Cluster as Factor and Levels corresponding Overall TRI
quest_tri_extended$Cluster <- as.factor(quest_tri_extended$Cluster)
quest_tri_extended$Cluster <- factor(quest_tri_extended$Cluster , 
                                     levels = c("Avoiders", "Hesitators", "Pioneers", "Explorers"))

summary(quest_tri_extended$Cluster)
########################################## Validation of Clusters ##########################################################
quest_tri_extended
tri_segments_4C_summary

table(quest_tri_extended$v_169)

#### Demographic Data ####
# Gender, Age, Mean Household Income per Month, Level of Education
# Gender: v_169 -> Male = 1, Female = 2
f_c <- quest_tri_extended[v_169 == 2, .("Female (N)" = .N ), by = "Cluster"]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]

demo <- cbind(f_c, n_c)
demo[, 3 := NULL]
demo[, "Female (%)" := round(`Female (N)` / `N`,2)][, `Female (N)` := NULL]
demo

# Age: v_285

# Clean Environment
rm(list = ls())