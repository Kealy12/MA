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
                                     levels = c("Explorers", "Pioneers", "Hesitators", "Avoiders"))

summary(quest_tri_extended$Cluster)
########################################## Validation of Clusters ##########################################################
quest_tri_extended
tri_segments_4C_summary

table(quest_tri_extended$Cluster)
table(quest_tri_extended$v_169)

###### Demographic Data ######

# Gender
# v_169 -> Male = 1, Female = 2
f_c <- quest_tri_extended[v_169 == 2, .("Female (N)" = .N ), by = "Cluster"]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
demo_dt <- merge(f_c, n_c, by = "Cluster")
demo_dt[, "Female (%)" := round(`Female (N)` / `N`,2)][, `Female (N)` := NULL]
demo_dt

# Age
# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
quest_tri_extended[, v_285 := v_285 + 14]
quest_tri_extended[ v_285 == 16, .N]

# Mean Age of respondents 
mean(quest_tri_extended$v_285)

# % of people > 50
age50plus <- quest_tri_extended[v_285 >= 50, .("Age 50+ (N)" = .N ), by = "Cluster"]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
age_dt <- merge(age50plus, n_c, by = "Cluster")
age_dt[, "Age 50+ (%)" := round(`Age 50+ (N)` / `N`,2)][, `Age 50+ (N)` := NULL]
age_dt[, N := NULL]

demo_dt <- merge(demo_dt, age_dt, by = "Cluster", all.x = T)
demo_dt

# Minimum Bachelors Degree
# v_186
# If bachelor, master or phd - else 0
quest_tri_extended[, bach := ifelse(v_186 == 4 | v_186 == 5 | v_186 == 6, 1, 0)]
bach_degree <- quest_tri_extended[bach == 1, .("Minimum Bachelor's Degree (N)" = .N), by = Cluster]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
bach_dt <- merge(bach_degree, n_c, by = "Cluster")
bach_dt[, "Minimum Bachelor's Degree (%)" := round(`Minimum Bachelor's Degree (N)` / `N`,2)][, `Minimum Bachelor's Degree (N)` := NULL]
bach_dt[, N := NULL]

demo_dt <- merge(demo_dt, bach_dt, by = "Cluster", all.x = T)
demo_dt

# More TBD

###### Blockchain Tech Characteristics ######


# Mean Household Income per Month, Level of Education

# Clean Environment
rm(list = ls())