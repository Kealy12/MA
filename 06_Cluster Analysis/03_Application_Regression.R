#######################################################################################################################################

# Regressing LCA-based clusters on blockchain applications

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
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/lca_5class_analysis.RData")
load("./../01_Input/01_RData/lca_4class_analysis.RData")

################################################################ Mapping TRI data back to questionnaire ###############################################################
quest_tri_extended <- cbind(quest_clean, tri_comp_all)
quest_tri_extended

########## SANITY CHECKS ##########
# 0s on TRI answers in questionnaire need to be NA (they are invalid)
quest_tri_extended[v_108 == 0, v_108 := NA]
quest_tri_extended[v_109 == 0, v_109 := NA]
quest_tri_extended[v_207 == 0, v_207 := NA]
quest_tri_extended[v_208 == 0, v_208 := NA]
quest_tri_extended[v_209 == 0, v_209 := NA]
quest_tri_extended[v_228 == 0, v_228 := NA]
quest_tri_extended[v_229 == 0, v_229 := NA]
quest_tri_extended[v_230 == 0, v_230 := NA]
quest_tri_extended[v_231 == 0, v_231 := NA]
quest_tri_extended[v_232 == 0, v_232 := NA]

mean(quest_tri_extended$v_209, na.rm = T)
mean(quest_tri_extended$INN4, na.rm = T)
sum(is.na(quest_tri_extended)) # Total of 28 NAs
rownames(quest_tri_extended)[!complete.cases(quest_tri_extended)] # Indices of rows with NAs
quest_tri_extended[rowSums(is.na(quest_tri_extended)) > 0] # Overview of rows with NA

########### LCA Mapping #############
# Filter out respondents who had invalid answers on TRI questions
# LCA only worked on non-NA answers
quest_tri_extended <- quest_tri_extended[complete.cases(quest_tri_extended)]

# Map clusters to remaining data
quest_tri_extended <- cbind(quest_tri_extended, tri_comp_all_noNA[, "Predicted Class (4Cs)"], 
                                tri_comp_all_noNA[, "Predicted Class (5Cs)"])


################################################################ Extracting blockchain applications ###############################################################

# need to encode clusters as factors
quest_tri_extended$`Predicted Class (5Cs)` <- as.factor(quest_tri_extended$`Predicted Class (5Cs)`)
quest_tri_extended$`Predicted Class (4Cs)` <- as.factor(quest_tri_extended$`Predicted Class (4Cs)`)

setnames(quest_tri_extended, "Predicted Class (5Cs)", "Predicted_Class_5C")
setnames(quest_tri_extended, "Predicted Class (4Cs)", "Predicted_Class_4C")

quest_tri_extended

##### 01 Tokenization of Assets
# v_265

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_265)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Asset Tokenization")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_265)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Asset Tokenization")





################################################################ Regressions ###############################################################




# Clean Environment
rm(list = ls())



