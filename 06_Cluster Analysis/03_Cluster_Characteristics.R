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
########################################## Cluster-Demographic Characteristics ##########################################################
quest_tri_extended
tri_segments_4C_summary

table(quest_tri_extended$Cluster)
table(quest_tri_extended$v_169)

###### Gender ######
# v_169 -> Male = 1, Female = 2
f_c <- quest_tri_extended[v_169 == 2, .("Female (N)" = .N ), by = "Cluster"]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
cluster_demo_dt <- merge(f_c, n_c, by = "Cluster")
cluster_demo_dt[, "Female (%)" := round(`Female (N)` / `N`,2)][, `Female (N)` := NULL]
cluster_demo_dt

#### Age ####
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

cluster_demo_dt <- merge(cluster_demo_dt, age_dt, by = "Cluster", all.x = T)
cluster_demo_dt

##### Minimum Bachelors Degree ####
# v_186
# If bachelor, master or phd - else 0
quest_tri_extended[, bach := ifelse(v_186 == 4 | v_186 == 5 | v_186 == 6, 1, 0)]
bach_degree <- quest_tri_extended[bach == 1, .("Minimum Bachelor's Degree (N)" = .N), by = Cluster]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
bach_dt <- merge(bach_degree, n_c, by = "Cluster")
bach_dt[, "Minimum Bachelor's Degree (%)" := round(`Minimum Bachelor's Degree (N)` / `N`,2)][, `Minimum Bachelor's Degree (N)` := NULL]
bach_dt[, N := NULL]

cluster_demo_dt <- merge(cluster_demo_dt, bach_dt, by = "Cluster", all.x = T)
cluster_demo_dt

#### More TBD ####


########################################## Cluster-Blockchain Characteristics ##########################################################

#### Knowledge of Blockchain ####
# v_286 (1-10 scale)

#Overall: Before and after
quest_tri_extended[, .(Mean = mean(v_286, na.rm = T))]
quest_tri_extended[, .(Mean = mean(v_333, na.rm = T))]

# Cluster specific, before and after
quest_tri_extended[, .(Mean = mean(v_286, na.rm = T)), by = Cluster]
quest_tri_extended[, .(Mean = mean(v_333, na.rm = T)), by = Cluster]

cluster_bc_dt <- quest_tri_extended[, .("Knowledge of Blockchain Technology (1-10)" = round(mean(v_286, na.rm = T),2)), by = Cluster][order(Cluster)]
cluster_bc_dt

####  Ability to Explain Blockchain ####
# v_50 (1-10 scale)
explain <- quest_tri_extended[, .("Ability to Explain Blockchain Technology (1-10)" = round(mean(v_50, na.rm = T),2)), by = Cluster]

cluster_bc_dt <- merge(cluster_bc_dt, explain, by = "Cluster", all.x = T)
cluster_bc_dt

####  Ability to Explain the Internet ####
# v_49 (1-10 scale)
internet <- quest_tri_extended[, .("Ability to Explain the Internet (1-10)" = round(mean(v_49, na.rm = T),2)), by = Cluster]

cluster_bc_dt <- merge(cluster_bc_dt, internet, by = "Cluster", all.x = T)
cluster_bc_dt

# Only need one of those

#### Possession of any cryptocurrency ####
# v_54 (1 = Yes, 2 = No)

crypto_dt <- quest_tri_extended[ v_54 == 1, .("Possession of Cryptocurrency (N)" = .N), by = Cluster ]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
crypto_dt <- merge(crypto_dt, n_c, by = "Cluster")
crypto_dt[, "Possession of Cryptocurrency (%)" := round(`Possession of Cryptocurrency (N)` / `N`,2)][, `Possession of Cryptocurrency (N)` := NULL]
crypto_dt[, N := NULL]

cluster_bc_dt <- merge(cluster_bc_dt, crypto_dt, by = "Cluster", all.x = T)
cluster_bc_dt

#### Possession of a NFT ####
# v_331 (1 = Yes, 2 = No, -77 = missing value (conditional question, if person heard of NFT))

nft_dt <- quest_tri_extended[ v_331 == 1, .("Possession of NFT (N)" = .N), by = Cluster ]
n_c <- quest_tri_extended[, .(N = .N), by = "Cluster"]
nft_dt <- merge(nft_dt, n_c, by = "Cluster")
nft_dt[, "Possession of NFT (%)" := round(`Possession of NFT (N)` / `N`,2)][, `Possession of NFT (N)` := NULL]
nft_dt[, N := NULL]

cluster_bc_dt <- merge(cluster_bc_dt, nft_dt, by = "Cluster", all.x = T)
cluster_bc_dt

quest_tri_extended[, .N, by =v_331]




# Clean Environment
rm(list = ls())

