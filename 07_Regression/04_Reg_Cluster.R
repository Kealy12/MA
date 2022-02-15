#######################################################################################################################################

# Multivariate Linear Regression Analysis - Testing on Clusters

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
library(lavaan)
library(psych)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/01_quest_cluster_extended.RData")
load("./../01_Input/01_RData/tri_all.RData")

# Loading Theme
source("./../04_Data_Prep/99_APA_Theme.R")

# Binding questionnaire to TRI data
quest_reg <- cbind(quest_clean, tri_comp_all)
colnames(quest_reg)[222] <- c("Overall_TRI")
colnames(quest_reg)[216] <- c("Optimism")
colnames(quest_reg)[217] <- c("Innovativeness")
colnames(quest_reg)[218] <- c("Discomfort")
colnames(quest_reg)[219] <- c("Insecurity")

# Extract Application columns
app_scores <- quest_reg[, .(v_265, v_266, v_267, v_268, v_269, v_270)]

dim(quest_tri_extended)



############################################# CLUSTER TEST #############################################

# Base table
dim(quest_tri_extended)

# extend base table with additional columns from predictors
dim(quest_reg)

clus_table <- merge(quest_tri_extended, quest_reg, by.x = "lfdn", by.y = "lfdn", all.x = T)
dim(clus_table)
clus_table[, grep(pattern=".y$", colnames(clus_table)) := NULL]
clus_table

# Separate Cluster Indicators to 0 and 1
clus_table <- dcast(clus_table, ... ~ Cluster,  value.var = "Cluster")

# Convert Factor into character
clus_table$Explorers <- as.character(clus_table$Explorers)
clus_table$Pioneers <- as.character(clus_table$Pioneers)
clus_table$Hesitators <- as.character(clus_table$Hesitators)
clus_table$Avoiders <- as.character(clus_table$Avoiders)

# Assign 0 and 1
clus_table[!is.na(Explorers), Explorers := "1"]
clus_table[is.na(Explorers), Explorers := "0"]
clus_table[!is.na(Avoiders), Avoiders := "1"]
clus_table[is.na(Avoiders), Avoiders := "0"]
clus_table[!is.na(Hesitators), Hesitators := "1"]
clus_table[is.na(Hesitators), Hesitators := "0"]
clus_table[!is.na(Pioneers), Pioneers := "1"]
clus_table[is.na(Pioneers), Pioneers := "0"]

# Back to Factor
clus_table$Explorers <- as.numeric(clus_table$Explorers)
clus_table$Pioneers <- as.numeric(clus_table$Pioneers)
clus_table$Hesitators <- as.numeric(clus_table$Hesitators)
clus_table$Avoiders <- as.numeric(clus_table$Avoiders)

# Extracting Predictors
# Have you heard of BT #
# (yes = 1,no = 0): v_321
clus_table <- clus_table[v_321.x == 2, v_321.x := 0]
pc1 <- clus_table[, .(v_321.x)]
colnames(pc1) <- c("Heard_of_BT")

# Contact with BT
# v_10: in professional life
# v_11: in personal life
contact_c<- clus_table[, c("v_10.x", "v_11.x")]
colnames(contact_c) <- c("Contact_in_professional_life", "Contact_in_personal_life")

# Knowledge of BT
# (1-10 scale): v_286
pc2 <- clus_table[, .(v_286.x)]
colnames(pc2) <- c("Knowledge_of_BT")

# Possession of Crypto
# v_54: (yes = 1, no = 0)
clus_table <- clus_table[v_54.x == 2, v_54.x := 0]
table(clus_table[, .(v_54.x)])
pc3 <- clus_table[, .(v_54.x)]
colnames(pc3) <- c("Possess_Crypto")

# Social influence on blockchain usage
# v_296
# Scale 1 (they never heard of it) - 10 (they are experts)
social_c <- clus_table[, .(v_296.x)]
colnames(social_c) <- c("Social_Influence")
social_c[, .("Social influence on my blockchain usage (1-10)" = round(mean(Social_Influence, na.rm = T),2))]
# They would rather discourage me


# TRI
clus_table
clus_table[, .(`Overall_TRI`)] # using single items for CFA
pc4 <- clus_table[, .(`Overall_TRI`)]

optimism_c <- clus_table[, .(Optimism)]
innovativeness_c <- clus_table[, .(Innovativeness)]
discomfort_c <- clus_table[, .(Discomfort)]
insecurity_c <- clus_table[, .(`Insecurity (INS)`)]
colnames(insecurity_c) <- c("Insecurity")

explorers <- clus_table[, .(Explorers)]
pioneers <- clus_table[, .(Pioneers)]
hesitators <- clus_table[, .(Hesitators)]
avoiders <- clus_table[, .(Avoiders)]

# Disposition to privacy
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be 0
clus_table[v_104.x == 0, v_104.x := NA]
clus_table[v_105.x == 0, v_105.x := NA]
clus_table[v_106.x == 0, v_106.x := NA]

pc5 <- clus_table[, .(DISPPRIV)]


# Usage Intention
# v_132, v_133: Likert (1-7)
# can be no 0s
clus_table[v_132.x == 0, v_132.x := NA]
clus_table[v_133.x == 0, v_133.x := NA]

pc6 <- clus_table[, .(Usage_Int)]


# Trust in blockchain technology
pc7 <- clus_table[, .(Trust_in_BT)]

# Perceived benefit for society
pc8 <- clus_table[, .(Perc_Benefit)]

# Perceived Risk
pc9 <- clus_table[, .(Perc_Risk)]

# Potential of disruption

pc10 <- clus_table[, .(Pot_Dis)]
# Test
# a <- "2910"
# contain <- function(searchValue, table){
#   for(i in 1:nrow(table)){
#     id <- table[i, c(1)]$lfdn
#     if(id ==a) return(T)
#   }
#   return(F)
# }
# contain(a, quest_tri_extended)
#
#
# for(i in 1:nrow(quest_tri_extended)){
#   id <- quest_tri_extended[i, c(1)]$lfdn
#   if(contain(id, quest_reg) == T){
#     get <- quest_reg$Usage_Int[id]
#     quest_tri_extended[, addedColum := get]
#   }else print("not here")
# }
#
# uniqueN(quest_tri_extended$lfdn)
# uniqueN(quest_reg$lfdn)



# Correlations
clus_table
# Omitted Posession of Crypto as only 17 predictors can be shown
predictors_clus <- cbind(pc2,pc3, contact_c,social_c, optimism_c, innovativeness_c, discomfort_c, insecurity_c, explorers, pioneers, hesitators, avoiders, pc5,pc6,pc7,pc8,pc9,pc10)
cor_clus <- apa.cor.table(predictors_clus, show.conf.interval = F)
cor_clus
cor(predictors_clus)

############################################# Regression ###############################################################
indep_var_clus <-   c("Knowledge_of_BT + Possess_Crypto + Social_Influence + Contact_in_professional_life + Contact_in_personal_life + Optimism + Innovativeness + Discomfort + Insecurity + Explorers + Pioneers + Hesitators + Avoiders + DISPPRIV + Usage_Int + Trust_in_BT + Perc_Risk + Perc_Benefit + Pot_Dis")


token_pred_clus <- cbind(predictors_clus, clus_table[, .(v_265.x)])
outcome_1 <- c("v_265.x")
f1 <- as.formula(paste(outcome_1, paste(indep_var_clus, collapse = " + "), sep = " ~ "))
lm_token_clus <- lm(f1, data = token_pred_clus)

apa.reg.table(lm_token_clus)
car::vif(lm_token_clus)

alias(lm_token_clus)

###### Save and Clean ####

# Clean Environment
rm(list = ls())

