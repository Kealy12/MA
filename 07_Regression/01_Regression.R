#######################################################################################################################################

# Regression on Application Scores 

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
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/01_quest_cluster_extended.RData")
load("./../01_Input/01_RData/tri_all.RData")

# Loading Theme
source("./../04_Data_Prep/99_APA_Theme.R")

# Binding questionnaire to TRI data
quest_reg <- cbind(quest_clean, tri_comp_all)
quest_reg

# Extract Application columns
app_scores <- quest_reg[, .(v_265, v_266, v_267, v_268, v_269, v_270)]

############################################# Extracting relevant predictors ###############################################################

# 1. Have you heard of blockchain (yes = 1,no = 0): v_321
quest_reg <- quest_reg[v_321 == 2, v_321 := 0]
quest_reg[, .(v_321)]

# 2. TRI: Overall TRI (1-7)
quest_reg[, .(`Overall TRI`)]

# 3. Knowledge of difference between Bitcoin and Blockchain (yes = 1, no = 0): v_282
quest_reg <- quest_reg[v_282 == 2, v_282 := 0]
quest_reg[, .(v_282)]

# 4. Knowledge of BT (1-10 scale): v_286
quest_reg[, .(v_286)]

# 5. Possession of Crypto (yes = 1, no = 0)
quest_reg <- quest_reg[v_54 == 2, v_54 := 0]
quest_reg[, .(v_54)]





#### Testing Regression ####
lm_model <- lm(v_265 ~ v_321, data = quest_reg)
coefficients(lm_model)
summary(lm_model)
ggplot(quest_reg, aes(v_321, v_265)) + geom_jitter() + geom_smooth(method = "lm")









###### Save and Clean ####

# Clean Environment
rm(list = ls())

