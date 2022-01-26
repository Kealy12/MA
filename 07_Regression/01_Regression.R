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

#### 1. Have you heard of blockchain ####
# (yes = 1,no = 0): v_321
quest_reg <- quest_reg[v_321 == 2, v_321 := 0]
quest_reg[, .(v_321)]

#### 2. TRI: Overall TRI (1-7) ####
quest_reg[, .(`Overall TRI`)]

#### 3. Knowledge of difference between Bitcoin and Blockchain  ####
# (yes = 1, no = 0): v_282
quest_reg <- quest_reg[v_282 == 2, v_282 := 0]
table(quest_reg[, .(v_282)])

#### 4. Knowledge of BT  ####
# (1-10 scale): v_286
quest_reg[, .(v_286)]

#### 5. Possession of Crypto ####
# v_54: (yes = 1, no = 0)
quest_reg <- quest_reg[v_54 == 2, v_54 := 0]
table(quest_reg[, .(v_54)])

#### 6. Disposition to privacy ####
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be 0
quest_reg[v_104 == 0, v_104 := NA]
quest_reg[v_105 == 0, v_105 := NA]
quest_reg[v_106 == 0, v_106 := NA]

# Reverse code v_106
quest_reg[, Reverse_v_106 := 8 - v_106]

# Calculate Average 
quest_reg[, DISPPRIV := round(rowMeans(quest_reg[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]

# Score overall
quest_reg[, .("Privacy score overall" = round(mean(DISPPRIV, na.rm = T),2))]






#### 7. Usage Intention ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_reg[v_132 == 0, v_132 := NA]
quest_reg[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_reg[, Usage_Int := round(rowMeans(quest_reg[, .(v_132, v_133)], na.rm = T), 2)]

# Score overall
quest_reg[, .("Usage intention score overall" = round(mean(Usage_Int, na.rm = T),2))]





#### 8. Disposition to trust ####






#### Testing Regression ####
lm_model <- lm(v_267 ~ v_321, data = quest_reg)
coefficients(lm_model)
summary(lm_model)
ggplot(quest_reg, aes(v_321, v_267)) + geom_jitter() + geom_smooth(method = "lm")









###### Save and Clean ####

# Clean Environment
rm(list = ls())

