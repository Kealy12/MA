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
library(lavaan)

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

# Extract Application columns
app_scores <- quest_reg[, .(v_265, v_266, v_267, v_268, v_269, v_270)]

############################################# Extracting relevant predictors ###############################################################

#### 1. Have you heard of blockchain ####
# (yes = 1,no = 0): v_321
quest_reg <- quest_reg[v_321 == 2, v_321 := 0]
quest_reg[, .(v_321)]

#### 2. TRI: Overall TRI (1-7) ####
quest_reg[, .(`Overall_TRI`)]

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





#### 8. Disposition to trust 
#### 9. Perceived benefit for society
#### 10. Perceived Risk ####

# v_149, v_150: Likert (1-7)
# can be no 0s
quest_reg[v_149 == 0, v_149 := NA]
quest_reg[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_reg[, Perc_Risk := round(rowMeans(quest_reg[, .(v_149, v_150)], na.rm = T), 2)]

# Score overall
quest_reg[, .("Perceived risk score overall" = round(mean(Perc_Risk, na.rm = T),2))]





#### 11. Potential of disruption ####

# v_151, v_152, v_153, v_154 (reverse coded): Likert (1-7)
# can be no 0s
quest_reg[v_151 == 0, v_151 := NA]
quest_reg[v_152 == 0, v_152 := NA]
quest_reg[v_153 == 0, v_153 := NA]
quest_reg[v_154 == 0, v_154 := NA]

# reverse code v_154
quest_reg[, Reverse_v_154 := 8 - v_154]

# Calculate Average 
quest_reg[, Pot_Dis := round(rowMeans(quest_reg[, .(v_151, v_152, v_153, Reverse_v_154)], na.rm = T), 2)]

# Score overall
quest_reg[, .("Potential of disruption score overall" = round(mean(Pot_Dis, na.rm = T),2))]







#### TBD ####
quest_reg

############################################# Performing CFA ###############################################################
factors <- '
Dis_Privacy =~ v_104 + v_105 + Reverse_v_106
Use_Intent =~ v_132 + v_133
integrity =~ v_247 + v_248 + v_249
benevolence =~ v_250 + v_251 + v_252
ability =~ v_144 + v_145 + v_146
Dis_Trust =~ integrity + benevolence + ability
Perceived_Risk =~ v_149 + v_150
Potential_Disruption =~ v_151 + v_152 + v_153 + Reverse_v_154'

# Heard_BT =~ v_321
# TRI =~ Overall_TRI
# Know_Diff =~ v_282
# Know_BT =~ v_286
# Poss_Crypto =~ v_54'

cfa <- cfa(factors, quest_reg, std.lv=TRUE)
summary(cfa, standardized=TRUE, fit.measures = T)

semPlot::semPaths(cfa, "std")

apa.cor.table()


############################################# Correlation Table  ###############################################################







#### Testing Regression ####
lm_model <- lm(v_267 ~ v_321, data = quest_reg)
coefficients(lm_model)
summary(lm_model)
ggplot(quest_reg, aes(v_321, v_267)) + geom_jitter() + geom_smooth(method = "lm")









###### Save and Clean ####

# Clean Environment
rm(list = ls())

