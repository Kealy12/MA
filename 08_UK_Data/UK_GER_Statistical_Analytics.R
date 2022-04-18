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

# Can use statistical testing only for maximum number of rows as in GER dataset, as length(GER data) < length(UK data)
quest_clean_UK <- subset(quest_clean_UK[1:nrow(quest_clean)])


############################################# Usage Intention  ###############################################################
#### GER ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_clean[v_132 == 0, v_132 := NA]
quest_clean[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_clean[, Usage_Intention_GER := round(rowMeans(quest_clean[, .(v_132, v_133)], na.rm = T), 2)]
usage_int_GER <- quest_clean[, .(Usage_Intention_GER)]

#### UK ####
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


############################################# Disposition to privacy ###############################################################
#### GER ####
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be zero
quest_clean[v_104 == 0, v_104 := NA]
quest_clean[v_105 == 0, v_105 := NA]
quest_clean[v_106 == 0, v_106 := NA]

# Reverse code v_106
quest_clean[, Reverse_v_106 := 8 - v_106]

# Calculate Average 
quest_clean[, Disposition_to_privacy_GER := round(rowMeans(quest_clean[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]
disp_priv_GER <- quest_clean[, .(Disposition_to_privacy_GER)]

#### UK ####
quest_clean_UK[v_104 == 0, v_104 := NA]
quest_clean_UK[v_105 == 0, v_105 := NA]
quest_clean_UK[v_106 == 0, v_106 := NA]

# Reverse code v_106
quest_clean_UK[, Reverse_v_106 := 8 - v_106]

# Calculate Average 
quest_clean_UK[, Disposition_to_privacy_UK := round(rowMeans(quest_clean_UK[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]
disp_priv_UK <- quest_clean_UK[, .(Disposition_to_privacy_UK)]




#### T-Test ####
disppriv_test <- cbind(disp_priv_GER, disp_priv_UK)
disppriv_test <- melt(disppriv_test)
t.test(value ~ variable, data = disppriv_test, alternative = "two.sided" , var.equal=F)

# p-value < 0.05 -> Significant difference 

############################################# Trust ###############################################################
#### GER ####
## Integrity: v_247, v_248, v_249
quest_clean[, .(v_247, v_248, v_249)]

# can be no 0s
quest_clean[v_247 == 0, v_247 := NA]
quest_clean[v_248 == 0, v_248 := NA]
quest_clean[v_249 == 0, v_249 := NA]

# Calculate Average Integrity:
quest_clean[, Integrity_GER := round(rowMeans(quest_clean[, .(v_247, v_248, v_249)], na.rm = T), 2)]

## Benevolence: v_250, v_251, v_252
quest_clean[, .(v_250, v_251, v_252)]

# can be no 0s
quest_clean[v_250 == 0, v_250 := NA]
quest_clean[v_251 == 0, v_251 := NA]
quest_clean[v_252 == 0, v_252 := NA]

# Calculate Average Benevolence:
quest_clean[, Benevolence_GER := round(rowMeans(quest_clean[, .(v_250, v_251, v_252)], na.rm = T), 2)]

## Ability: v_144, v_145, v_146
quest_clean[, .(v_144, v_145, v_146)]

# can be no 0s
quest_clean[v_144 == 0, v_144 := NA]
quest_clean[v_145 == 0, v_145 := NA]
quest_clean[v_146 == 0, v_146 := NA]

# Calculate Average Ability:
quest_clean[, Ability_GER := round(rowMeans(quest_clean[, .(v_144, v_145, v_146)], na.rm = T), 2)]

# Disposition to trust:
quest_clean[, Trust_GER := round(rowMeans(quest_clean[, .(Integrity_GER, Benevolence_GER, Ability_GER)], na.rm = T), 2)]
trust_GER <- quest_clean[, .(Trust_GER)]

quest_clean[, .("Tust_GER" = round(mean(Trust_GER, na.rm = T),2))]

#### UK ####
## Integrity: v_247, v_248, v_249
quest_clean_UK[, .(v_247, v_248, v_249)]

# can be no 0s
quest_clean_UK[v_247 == 0, v_247 := NA]
quest_clean_UK[v_248 == 0, v_248 := NA]
quest_clean_UK[v_249 == 0, v_249 := NA]

# Calculate Average Integrity:
quest_clean_UK[, Integrity_UK := round(rowMeans(quest_clean_UK[, .(v_247, v_248, v_249)], na.rm = T), 2)]

## Benevolence: v_250, v_251, v_252
quest_clean_UK[, .(v_250, v_251, v_252)]

# can be no 0s
quest_clean_UK[v_250 == 0, v_250 := NA]
quest_clean_UK[v_251 == 0, v_251 := NA]
quest_clean_UK[v_252 == 0, v_252 := NA]

# Calculate Average Benevolence:
quest_clean_UK[, Benevolence_UK := round(rowMeans(quest_clean_UK[, .(v_250, v_251, v_252)], na.rm = T), 2)]

## Ability: v_144, v_145, v_146
quest_clean_UK[, .(v_144, v_145, v_146)]

# can be no 0s
quest_clean_UK[v_144 == 0, v_144 := NA]
quest_clean_UK[v_145 == 0, v_145 := NA]
quest_clean_UK[v_146 == 0, v_146 := NA]

# Calculate Average Ability:
quest_clean_UK[, Ability_UK := round(rowMeans(quest_clean_UK[, .(v_144, v_145, v_146)], na.rm = T), 2)]

# Disposition to trust:
quest_clean_UK[, Trust_UK := round(rowMeans(quest_clean_UK[, .(Integrity_UK, Benevolence_UK, Ability_UK)], na.rm = T), 2)]
trust_UK <- quest_clean_UK[, .(Trust_UK)]

quest_clean_UK[, .("Tust_UK" = round(mean(Trust_UK, na.rm = T),2))]



#### T-Test ####
trust_test <- cbind(trust_GER, trust_UK)
trust_test <- melt(trust_test)
t.test(value ~ variable, data = trust_test, alternative = "two.sided" , var.equal=F)

# p-value < 0.05 -> Significant difference 


############################################# Perceived Risk ###############################################################
#### GER ####
# v_149, v_150: Likert (1-7)
# can be no 0s
quest_clean[v_149 == 0, v_149 := NA]
quest_clean[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_clean[, Perceived_risk_GER := round(rowMeans(quest_clean[, .(v_149, v_150)], na.rm = T), 2)]

perceived_risk_GER <- quest_clean[, .(Perceived_risk_GER)]

# Score overall
quest_clean[, .("Perceived risk_GER" = round(mean(Perceived_risk_GER, na.rm = T),2))]

#### UK ####
quest_clean_UK[v_149 == 0, v_149 := NA]
quest_clean_UK[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_clean_UK[, Perceived_risk_UK := round(rowMeans(quest_clean_UK[, .(v_149, v_150)], na.rm = T), 2)]

perceived_risk_UK <- quest_clean_UK[, .(Perceived_risk_UK)]

# Score overall
quest_clean_UK[, .("Perceived risk_UK" = round(mean(Perceived_risk_UK, na.rm = T),2))]

#### T-Test ####
risk_test <- cbind(perceived_risk_GER, perceived_risk_UK)
risk_test <- melt(risk_test)
t.test(value ~ variable, data = risk_test, alternative = "two.sided" , var.equal=F)
# p-value < 0.05 -> Significant difference 








################################## Cleaning ###################################
rm(list = ls())



