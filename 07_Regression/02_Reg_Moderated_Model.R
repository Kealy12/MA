#######################################################################################################################################

# Multivariate, moderated linear regression analysis

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
quest_reg_mod <- cbind(quest_clean, tri_comp_all)
colnames(quest_reg_mod)[222] <- c("Overall_TRI")
colnames(quest_reg_mod)[216] <- c("Optimism")
colnames(quest_reg_mod)[217] <- c("Innovativeness")
colnames(quest_reg_mod)[218] <- c("Discomfort")
colnames(quest_reg_mod)[219] <- c("Insecurity")

# Extract Application columns
app_scores <- quest_reg_mod[, .(v_265, v_266, v_267, v_268, v_269, v_270)]

############################################# Predictors ###############################################################
#### TRI: Optimism, Innovativeness, Discomfort, Insecurity ####
quest_reg_mod[, .(`Overall_TRI`)] # using single items for CFA
tri_overall <- quest_reg_mod[, .(`Overall_TRI`)]
optimism <- quest_reg_mod[, .(Optimism)]
innovativeness <- quest_reg_mod[, .(Innovativeness)]
discomfort <- quest_reg_mod[, .(Discomfort)]
insecurity <- quest_reg_mod[, .(Insecurity)]

# Reversing Items Dis & Ins:
quest_reg_mod[, Reverse_DIS2 := 8 - DIS2]
quest_reg_mod[, Reverse_DIS3 := 8 - DIS3]
quest_reg_mod[, Reverse_INS1 := 8 - INS1]
quest_reg_mod[, Reverse_INS2 := 8 - INS2]
quest_reg_mod[, Reverse_INS4 := 8 - INS4]

#### Social influence ####
# v_296
# Scale 1 (they never heard of it) - 10 (they are experts)
social <- quest_reg_mod[, .(v_296)]
colnames(social) <- c("Social_Influence")



#### Disposition to privacy ####
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be zero
quest_reg_mod[v_104 == 0, v_104 := NA]
quest_reg_mod[v_105 == 0, v_105 := NA]
quest_reg_mod[v_106 == 0, v_106 := NA]

# Reverse code v_106
quest_reg_mod[, Reverse_v_106 := 8 - v_106]

# Calculate Average 
quest_reg_mod[, Disposition_to_privacy := round(rowMeans(quest_reg_mod[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]

disp_priv <- quest_reg_mod[, .(Disposition_to_privacy)]



#### Trust ####

## Integrity: v_247, v_248, v_249
quest_reg_mod[, .(v_247, v_248, v_249)]

# can be no 0s
quest_reg_mod[v_247 == 0, v_247 := NA]
quest_reg_mod[v_248 == 0, v_248 := NA]
quest_reg_mod[v_249 == 0, v_249 := NA]

# Calculate Average trust_INT:
quest_reg_mod[, trust_INT := round(rowMeans(quest_reg_mod[, .(v_247, v_248, v_249)], na.rm = T), 2)]

## Benevolence: v_250, v_251, v_252
quest_reg_mod[, .(v_250, v_251, v_252)]

# can be no 0s
quest_reg_mod[v_250 == 0, v_250 := NA]
quest_reg_mod[v_251 == 0, v_251 := NA]
quest_reg_mod[v_252 == 0, v_252 := NA]

# Calculate Average trust_BEN:
quest_reg_mod[, trust_BEN := round(rowMeans(quest_reg_mod[, .(v_250, v_251, v_252)], na.rm = T), 2)]

## Ability: v_144, v_145, v_146
quest_reg_mod[, .(v_144, v_145, v_146)]

# can be no 0s
quest_reg_mod[v_144 == 0, v_144 := NA]
quest_reg_mod[v_145 == 0, v_145 := NA]
quest_reg_mod[v_146 == 0, v_146 := NA]

# Calculate Average trust_ABI:
quest_reg_mod[, trust_ABI := round(rowMeans(quest_reg_mod[, .(v_144, v_145, v_146)], na.rm = T), 2)]


# Score overall
quest_reg_mod[, .("T_INT score overall" = round(mean(trust_INT, na.rm = T),2))]
quest_reg_mod[, .("T_BEN score overall" = round(mean(trust_BEN, na.rm = T),2))]
quest_reg_mod[, .("T_ABI score overall" = round(mean(trust_ABI, na.rm = T),2))]

# Disposition to trust:
quest_reg_mod[, Trust := round(rowMeans(quest_reg_mod[, .(trust_INT, trust_BEN, trust_ABI)], na.rm = T), 2)]
trust <- quest_reg_mod[, .(Trust)]

quest_reg_mod[, .("Tust in BT score overall" = round(mean(Trust, na.rm = T),2))]


#### Perceived benefit for society ####
# v_147, v_148: Likert (1-7)
# can be no 0s
quest_reg_mod[v_147 == 0, v_147 := NA]
quest_reg_mod[v_148 == 0, v_148 := NA]

# Reverse code v_148
quest_reg_mod[, Reverse_v_148 := 8 - v_148]

# Calculate Average 
quest_reg_mod[, Perceived_benefit_for_society := round(rowMeans(quest_reg_mod[, .(v_147, Reverse_v_148)], na.rm = T), 2)]

perc_benf_s <- quest_reg_mod[, .(Perceived_benefit_for_society)]

# Score overall
quest_reg_mod[, .("Perceived benefit for society score overall" = round(mean(Perceived_benefit_for_society, na.rm = T),2))]


#### Perceived risk ####

# v_149, v_150: Likert (1-7)
# can be no 0s
quest_reg_mod[v_149 == 0, v_149 := NA]
quest_reg_mod[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_reg_mod[, Percieved_risk := round(rowMeans(quest_reg_mod[, .(v_149, v_150)], na.rm = T), 2)]

percieved_risk <- quest_reg_mod[, .(Percieved_risk)]

# Score overall
quest_reg_mod[, .("Perceived risk score overall" = round(mean(Percieved_risk, na.rm = T),2))]

#### Potential of disruption ####
# v_151, v_152, v_153, v_154 (reverse coded): Likert (1-7)
# can be no 0s
quest_reg_mod[v_151 == 0, v_151 := NA]
quest_reg_mod[v_152 == 0, v_152 := NA]
quest_reg_mod[v_153 == 0, v_153 := NA]
quest_reg_mod[v_154 == 0, v_154 := NA]

# reverse code v_154
quest_reg_mod[, Reverse_v_154 := 8 - v_154]

# Calculate Average 
quest_reg_mod[, Potential_of_disruption := round(rowMeans(quest_reg_mod[, .(v_151, v_152, v_153, Reverse_v_154)], na.rm = T), 2)]

pot_disrup <- quest_reg_mod[, .(Potential_of_disruption)]

# Score overallquest_reg_mod[, .("Potential of disruption score overall" = round(mean(Potential_of_disruption, na.rm = T),2))]


#### Usage intention ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_reg_mod[v_132 == 0, v_132 := NA]
quest_reg_mod[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_reg_mod[, Usage_Intention := round(rowMeans(quest_reg_mod[, .(v_132, v_133)], na.rm = T), 2)]
usage_int <- quest_reg_mod[, .(Usage_Intention)]



############################################# Moderators ###############################################################
#### Age ####
# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
age <- quest_reg_mod[, c("v_285")]
age <- age[, v_285 := v_285 + 14]
colnames(age) <- c("Age")

#### Gender ####
# v_169 -> Male = 1, Female = 0
gender <- quest_reg_mod[, .(v_169)]
gender <- gender[v_169 == 2, v_169 := 0]
colnames(gender) <- c("Gender")



#### Experience ####
# Mean of contact with BT and knowledge

# Contact with BT
# v_10: in professional life
# v_11: in personal life
contact<- quest_reg_mod[, c("v_10", "v_11")]
colnames(contact) <- c("Contact_in_professional_life", "Contact_in_personal_life")

# Knowledge 
# (1-10 scale): v_286
knowledge <- quest_reg_mod[, .(v_286)]
colnames(knowledge) <- c("Knowledge_of_BT")

# Binding together and creating mean
experience <- cbind(contact, knowledge)
experience[, Experience := round(rowMeans(experience[, .(Contact_in_professional_life, Contact_in_personal_life, Knowledge_of_BT)], na.rm = T), 2)]


#### Possession of crypto ####
# v_54: (yes = 1, no = 0)
quest_reg_mod <- quest_reg_mod[v_54 == 2, v_54 := 0]
table(quest_reg_mod[, .(v_54)])
poss_crypto <- quest_reg_mod[, .(v_54)]
colnames(poss_crypto) <- c("Possession_of_cryptocurrency")

#### National culture TBD ####
# TBD

############################################# CFA ###############################################################

cfa_mod_model <- 
'tri =~ optimism + innovativeness + discomfort + insecurity
optimism =~ OPT2 + OPT4
innovativeness =~ INN1 + INN2 + INN4 
discomfort =~ DIS2 + DIS3
insecurity =~ INS1 + INS2 + INS4
disposition_to_privacy =~ v_104 + v_105 + Reverse_v_106
integrity =~ v_247 + v_248 + v_249
benevolence =~ v_250 + v_251 + v_252
ability =~ v_144 + v_145 + v_146
trust =~ integrity + benevolence + ability
perceived_benefit_for_society =~ v_147 + Reverse_v_148
perceived_risk =~ v_149 + v_150
potential_of_disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
usage_intention =~ v_132 + v_133
experience =~ v_10 + v_11 + v_286
'
# Non-Factors:
# social_influence =~ v_296
# age =~ v_285
# gender =~ v_169
# possession_of_cryptocurrency =~ v_54
# national_culture =~ 0

# Performing CFA
cfa_mod <- cfa(cfa_mod_model, quest_reg_mod, std.lv=TRUE)
summary(cfa_mod, standardized=TRUE, fit.measures = T)

# factor loadings
inspect(cfa_mod,what="std")$lambda
standardizedsolution(cfa_mod)

############################################# C's alpha & AVE ###############################################################
# Average Variance extracted > 0.5  Cf. Parasuraman and Colby (2015)
# Alpha > 0.7 Cf. Nunnally & Bernstein (1978)
semTools::reliability(cfa_mod)

############################################# Correlations  ###############################################################
predictors_mod <- cbind(optimism, innovativeness, discomfort, insecurity, social, disp_priv, trust, perc_benf_s, percieved_risk, pot_disrup, usage_int,
                        age, gender, experience, poss_crypto)
cor_all <- apa.cor.table(predictors_mod, show.conf.interval = F)
cor_all

############################################# Regression results incl. VIF ###############################################################
gender$Gender <- as.factor(gender$Gender)
poss_crypto$Possession_of_cryptocurrency <- as.factor(poss_crypto$Possession_of_cryptocurrency)










###### Save and Clean ####

# Clean Environment
rm(list = ls())







