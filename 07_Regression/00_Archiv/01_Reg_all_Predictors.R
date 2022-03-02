#######################################################################################################################################

# Multivariate Linear Regression Analysis

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

############################################# Extracting predictors ###############################################################

#### Have you heard of BT ####
# (yes = 1,no = 0): v_321
quest_reg <- quest_reg[v_321 == 2, v_321 := 0]
heard <- quest_reg[, .(v_321)]
colnames(heard) <- c("Heard_of_BT")

#### Contact with BT ####

# v_10: in professional life
# v_11: in personal life
contact<- quest_reg[, c("v_10", "v_11")]
colnames(contact) <- c("Contact_in_professional_life", "Contact_in_personal_life")

#### Knowledge of BT  ####
# (1-10 scale): v_286
knowledge <- quest_reg[, .(v_286)]
colnames(knowledge) <- c("Knowledge_of_BT")

#### Possession of Crypto ####
# v_54: (yes = 1, no = 0)
quest_reg <- quest_reg[v_54 == 2, v_54 := 0]
table(quest_reg[, .(v_54)])
poss_crypto <- quest_reg[, .(v_54)]
colnames(poss_crypto) <- c("Possess_Crypto")




#### Social influence on blockchain usage ####

# v_296
# Scale 1 (they never heard of it) - 10 (they are experts)
social <- quest_reg[, .(v_296)]
colnames(social) <- c("Social_Influence")
social[, .("Social influence on my blockchain usage (1-10)" = round(mean(Social_Influence, na.rm = T),2))]
# They would rather discourage me



#### Personal Innovativeness ####
pers_inn <- quest_reg[, .(v_205, v_206)]
pers_inn[pers_inn == 0] <- NA
quest_reg[v_205 == 0, v_205 := NA]
quest_reg[v_206 == 0, v_206 := NA]

# Reverse code v_205
pers_inn[, Reverse_v_205 := 8 - v_205]
quest_reg[, Reverse_v_205 := 8 - v_205]
pers_inn[, PIIT := round(rowMeans(pers_inn_score[, .(v_206, Reverse_v_205)], na.rm = T), 2)]

pers_inn_pred <- pers_inn[, .(PIIT)]


####### Constructs ########

#### TRI ####
quest_reg[, .(`Overall_TRI`)] # using single items for CFA
tri_overall <- quest_reg[, .(`Overall_TRI`)]
optimism <- quest_reg[, .(Optimism)]
innovativeness <- quest_reg[, .(Innovativeness)]
discomfort <- quest_reg[, .(Discomfort)]
insecurity <- quest_reg[, .(Insecurity)]

# Reversing Items Dis & Ins:
quest_reg[, Reverse_DIS2 := 8 - DIS2]
quest_reg[, Reverse_DIS3 := 8 - DIS3]
quest_reg[, Reverse_INS1 := 8 - INS1]
quest_reg[, Reverse_INS2 := 8 - INS2]
quest_reg[, Reverse_INS4 := 8 - INS4]


#### Disposition to privacy ####
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be 0
quest_reg[v_104 == 0, v_104 := NA]
quest_reg[v_105 == 0, v_105 := NA]
quest_reg[v_106 == 0, v_106 := NA]

# Reverse code v_106
quest_reg[, Reverse_v_106 := 8 - v_106]

# Calculate Average 
quest_reg[, DISPPRIV := round(rowMeans(quest_reg[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]

disp_priv <- quest_reg[, .(DISPPRIV)]

# Score overall
quest_reg[, .("Privacy score overall" = round(mean(DISPPRIV, na.rm = T),2))]




#### Usage Intention ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_reg[v_132 == 0, v_132 := NA]
quest_reg[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_reg[, Usage_Int := round(rowMeans(quest_reg[, .(v_132, v_133)], na.rm = T), 2)]

usage_int <- quest_reg[, .(Usage_Int)]

# Score overall
quest_reg[, .("Usage intention score overall" = round(mean(Usage_Int, na.rm = T),2))]



#### Trust in blockchain technology ####

## Integrity: v_247, v_248, v_249
quest_reg[, .(v_247, v_248, v_249)]

# can be no 0s
quest_reg[v_247 == 0, v_247 := NA]
quest_reg[v_248 == 0, v_248 := NA]
quest_reg[v_249 == 0, v_249 := NA]

# Calculate Average trust_INT:
quest_reg[, trust_INT := round(rowMeans(quest_reg[, .(v_247, v_248, v_249)], na.rm = T), 2)]

## Benevolence: v_250, v_251, v_252
quest_reg[, .(v_250, v_251, v_252)]

# can be no 0s
quest_reg[v_250 == 0, v_250 := NA]
quest_reg[v_251 == 0, v_251 := NA]
quest_reg[v_252 == 0, v_252 := NA]

# Calculate Average trust_BEN:
quest_reg[, trust_BEN := round(rowMeans(quest_reg[, .(v_250, v_251, v_252)], na.rm = T), 2)]

## Ability: v_144, v_145, v_146
quest_reg[, .(v_144, v_145, v_146)]

# can be no 0s
quest_reg[v_144 == 0, v_144 := NA]
quest_reg[v_145 == 0, v_145 := NA]
quest_reg[v_146 == 0, v_146 := NA]

# Calculate Average trust_ABI:
quest_reg[, trust_ABI := round(rowMeans(quest_reg[, .(v_144, v_145, v_146)], na.rm = T), 2)]


# Score overall
quest_reg[, .("T_INT score overall" = round(mean(trust_INT, na.rm = T),2))]
quest_reg[, .("T_BEN score overall" = round(mean(trust_BEN, na.rm = T),2))]
quest_reg[, .("T_ABI score overall" = round(mean(trust_ABI, na.rm = T),2))]

# Disposition to trust:
quest_reg[, Trust_in_BT := round(rowMeans(quest_reg[, .(trust_INT, trust_BEN, trust_ABI)], na.rm = T), 2)]
disp_trust <- quest_reg[, .(Trust_in_BT)]

quest_reg[, .("Tust in BT score overall" = round(mean(Trust_in_BT, na.rm = T),2))]


#### Perceived benefit for society ####

# v_147, v_148: Likert (1-7)
# can be no 0s
quest_reg[v_147 == 0, v_147 := NA]
quest_reg[v_148 == 0, v_148 := NA]

# Reverse code v_148
quest_reg[, Reverse_v_148 := 8 - v_148]

# Calculate Average 
quest_reg[, Perc_Benefit := round(rowMeans(quest_reg[, .(v_147, Reverse_v_148)], na.rm = T), 2)]

perc_benf_s <- quest_reg[, .(Perc_Benefit)]

# Score overall
quest_reg[, .("Perceived benefit for society score overall" = round(mean(Perc_Benefit, na.rm = T),2))]


#### Perceived Risk ####

# v_149, v_150: Likert (1-7)
# can be no 0s
quest_reg[v_149 == 0, v_149 := NA]
quest_reg[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_reg[, Perc_Risk := round(rowMeans(quest_reg[, .(v_149, v_150)], na.rm = T), 2)]

perc_risk <- quest_reg[, .(Perc_Risk)]

# Score overall
quest_reg[, .("Perceived risk score overall" = round(mean(Perc_Risk, na.rm = T),2))]





#### Potential of disruption ####

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

pot_disrup <- quest_reg[, .(Pot_Dis)]

# Score overall
quest_reg[, .("Potential of disruption score overall" = round(mean(Pot_Dis, na.rm = T),2))]















############################################# Potential Moderators ###############################################################
#### Age ####

# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
age <- quest_reg[, c("v_285")]
age <- age[, v_285 := v_285 + 14]
colnames(age) <- c("Age")

#### Gender ####
# v_169 -> Male = 1, Female = 0
gender <- quest_reg[, .(v_169)]
gender <- gender[v_169 == 2, v_169 := 0]
colnames(gender) <- c("Gender")
gender$Gender <- as.factor(gender$Gender)



#### Experience? ####


############################################# Total model: CFA  ###############################################################

# Full CFA model
factors <- 
'Disposition_Privacy =~ v_104 + v_105 + Reverse_v_106
Useage_Intent =~ v_132 + v_133
integrity =~ v_247 + v_248 + v_249
benevolence =~ v_250 + v_251 + v_252
ability =~ v_144 + v_145 + v_146
Trust_BT =~ integrity + benevolence + ability
Perceived_Risk =~ v_149 + v_150
Perceived_Benefit_S =~ v_147 + Reverse_v_148
Potential_Disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
Social_Influence =~ v_296
TRI =~ Opt + Inn + Dis + Ins
Opt =~ OPT2 + OPT4
Inn =~ INN1 + INN2 + INN4 
Dis =~ DIS2 + DIS3
Ins =~ INS1 + INS2 + INS4'

# Excluded PIIT due to similarity with TRI
# Personal_Innovativeness =~ v_206 + Reverse_v_205

# Performing CFA
cfa_all <- cfa(factors, quest_reg, std.lv=TRUE)
summary(cfa_all, standardized=TRUE, fit.measures = T)

# factor loadings
inspect(cfa_all,what="std")$lambda

standardizedsolution(cfa_all)

############################################# Total model: C's alpha & AVE  ###############################################################

# Average Variance extracted > 0.5  Cf. Parasuraman and Colby (2015)
# Alpha > 0.7 Cf. Nunnally & Bernstein (1978)
semTools::reliability(cfa_all)


############################################# Total model: Correlations ###############################################################
# Correlations
predictors_all <- cbind(heard,knowledge,poss_crypto,contact,social,tri_overall, optimism, innovativeness, discomfort, insecurity, disp_priv,usage_int,disp_trust,perc_benf_s,perc_risk,pot_disrup)
cor_all <- apa.cor.table(predictors_all, show.conf.interval = F)
cor_all

# Variance Inflation Factor is checked in Regression analysis

############################################# Total model: Adjustments to be made  ###############################################################

# Cutoff value on factor loadings < 0.4 -> DIS3 = 0.378 deleted
# AVE < 0.5 -> DIS
# Alpha < 0.7 -> DIS, (Perceived benefit to Society)
# Deleted Perceived Benefit for Society and Heard of BT due to higher Correlations (VIF)
# Delete PIIT due to high correlations and VIF

############################################# Fitted Model: CFA #######################################################################

factors_fit <- 
'Disposition_Privacy =~ v_104 + v_105 + Reverse_v_106
Useage_Intent =~ v_132 + v_133
integrity =~ v_247 + v_248 + v_249
benevolence =~ v_250 + v_251 + v_252
ability =~ v_144 + v_145 + v_146
Trust_BT =~ integrity + benevolence + ability
Perceived_Risk =~ v_149 + v_150
Perceived_Benefit_S =~ v_147 + Reverse_v_148
Potential_Disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
TRI =~ Opt + Inn + Dis + Ins
Opt =~ OPT2 + OPT4
Inn =~ INN1 + INN2 + INN4 
Dis =~ DIS2
Ins =~ INS1 + INS2 + INS4'

# CFA - fitted
cfa_fit <- cfa(factors_fit, quest_reg, std.lv=TRUE)
summary(cfa_fit, standardized=TRUE, fit.measures = T)
standardizedsolution(cfa_fit)

# Insecurity construct < 0.4

############################################# Fitted model: C's alpha & AVE  ###############################################################

# Average Variance extracted > 0.5  Cf. Parasuraman and Colby (2015)
# Alpha > 0.7 Cf. Nunnally & Bernstein (1978)
semTools::reliability(cfa_fit)

# keep INS due to high Alpha and AVE


############################################# Fitted Model: Correlations ###############################################################

predictors_fit <- cbind(knowledge,poss_crypto,contact,social, optimism, innovativeness, discomfort, insecurity, disp_priv,usage_int,disp_trust,perc_benf_s,perc_risk,pot_disrup)
cor_fit <- apa.cor.table(predictors_fit, show.conf.interval = F)
cor_fit

############################################# Regression analysis - separate Applications  ###############################################################
#### Setup ####
indep_var_all <- c("Heard_of_BT + Knowledge_of_BT + Possess_Crypto + Social_Influence + Contact_in_professional_life + Contact_in_personal_life + Optimism + Innovativeness + Discomfort + Insecurity + DISPPRIV + Usage_Int + Trust_in_BT + Perc_Benefit + Perc_Risk + Pot_Dis")
indep_var_fitted <- c("Knowledge_of_BT + Possess_Crypto + Social_Influence + Contact_in_professional_life + Contact_in_personal_life + Optimism + Innovativeness + Discomfort + Insecurity + DISPPRIV + Mean_all_Applic + Trust_in_BT + Perc_Risk + Perc_Benefit + Pot_Dis")
indep_var_moderated <- c(" Knowledge_of_BT + Possess_Crypto + Social_Influence*Gender + Contact_in_professional_life*Gender + Contact_in_personal_life*Gender + Optimism*Gender + Innovativeness*Gender + Discomfort*Gender + Insecurity*Gender + DISPPRIV*Gender + Usage_Int*Gender + Trust_in_BT*Gender + Perc_Risk*Gender + Perc_Benefit*Gender + Pot_Dis*Gender")

#### 1. Tokenization of Assets ####

## ALL predictors
token_pred <- cbind(predictors_all, quest_reg[, .(v_265)])
outcome_1 <- c("v_265")
f1 <- as.formula(paste(outcome_1, paste(indep_var_all, collapse = " + "), sep = " ~ "))
lm_token <- lm(f1, data = token_pred)

apa.reg.table(lm_token)
car::vif(lm_token)
# low VIFs


## FITTED model
token_pred_fit <- cbind(predictors_fit, quest_reg[, .(v_265)])
outcome_1 <- c("v_265")
f1 <- as.formula(paste(outcome_1, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_token_fit <- lm(f1, data = token_pred_fit)

apa.reg.table(lm_token_fit)
car::vif(lm_token_fit)

## Moderated Model ##
token_pred_mod <- cbind(predictors_fit, quest_reg[, .(v_265)], gender, age)
outcome_1 <- c("v_265")
f1 <- as.formula(paste(outcome_1, paste(indep_var_moderated, collapse = " + "), sep = " ~ "))
lm_token_mod <- lm(f1, data = token_pred_mod)

apa.reg.table(lm_token_mod)
car::vif(lm_token_mod)




#### 2. Fractional Ownership ####
fract_pred <- cbind(predictors_fit, quest_reg[, .(v_266)])
outcome_2 <- c("v_266")
f2 <- as.formula(paste(outcome_2, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_fract <- lm(f2, data = fract_pred)
apa.reg.table(lm_fract)

# Cluster model
fract_pred_clus <- cbind(predictors_clus, clus_table[, .(v_266.x)])
outcome_2 <- c("v_266.x")
fc2 <- as.formula(paste(outcome_2, paste(indep_var_clus, collapse = " + "), sep = " ~ "))
lm_fract <- lm(fc2, data = fract_pred_clus)
apa.reg.table(lm_fract)

## Moderated Model ##
fract_pred_mod <- cbind(predictors_fit, quest_reg[, .(v_266)], gender, age)
outcome_2 <- c("v_266")
fm2 <- as.formula(paste(outcome_2, paste(indep_var_moderated, collapse = " + "), sep = " ~ "))
lm_fract_mod <- lm(fm2, data = fract_pred_mod)

apa.reg.table(lm_fract_mod)
car::vif(lm_fract_mod)

#### 3. Self-Sovereign Identity ####
self_pred <- cbind(predictors_fit, quest_reg[, .(v_267)])
outcome_3 <- c("v_267")
f3 <- as.formula(paste(outcome_3, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_self <- lm(f3, data = self_pred)
apa.reg.table(lm_self)

#### 4. Smart Contracts ####
smart_pred <- cbind(predictors_fit, quest_reg[, .(v_268)])
outcome_4 <- c("v_268")
f4 <- as.formula(paste(outcome_4, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_smart <- lm(f4, data = smart_pred)
apa.reg.table(lm_smart)

#### 5. Micropayments ####
micro_pred <- cbind(predictors_fit, quest_reg[, .(v_269)])
outcome_5 <- c("v_269")
f5 <- as.formula(paste(outcome_5, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_micro <- lm(f5 , data = micro_pred)
apa.reg.table(lm_micro)

#### 6. Anonymous Transactions ####
anony_pred <- cbind(predictors_fit, quest_reg[, .(v_270)])
outcome_6 <- c("v_270")
f6 <- as.formula(paste(outcome_6, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_anony <- lm(f6, data = anony_pred)
apa.reg.table(lm_anony)


























############################################# Regression analysis - mean Applications Overall  ###############################################################

# Calculating mean over all applications
quest_reg[, Mean_all_Applic := round(rowMeans(quest_reg[, .(v_265, v_266, v_267, v_268, v_269, v_270)], na.rm = T), 2)]

# FITTED model
overall_pred <- cbind(predictors_fit, quest_reg[, .(Mean_all_Applic)])
outcome_overall <- c("Usage_Int")
f_overall <- as.formula(paste(outcome_overall, paste(indep_var_fitted, collapse = " + "), sep = " ~ "))
lm_overall <- lm(f_overall, data = overall_pred)

apa.reg.table(lm_overall)
car::vif(lm_overall)


# MODERATED model
overall_mod <- cbind(predictors_fit, quest_reg[, .(Mean_all_Applic)], gender, age)
outcome_overall <- c("Mean_all_Applic")
f_overall <- as.formula(paste(outcome_overall, paste(indep_var_moderated, collapse = " + "), sep = " ~ "))
lm_overall_mod <- lm(f_overall, data = overall_mod)

apa.reg.table(lm_overall_mod)
car::vif(lm_overall_mod)


############################################# Regression analysis - Intention to use BT (pre-survey) ###############################################################

intention_pre <- quest_clean[, .(v_28)]
intention_pre <- intention_pre[v_28 == 2, v_28 := 0]
# Turning "Don't knows" into "Nos"
intention_pre <- intention_pre[v_28 == 3, v_28 := 0]
colnames(intention_pre) <- c("Intention_to_use_BT")

#### Logistic Regression, as Y is categorical 
indep_var_logistic<- c("Knowledge_of_BT + Possess_Crypto + Social_Influence + Contact_in_professional_life + Contact_in_personal_life + Optimism + Innovativeness + Discomfort + Insecurity + DISPPRIV + Trust_in_BT + Perc_Risk + Perc_Benefit + Pot_Dis")
intention_pred <- cbind(predictors_fit, intention_pre)
outcome_usage <- c("Intention_to_use_BT")
f_usage <- as.formula(paste(outcome_usage, paste(indep_var_logistic, collapse = " + "), sep = " ~ "))

lm_intention <- glm(f_usage, data = intention_pred, family = "binomial")

summary(lm_intention)
car::vif(lm_intention)


###### Save and Clean ####

# Clean Environment
rm(list = ls())

