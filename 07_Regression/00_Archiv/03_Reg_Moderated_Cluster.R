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
library(semTools)
library(cSEM)

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
quest_reg_mod_cluster <- cbind(quest_clean, tri_comp_all)
colnames(quest_reg_mod_cluster)[222] <- c("Overall_TRI")
colnames(quest_reg_mod_cluster)[216] <- c("Optimism")
colnames(quest_reg_mod_cluster)[217] <- c("Innovativeness")
colnames(quest_reg_mod_cluster)[218] <- c("Discomfort")
colnames(quest_reg_mod_cluster)[219] <- c("Insecurity")

# Extract Application columns
app_scores <- quest_reg_mod_cluster[, .(v_265, v_266, v_267, v_268, v_269, v_270)]

# Base table
dim(quest_tri_extended)

clus_table <- merge(quest_tri_extended, quest_reg_mod_cluster, by.x = "lfdn", by.y = "lfdn", all.x = T)
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

############################################# Predictors ###############################################################
#### TRI: Optimism, Innovativeness, Discomfort, Insecurity ####
clus_table[, .(`Overall_TRI`)] # using single items for CFA
tri_overall <- clus_table[, .(`Overall_TRI`)]
optimism <- clus_table[, .(Optimism)]
innovativeness <- clus_table[, .(Innovativeness)]
discomfort <- clus_table[, .(Discomfort)]
insecurity <- clus_table[, .(`Insecurity (INS)`)]
colnames(insecurity) <- c("Insecurity")

# Reversing Items Dis & Ins:
clus_table[, Reverse_DIS2 := 8 - DIS2.x]
clus_table[, Reverse_DIS3 := 8 - DIS3.x]
clus_table[, Reverse_INS1 := 8 - INS1.x]
clus_table[, Reverse_INS2 := 8 - INS2.x]
clus_table[, Reverse_INS4 := 8 - INS4.x]

# Adjusting Discomfort to fit CFA: Deleted DIS3; Only DIS2 relevant for Discomfort
discomfort_fitted <- clus_table[, .(Discomfort_fit = DIS2.x)]
discomfort_fitted <- cbind(discomfort, discomfort_fitted)
discomfort_fitted[, Discomfort := NULL]
colnames(discomfort_fitted) <- c("Discomfort")
discomfort_fitted

#### Social influence ####
# v_296
# Scale 1 (they never heard of it) - 10 (they are experts)
social <- clus_table[, .(v_296.x)]
colnames(social) <- c("Social_Influence")

#### Disposition to privacy ####
# Likert 1 (low privacy concern) - 7 (high privacy concern)): v_104, v_105, v_106
# can not be zero
clus_table[v_104.x == 0, v_104.x := NA]
clus_table[v_105.x == 0, v_105.x := NA]
clus_table[v_106.x == 0, v_106.x := NA]

# Reverse code v_106
clus_table[, Reverse_v_106 := 8 - v_106.x]

# Calculate Average 
clus_table[, Disposition_to_privacy := round(rowMeans(clus_table[, .(v_104.x, v_105.x, Reverse_v_106)], na.rm = T), 2)]

disp_priv <- clus_table[, .(Disposition_to_privacy)]



#### Trust ####

## Integrity: v_247.x, v_248.x, v_249.x
clus_table[, .(v_247.x, v_248.x, v_249.x)]

# can be no 0s
clus_table[v_247.x == 0, v_247.x := NA]
clus_table[v_248.x == 0, v_248.x := NA]
clus_table[v_249.x == 0, v_249.x := NA]

# Calculate Average Integrity:
clus_table[, Integrity := round(rowMeans(clus_table[, .(v_247.x, v_248.x, v_249.x)], na.rm = T), 2)]

## Benevolence: v_250.x, v_251.x, v_252.x
clus_table[, .(v_250.x, v_251.x, v_252.x)]

# can be no 0s
clus_table[v_250.x == 0, v_250.x := NA]
clus_table[v_251.x == 0, v_251.x := NA]
clus_table[v_252.x == 0, v_252.x := NA]

# Calculate Average Benevolence:
clus_table[, Benevolence := round(rowMeans(clus_table[, .(v_250.x, v_251.x, v_252.x)], na.rm = T), 2)]

## Ability: v_144.x, v_145.x, v_146.x
clus_table[, .(v_144.x, v_145.x, v_146.x)]

# can be no 0s
clus_table[v_144.x == 0, v_144.x := NA]
clus_table[v_145.x == 0, v_145.x := NA]
clus_table[v_146.x == 0, v_146.x := NA]

# Calculate Average Ability:
clus_table[, Ability := round(rowMeans(clus_table[, .(v_144.x, v_145.x, v_146.x)], na.rm = T), 2)]


# Score overall
clus_table[, .("T_INT score overall" = round(mean(Integrity, na.rm = T),2))]
clus_table[, .("T_BEN score overall" = round(mean(Benevolence, na.rm = T),2))]
clus_table[, .("T_ABI score overall" = round(mean(Ability, na.rm = T),2))]

# Disposition to trust:
clus_table[, Trust := round(rowMeans(clus_table[, .(Integrity, Benevolence, Ability)], na.rm = T), 2)]
trust <- clus_table[, .(Trust)]

clus_table[, .("Tust in BT score overall" = round(mean(Trust, na.rm = T),2))]


#### Perceived benefit for society ####
# v_147.x, v_148.x: Likert (1-7)
# can be no 0s
clus_table[v_147.x == 0, v_147.x := NA]
clus_table[v_148.x == 0, v_148.x := NA]

# Reverse code v_148.x
clus_table[, Reverse_v_148 := 8 - v_148.x]

# Calculate Average 
clus_table[, Perceived_benefit_for_society := round(rowMeans(clus_table[, .(v_147.x, Reverse_v_148)], na.rm = T), 2)]

perc_benf_s <- clus_table[, .(Perceived_benefit_for_society)]

# Score overall
clus_table[, .("Perceived benefit for society score overall" = round(mean(Perceived_benefit_for_society, na.rm = T),2))]


#### Perceived risk ####

# v_149.x, v_150.x: Likert (1-7)
# can be no 0s
clus_table[v_149.x == 0, v_149.x := NA]
clus_table[v_150.x == 0, v_150.x := NA]

# Calculate Average 
clus_table[, Percieved_risk := round(rowMeans(clus_table[, .(v_149.x, v_150.x)], na.rm = T), 2)]

percieved_risk <- clus_table[, .(Percieved_risk)]

# Score overall
clus_table[, .("Perceived risk score overall" = round(mean(Percieved_risk, na.rm = T),2))]

#### Potential of disruption ####
# v_151.x, v_152.x, v_153.x, v_154.x (reverse coded): Likert (1-7)
# can be no 0s
clus_table[v_151.x == 0, v_151.x := NA]
clus_table[v_152.x == 0, v_152.x := NA]
clus_table[v_153.x == 0, v_153.x := NA]
clus_table[v_154.x == 0, v_154.x := NA]

# reverse code v_154.x
clus_table[, Reverse_v_154 := 8 - v_154.x]

# Calculate Average 
clus_table[, Potential_of_disruption := round(rowMeans(clus_table[, .(v_151.x, v_152.x, v_153.x, Reverse_v_154)], na.rm = T), 2)]

pot_disrup <- clus_table[, .(Potential_of_disruption)]

# Score overallquest_reg_mod[, .("Potential of disruption score overall" = round(mean(Potential_of_disruption, na.rm = T),2))]


#### Usage intention ####
# v_132.x, v_133.x: Likert (1-7)
# can be no 0s
clus_table[v_132.x == 0, v_132.x := NA]
clus_table[v_133.x == 0, v_133.x := NA]

# Calculate Average 
clus_table[, Usage_Intention := round(rowMeans(clus_table[, .(v_132.x, v_133.x)], na.rm = T), 2)]
usage_int <- clus_table[, .(Usage_Intention)]



############################################# Moderators ###############################################################
#### Age ####
# v_285.x -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
age <- clus_table[, c("v_285.x")]
age <- age[, v_285.x := v_285.x + 14]
colnames(age) <- c("Age")

#### Gender ####
# v_169.x -> Male = 1, Female = 0
gender <- clus_table[, .(v_169.x)]
gender <- gender[v_169.x == 2, v_169.x := 0]
colnames(gender) <- c("Gender")


#### Experience ####
# Mean of contact with BT and knowledge

# Contact with BT
# v_10.x: in professional life
# v_11.x: in personal life
contact<- clus_table[, c("v_10.x", "v_11.x")]
colnames(contact) <- c("Contact_in_professional_life", "Contact_in_personal_life")

# Knowledge 
# (1-10 scale): v_286.x
knowledge <- clus_table[, .(v_286.x)]
colnames(knowledge) <- c("Knowledge_of_BT")

# Binding together and creating mean
experience <- cbind(contact, knowledge)
experience[, Experience := round(rowMeans(experience[, .(Contact_in_professional_life, Contact_in_personal_life, Knowledge_of_BT)], na.rm = T), 2)]


#### Possession of crypto ####
# v_54.x: (yes = 1, no = 0)
clus_table <- clus_table[v_54.x == 2, v_54.x := 0]
table(clus_table[, .(v_54.x)])
poss_crypto <- clus_table[, .(v_54.x)]
colnames(poss_crypto) <- c("Possession_of_cryptocurrency")

#### National culture TBD ####
# TBD

############################################# Preliminary CFA ###############################################################

cfa_mod_model <- 
'tri =~ optimism + innovativeness + discomfort + insecurity
optimism =~ OPT2.x + OPT4.x
innovativeness =~ INN1.x + INN2.x + INN4.x 
discomfort =~ DIS2.x + DIS3.x
insecurity =~ INS1.x + INS2.x + INS4.x
disposition_to_privacy =~ v_104.x + v_105.x + Reverse_v_106
integrity =~ v_247.x + v_248.x + v_249.x
benevolence =~ v_250.x + v_251.x + v_252.x
ability =~ v_144.x + v_145.x + v_146.x
trust =~ integrity + benevolence + ability
perceived_benefit_for_society =~ v_147.x + Reverse_v_148
perceived_risk =~ v_149.x + v_150.x
potential_of_disruption =~ v_151.x + v_152.x + v_153.x + Reverse_v_154
usage_intention =~ v_132.x + v_133.x
experience =~ v_10.x + v_11.x + v_286.x
'
# Non-Factors:
# social_influence =~ v_296
# age =~ v_285.x
# gender =~ v_169.x
# possession_of_cryptocurrency =~ v_54.x
# national_culture =~ 0

# Performing CFA
cfa_mod <- cfa(cfa_mod_model, clus_table, std.lv=TRUE)
summary(cfa_mod, standardized=TRUE, fit.measures = T)

# factor loadings
inspect(cfa_mod,what="std")$lambda
cfa_mod_table <- standardizedsolution(cfa_mod)
cfa_mod_table

############################################# Preliminary Reliability & Validity Criteria ###############################################################
# Average Variance extracted > 0.5  Cf. Parasuraman and Colby (2015)
# Alpha > 0.7 Cf. Nunnally & Bernstein (1978)
semTools::reliability(cfa_mod)

############################################# Preliminary Correlations  ###############################################################
predictors_mod <- cbind(optimism, innovativeness, discomfort, insecurity, social, disp_priv, trust, perc_benf_s, percieved_risk, pot_disrup, usage_int,
                        age, gender, experience[, .(Experience)], poss_crypto)
cor_all <- apa.cor.table(predictors_mod, show.conf.interval = FALSE)
cor_all

############################################# Adjustments  ###############################################################
# Cutoff value on factor loadings < 0.5 (Hair et al. 2009) -> DIS3 = 0.378 deleted, insecurity = 0.406 -> Validated through AVE and Cronbach's alpha
# AVE < 0.5 -> DIS

# Discomfort only consists of DIS2
quest_reg_mod_cluster

############################################# 1. Fitted CFA  ###############################################################

cfa_mod_model_fit <- 
'tri =~ optimism + innovativeness + discomfort + insecurity
optimism =~ OPT2.x + OPT4.x
innovativeness =~ INN1.x + INN2.x + INN4.x 
discomfort =~ DIS2.x
insecurity =~ INS1.x + INS2.x + INS4.x
disposition_to_privacy =~ v_104.x + v_105.x + Reverse_v_106
integrity =~ v_247.x + v_248.x + v_249.x
benevolence =~ v_250.x + v_251.x + v_252.x
ability =~ v_144.x + v_145.x + v_146.x
trust =~ integrity + benevolence + ability
perceived_benefit_for_society =~ v_147.x + Reverse_v_148
perceived_risk =~ v_149.x + v_150.x
potential_of_disruption =~ v_151.x + v_152.x + v_153.x + Reverse_v_154
usage_intention =~ v_132.x + v_133.x
experience =~ v_10.x + v_11.x + v_286.x
'

cfa_mod_fit <- cfa(cfa_mod_model_fit, clus_table, std.lv=TRUE)
summary(cfa_mod_fit, standardized=TRUE, fit.measures = T)

# factor loadings
inspect(cfa_mod_fit,what="std")$lambda
cfa_mod_fit_table <- standardizedsolution(cfa_mod_fit)
cfa_mod_fit_table

############################################# 2. Fitted Reliability & Validity Criteria ###############################################################
# See Paper: Rönkkö & Cho (2022)
# Average Variance extracted > 0.5  Cf. Parasuraman and Colby (2015)
# Alpha > 0.7 Cf. Nunnally & Bernstein (1978), Hair et al. (2009)
reliability(cfa_mod_fit)
discriminantValidity(cfa_mod_fit)

# HTMT Analysis for discriminant Validity (Cross-Loadings): CF. Henseler et al. (2015)
htmt_factors <- 
'Optimism =~ OPT2.x + OPT4.x
Innovativeness =~ INN1.x + INN2.x + INN4.x 
Insecurity =~ INS1.x + INS2.x + INS4.x
disposition_to_privacy =~ v_104.x + v_105.x + Reverse_v_106
Integrity =~ v_247.x + v_248.x + v_249.x
Benevolence =~ v_250.x + v_251.x + v_252.x
Ability =~ v_144.x + v_145.x + v_146.x
perceived_benefit_for_society =~ v_147.x + Reverse_v_148
perceived_risk =~ v_149.x + v_150.x
potential_of_disruption =~ v_151.x + v_152.x + v_153.x + Reverse_v_154
usage_intention =~ v_132.x + v_133.x
experience =~ v_10.x + v_11.x + v_286.x
'
# Note: Single-Item constructs are excluded as htmt computation can only be performend on multi-item constructs
htmt(model = htmt_factors, data = clus_table)
# 4 HTMTs > 0.9 -> some degree of cross-loadings -> However, VIF very low

# Fornell-Larcker Criterion TBD
csem()


############################################# 3. Fitted Correlations  ###############################################################
predictors_mod_fit <- cbind(optimism, innovativeness, discomfort_fitted, insecurity, clus_table[, .(Explorers)], clus_table[, .(Pioneers)],clus_table[, .(Hesitators)],clus_table[, .(Avoiders)], social, disp_priv, trust, perc_benf_s, percieved_risk, pot_disrup, usage_int,
                        age, gender, experience[, .(Experience)], poss_crypto)
cor_all <- apa.cor.table(predictors_mod_fit, show.conf.interval = FALSE, filename = "Cortable.doc")
cor_all

############################################# 4.0 Setup Regressions ############################################# 

gender$Gender <- as.factor(gender$Gender)
poss_crypto$Possession_of_cryptocurrency <- as.factor(poss_crypto$Possession_of_cryptocurrency)

predictors_all_NonModerated <- c("Explorers", "Pioneers" , "Hesitators", "Social_Influence", "Disposition_to_privacy", "Trust", "Percieved_risk",
                        "Perceived_benefit_for_society", "Potential_of_disruption", "Usage_Intention")

predictors_all_age <- c("Explorers*Age", "Pioneers*Age", "Hesitators*Age", "Social_Influence*Age", "Disposition_to_privacy*Age", "Trust*Age", "Percieved_risk*Age",
                        "Perceived_benefit_for_society*Age", "Potential_of_disruption*Age", "Usage_Intention*Age")

predictors_all_gender <- c("Explorers*Gender", "Pioneers*Gender", "Hesitators*Gender", "Social_Influence*Gender", "Disposition_to_privacy*Gender", "Trust*Gender", "Percieved_risk*Gender",
                        "Perceived_benefit_for_society*Gender", "Potential_of_disruption*Gender", "Usage_Intention*Gender")

predictors_all_experience <- c("Explorers*Experience", "Pioneers*Experience", "Hesitators*Experience", "Social_Influence*Experience", "Disposition_to_privacy*Experience", "Trust*Experience", "Percieved_risk*Experience",
                        "Perceived_benefit_for_society*Experience", "Potential_of_disruption*Experience", "Usage_Intention*Experience")

predictors_all_possCrypto <- c("Explorers*Possession_of_cryptocurrency", "Pioneers*Possession_of_cryptocurrency", "Hesitators*Possession_of_cryptocurrency", "Social_Influence*Possession_of_cryptocurrency", "Disposition_to_privacy*Possession_of_cryptocurrency", "Trust*Possession_of_cryptocurrency", "Percieved_risk*Possession_of_cryptocurrency",
                        "Perceived_benefit_for_society*Possession_of_cryptocurrency", "Potential_of_disruption*Possession_of_cryptocurrency", "Usage_Intention*Possession_of_cryptocurrency")

predictors_all_national <- 0

predictors_without_Usage_NonModerated <- c("Explorers", "Pioneers" , "Hesitators" ,"Social_Influence", "Disposition_to_privacy", "Trust", "Percieved_risk",
                                 "Perceived_benefit_for_society", "Potential_of_disruption", "Usefulness_Applications")

predictors_without_Usage_age <- c("Explorers*Age", "Pioneers*Age", "Hesitators*Age", "Social_Influence*Age", "Disposition_to_privacy*Age", "Trust*Age", "Percieved_risk*Age",
                        "Perceived_benefit_for_society*Age", "Potential_of_disruption*Age", "Usefulness_Applications*Age")

predictors_without_Usage_gender <- c("Explorers*Gender", "Pioneers*Gender", "Hesitators*Gender", "Social_Influence*Gender", "Disposition_to_privacy*Gender", "Trust*Gender", "Percieved_risk*Gender",
                           "Perceived_benefit_for_society*Gender", "Potential_of_disruption*Gender", "Usefulness_Applications*Gender")

predictors_without_Usage_experience <- c("Explorers*Experience", "Pioneers*Experience", "Hesitators*Experience", "Social_Influence*Experience", "Disposition_to_privacy*Experience", "Trust*Experience", "Percieved_risk*Experience",
                               "Perceived_benefit_for_society*Experience", "Potential_of_disruption*Experience", "Usefulness_Applications*Experience")

predictors_without_Usage_possCrypto <- c("Explorers*Possession_of_cryptocurrency", "Pioneers*Possession_of_cryptocurrency", "Hesitators*Possession_of_cryptocurrency", "Social_Influence*Possession_of_cryptocurrency", "Disposition_to_privacy*Possession_of_cryptocurrency", "Trust*Possession_of_cryptocurrency", "Percieved_risk*Possession_of_cryptocurrency",
                               "Perceived_benefit_for_society*Possession_of_cryptocurrency", "Potential_of_disruption*Possession_of_cryptocurrency", "Usefulness_Applications*Possession_of_cryptocurrency")

predictors_without_Usage_national <- 0

# Generalized function for regression
perform_linear_regression <- function(dependent_var, predictors, data){
  f_usefulness <- as.formula(paste(dependent_var, paste(predictors, collapse = " + "), sep = " ~ "))
  lm_usefulness_nonMod <- lm(f_usefulness, data)
}

#  Assessing the significance of a polynomial or interaction term is accomplished by evaluating
# incremental R2, not the significance of individual coefficients, due to high multicollinearity (Cf. Hair et al.2009)

############################################# 4.1 Regression: Usage Intention  ###############################################################

outcome_usage <- c("Usage_Intention")

# Calculating mean over all applications
clus_table[, Usefulness_Applications := round(rowMeans(clus_table[, .(v_265.x, v_266.x, v_267.x, v_268.x, v_269.x, v_270.x)], na.rm = T), 2)]
lm_data_usefulness <- cbind(predictors_mod_fit, clus_table[, .(Usefulness_Applications)])

# 0. Non-moderated
lm_usageIntention_nonMod <- perform_linear_regression(outcome_usage, predictors_without_Usage_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod)

# 1. Moderated by AGE
lm_usageIntention_age <- perform_linear_regression(outcome_usage, predictors_without_Usage_age, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod, lm_usageIntention_age)

# 2. Moderated by GENDER
lm_usageIntention_gender <- perform_linear_regression(outcome_usage, predictors_without_Usage_gender, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_gender)

# 3. Moderated by EXPERIENCE
lm_usageIntention_experience <- perform_linear_regression(outcome_usage, predictors_without_Usage_experience, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_usageIntention_crypto <- perform_linear_regression(outcome_usage, predictors_without_Usage_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_crypto)

plot(lm_usageIntention_nonMod)

############################################# 4.2 Regression: Usefulness Applications (Mean)  ###############################################################
# Setting Dependent variable
outcome_usefulness <- c("Usefulness_Applications")

# 0. Non-moderated
lm_usefulness_nonMod <- perform_linear_regression(outcome_usefulness, predictors_all_NonModerated, data = lm_data_usefulness )
apa.reg.table(lm_usefulness_nonMod)
car::vif(lm_usefulness_nonMod)

# 1. Moderated by AGE
lm_usefulness_age <- perform_linear_regression(outcome_usefulness, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_usefulness_nonMod, lm_usefulness_age)

# 2. Moderated by GENDER
lm_usefulness_gender <- perform_linear_regression(outcome_usefulness, predictors_all_gender, data = lm_data_usefulness )
apa.reg.table(lm_usefulness_nonMod, lm_usefulness_gender)

# 3. Moderated by EXPERIENCE
lm_usefulness_experience <- perform_linear_regression(outcome_usefulness, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_usefulness_nonMod, lm_usefulness_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_usefulness_crypto <- perform_linear_regression(outcome_usefulness, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_usefulness_nonMod, lm_usefulness_crypto)

# COMPARING MODELS
anova(lm_usefulness_nonMod, lm_usefulness_age)
anova(lm_usefulness_nonMod, lm_usefulness_gender)
anova(lm_usefulness_nonMod, lm_usefulness_experience)
anova(lm_usefulness_nonMod, lm_usefulness_crypto)

# Plots
# QQ-Plot: Do residuals follow gaussian assumption?
plot(lm_usefulness_nonMod) # Yes

# HIERARHICAL EXAMPLE
pred_one <- c("Optimism")
lm_one <- perform_linear_regression(outcome_usefulness, pred_one ,data = lm_data_usefulness )
apa.reg.table(lm_one)

pred_two <- c("Optimism", "Innovativeness")
lm_two <- perform_linear_regression(outcome_usefulness, pred_two ,data = lm_data_usefulness)
apa.reg.table(lm_two)

pred_three <- c("Optimism", "Innovativeness", "Discomfort")
lm_three <- perform_linear_regression(outcome_usefulness, pred_three ,data = lm_data_usefulness)
apa.reg.table(lm_three)

pred_four <- c("Optimism", "Innovativeness", "Discomfort", "Insecurity")
lm_four <- perform_linear_regression(outcome_usefulness, pred_four ,data = lm_data_usefulness)
apa.reg.table(lm_four)


############################################# 4.3 Regression: Usefulness Applications - Separate Applications  ###############################################################

#### 1. Tokenization of Assets ####
# Setup
outcome_token <- c("Usefulness_of_tokenization_of_assets")
lm_data_usefulness <- cbind(predictors_mod_fit, clus_table[, .(Usefulness_Applications)], clus_table[, .(Usefulness_of_tokenization_of_assets = v_265.x)])

# 0. Non-moderated
lm_token_nonMod <- perform_linear_regression(outcome_token, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod)
car::vif(lm_token_nonMod)

# 1. Moderated by AGE
lm_token_age <- perform_linear_regression(outcome_token, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod, lm_token_age)

# 2. Moderated by GENDER
lm_token_gender<- perform_linear_regression(outcome_token, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod, lm_token_gender)

# 3. Moderated by EXPERIENCE
lm_token_experience <- perform_linear_regression(outcome_token, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod, lm_token_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_token_crypto <- perform_linear_regression(outcome_token, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod, lm_token_crypto)


#### 2. Fractional Ownership ####
# Setup
outcome_fract <- c("Usefulness_of_fractional_ownership")
lm_data_usefulness <- cbind(lm_data_usefulness, clus_table[, .(Usefulness_of_fractional_ownership = v_266.x)])

# 0. Non-moderated
lm_fract_nonMod <- perform_linear_regression(outcome_fract, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod)
car::vif(lm_fract_nonMod)

# 1. Moderated by AGE
lm_fract_age <- perform_linear_regression(outcome_fract, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod, lm_fract_age)

# 2. Moderated by GENDER
lm_fract_gender<- perform_linear_regression(outcome_fract, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod, lm_fract_gender)

# 3. Moderated by EXPERIENCE
lm_fract_experience <- perform_linear_regression(outcome_fract, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod, lm_fract_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_fract_crypto <- perform_linear_regression(outcome_fract, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod, lm_fract_crypto)

#### 3. Self-Sovereign Identity ####
# Setup
outcome_self <- c("Usefulness_of_self_sovereign_identity")
lm_data_usefulness <- cbind(lm_data_usefulness, clus_table[, .(Usefulness_of_self_sovereign_identity = v_267.x)])

# 0. Non-moderated
lm_self_nonMod <- perform_linear_regression(outcome_self, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod)
car::vif(lm_self_nonMod)

# 1. Moderated by AGE
lm_self_age <- perform_linear_regression(outcome_self, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod, lm_self_age)

# 2. Moderated by GENDER
lm_self_gender<- perform_linear_regression(outcome_self, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod, lm_self_gender)

# 3. Moderated by EXPERIENCE
lm_self_experience <- perform_linear_regression(outcome_self, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod, lm_self_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_self_crypto <- perform_linear_regression(outcome_self, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod, lm_self_crypto)

#### 4. Smart Contracts ####
# Setup
outcome_smart <- c("Usefulness_of_smart_contracts")
lm_data_usefulness <- cbind(lm_data_usefulness, clus_table[, .(Usefulness_of_smart_contracts = v_268.x)])

# 0. Non-moderated
lm_smart_nonMod <- perform_linear_regression(outcome_smart, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod)
car::vif(lm_smart_nonMod)

# 1. Moderated by AGE
lm_smart_age <- perform_linear_regression(outcome_smart, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod, lm_smart_age)

# 2. Moderated by GENDER
lm_smart_gender<- perform_linear_regression(outcome_smart, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod, lm_smart_gender)

# 3. Moderated by EXPERIENCE
lm_smart_experience <- perform_linear_regression(outcome_smart, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod, lm_smart_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_smart_crypto <- perform_linear_regression(outcome_smart, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod, lm_smart_crypto)


#### 5. Micropayments ####
# Setup
outcome_micro <- c("Usefulness_of_micropayments")
lm_data_usefulness <- cbind(lm_data_usefulness, clus_table[, .(Usefulness_of_micropayments = v_269.x)])

# 0. Non-moderated
lm_micro_nonMod <- perform_linear_regression(outcome_micro, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod)
car::vif(lm_micro_nonMod)

# 1. Moderated by AGE
lm_micro_age <- perform_linear_regression(outcome_micro, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod, lm_micro_age)

# 2. Moderated by GENDER
lm_micro_gender<- perform_linear_regression(outcome_micro, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod, lm_micro_gender)

# 3. Moderated by EXPERIENCE
lm_micro_experience <- perform_linear_regression(outcome_micro, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod, lm_micro_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_micro_crypto <- perform_linear_regression(outcome_micro, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod, lm_micro_crypto)

anova(lm_micro_nonMod, lm_micro_crypto)

#### 6. Anonymous Transactions ####
# Setup
outcome_anony <- c("Usefulness_of_anonymous_transactions")
lm_data_usefulness <- cbind(lm_data_usefulness, clus_table[, .(Usefulness_of_anonymous_transactions = v_270.x)])

# 0. Non-moderated
lm_anony_nonMod <- perform_linear_regression(outcome_anony, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod)
car::vif(lm_anony_nonMod)

# 1. Moderated by AGE
lm_anony_age <- perform_linear_regression(outcome_anony, predictors_all_age, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod, lm_anony_age)

# 2. Moderated by GENDER
lm_anony_gender<- perform_linear_regression(outcome_anony, predictors_all_gender, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod, lm_anony_gender)

# 3. Moderated by EXPERIENCE
lm_anony_experience <- perform_linear_regression(outcome_anony, predictors_all_experience, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod, lm_anony_experience)

# 4. Moderated by POSESSION OF CRYPTO
lm_anony_crypto <- perform_linear_regression(outcome_anony, predictors_all_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod, lm_anony_crypto)










############################################# Cleaning ###############################################################

# Clean Environment
rm(list = ls())







