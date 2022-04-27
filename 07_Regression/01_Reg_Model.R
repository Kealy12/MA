#######################################################################################################################################

# moderated multiple linear regression analysis

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
library(broom)

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

############################################# Independent / Dependent Variables ###############################################################
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

# Adjusting Discomfort to fit CFA: Deleted DIS3; Only DIS2 relevant for Discomfort
discomfort_fitted <- quest_reg_mod[, .(Discomfort_fit = DIS2)]
discomfort_fitted <- cbind(discomfort, discomfort_fitted)
discomfort_fitted[, Discomfort := NULL]
colnames(discomfort_fitted) <- c("Discomfort")
discomfort_fitted

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

# Calculate Average Integrity:
quest_reg_mod[, Integrity := round(rowMeans(quest_reg_mod[, .(v_247, v_248, v_249)], na.rm = T), 2)]

## Benevolence: v_250, v_251, v_252
quest_reg_mod[, .(v_250, v_251, v_252)]

# can be no 0s
quest_reg_mod[v_250 == 0, v_250 := NA]
quest_reg_mod[v_251 == 0, v_251 := NA]
quest_reg_mod[v_252 == 0, v_252 := NA]

# Calculate Average Benevolence:
quest_reg_mod[, Benevolence := round(rowMeans(quest_reg_mod[, .(v_250, v_251, v_252)], na.rm = T), 2)]

## Ability: v_144, v_145, v_146
quest_reg_mod[, .(v_144, v_145, v_146)]

# can be no 0s
quest_reg_mod[v_144 == 0, v_144 := NA]
quest_reg_mod[v_145 == 0, v_145 := NA]
quest_reg_mod[v_146 == 0, v_146 := NA]

# Calculate Average Ability:
quest_reg_mod[, Ability := round(rowMeans(quest_reg_mod[, .(v_144, v_145, v_146)], na.rm = T), 2)]


# Score overall
quest_reg_mod[, .("T_INT score overall" = round(mean(Integrity, na.rm = T),2))]
quest_reg_mod[, .("T_BEN score overall" = round(mean(Benevolence, na.rm = T),2))]
quest_reg_mod[, .("T_ABI score overall" = round(mean(Ability, na.rm = T),2))]

# Disposition to trust:
quest_reg_mod[, Trust := round(rowMeans(quest_reg_mod[, .(Integrity, Benevolence, Ability)], na.rm = T), 2)]
trust <- quest_reg_mod[, .(Trust)]

quest_reg_mod[, .("Tust in BT score overall" = round(mean(Trust, na.rm = T),2))]


#### Perceived risk ####

# v_149, v_150: Likert (1-7)
# can be no 0s
quest_reg_mod[v_149 == 0, v_149 := NA]
quest_reg_mod[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_reg_mod[, Perceived_risk := round(rowMeans(quest_reg_mod[, .(v_149, v_150)], na.rm = T), 2)]

Perceived_risk <- quest_reg_mod[, .(Perceived_risk)]

# Score overall
quest_reg_mod[, .("Perceived risk score overall" = round(mean(Perceived_risk, na.rm = T),2))]

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


#### Usefulness ####
quest_reg_mod[, Perceived_Usefulness := round(rowMeans(quest_reg_mod[, .(v_265, v_266, v_267, v_268, v_269, v_270)], na.rm = T), 2)]
usefulness <- quest_reg_mod[, .(Perceived_Usefulness)]



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

# rescaling to level all scales to 1-7
knowledge[, Knowledge_of_BT := (Knowledge_of_BT * (2/3) + (1/3))]

# Binding together and creating mean
experience <- cbind(contact, knowledge)
experience[, Experience := round(rowMeans(experience[, .(Contact_in_professional_life, Contact_in_personal_life, Knowledge_of_BT)], na.rm = T), 2)]

# FITTING Experience: Delete v10 due to Loading < 0.5
contact[, Contact_in_professional_life := NULL]
experience_fitted <- cbind(contact, knowledge)
experience_fitted[, Experience := round(rowMeans(experience_fitted[, .(Contact_in_personal_life, Knowledge_of_BT)], na.rm = T), 2)]


#### Possession of crypto ####
# v_54: (yes = 1, no = 0)
quest_reg_mod <- quest_reg_mod[v_54 == 2, v_54 := 0]
table(quest_reg_mod[, .(v_54)])
poss_crypto <- quest_reg_mod[, .(v_54)]
colnames(poss_crypto) <- c("Possession_of_cryptocurrency")



############################################# Preliminary CFA ###############################################################

cfa_mod_model <- 
'optimism =~ OPT2 + OPT4
innovativeness =~ INN1 + INN2 + INN4 
discomfort =~ DIS2 + DIS3
insecurity =~ INS1 + INS2 + INS4
disposition_to_privacy =~ v_104 + v_105 + Reverse_v_106
trust =~ v_247 + v_248 + v_249 + v_250 + v_251 + v_252 + v_144 + v_145 + v_146
perceived_risk =~ v_149 + v_150
perceived_benefit_for_society =~ v_147 + Reverse_v_148
potential_of_disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
usefulness =~ v_265 + v_266 + v_267 + v_268 + v_269 + v_270
usage_intention =~ v_132 + v_133
experience =~ v_10 + v_11 + v_286
'
# Non-Factors:
# tri =~ optimism + innovativeness + discomfort + insecurity
# integrity =~ v_247 + v_248 + v_249
# benevolence =~ v_250 + v_251 + v_252
# ability =~ v_144 + v_145 + v_146
# social_influence =~ v_296
# age =~ v_285
# gender =~ v_169
# possession_of_cryptocurrency =~ v_54

# Performing CFA
cfa_mod <- cfa(cfa_mod_model, quest_reg_mod, std.lv=TRUE)
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
predictors_mod <- cbind(optimism, innovativeness, discomfort, insecurity, social, disp_priv, trust, Perceived_risk, perc_benf_s, pot_disrup, 
                        usefulness, usage_int, age, gender, experience[, .(Experience)], poss_crypto)
cor_all <- apa.cor.table(predictors_mod, show.conf.interval = FALSE)
cor_all

############################################# Adjustments  ###############################################################
# Cutoff value on factor loadings < 0.5 (Hair et al. 2009) -> DIS3 = 0.292 deleted, v10 of Experience = 0.495 deleted
# AVE < 0.5 -> DIS

# Discomfort only consists of DIS2
quest_reg_mod

############################################# 1. Fitted CFA  ###############################################################

cfa_mod_model_fit <- 
'optimism =~ OPT2 + OPT4
innovativeness =~ INN1 + INN2 + INN4 
insecurity =~ INS1 + INS2 + INS4
disposition_to_privacy =~ v_104 + v_105 + Reverse_v_106
trust =~ v_247 + v_248 + v_249 +  v_250 + v_251 + v_252 + v_144 + v_145 + v_146
perceived_risk =~ v_149 + v_150
perceived_benefit_for_society =~ v_147 + Reverse_v_148
potential_of_disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
usefulness =~ v_265 + v_266 + v_267 + v_268 + v_269 + v_270
usage_intention =~ v_132 + v_133
experience =~ v_11 + v_286
'
# Note: Single-item constructs are excluded, as reliability & validity cannot be computed (Hair et al.(2009))
# tri =~ optimism + innovativeness + discomfort + insecurity
# discomfort =~ DIS2
# integrity =~ v_247 + v_248 + v_249
# benevolence =~ v_250 + v_251 + v_252
# ability =~ v_144 + v_145 + v_146
cfa_mod_fit <- cfa(cfa_mod_model_fit, quest_reg_mod, std.lv=TRUE)
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

# Multicollinearity: Irrelevant for moderators (McClelland 2017)
# Run AFTER regression was performed
car::vif(lm_usageIntention_nonMod)


# Heterotrait-monotrait (HTMT) ratio of correlations: Analysis for discriminant Validity (Cross-Loadings): CF. Henseler et al. (2015)
htmt_factors <- 
'Optimism =~ OPT2 + OPT4
Innovativeness =~ INN1 + INN2 + INN4 
Insecurity =~ INS1 + INS2 + INS4
disposition_to_privacy =~ v_104 + v_105 + Reverse_v_106
trust =~ v_247 + v_248 + v_249 + v_250 + v_251 + v_252 + v_144 + v_145 + v_146
perceived_risk =~ v_149 + v_150
perceived_benefit_for_society =~ v_147 + v_148
potential_of_disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
usefulness =~ v_265 + v_266 + v_267 + v_268 + v_269 + v_270
usage_intention =~ v_132 + v_133
experience =~ v_11 + v_286
'
# Integrity =~ v_247 + v_248 + v_249
# Benevolence =~ v_250 + v_251 + v_252
# Ability =~ v_144 + v_145 + v_146

# Note: Single-Item constructs are excluded as htmt computation can only be performend on multi-item constructs
htmt_result <- htmt(model = htmt_factors, data = quest_reg_mod)
htmt_result

# htmt_result <- as.data.table(htmt_result)
# 4 HTMTs > 0.9 -> some degree of cross-loadings -> However, VIF very low

# # Exporting HTMT results:
# print(xtable(htmt_result, type = "latex"), file = "./../02_Output/htmt_results.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(htmt_result)))
# sheets <- list("htmt_result" = htmt_result)
# write_xlsx(sheets, "./../02_Output/htmt_results.xlsx")
# 

############################################# 3. Fitted Correlations  ###############################################################
predictors_mod_fit <- cbind(optimism, innovativeness, discomfort_fitted, insecurity, social, disp_priv, trust, Perceived_risk, perc_benf_s, pot_disrup, 
                            usefulness, usage_int, age, gender, experience_fitted[, .(Experience)], poss_crypto)
cor_all <- apa.cor.table(predictors_mod_fit, show.conf.interval = FALSE, filename = "Cortable.doc")
cor_all

############################################# 4.0 Setup Regressions ############################################# 

gender$Gender <- as.factor(gender$Gender)
poss_crypto$Possession_of_cryptocurrency <- as.factor(poss_crypto$Possession_of_cryptocurrency)

predictors_all_NonModerated <- c("Optimism", "Innovativeness", "Discomfort", "Insecurity", "Social_Influence", "Disposition_to_privacy", "Trust", "Perceived_risk",
                        "Perceived_benefit_for_society", "Potential_of_disruption")

predictors_all_age <- c("Optimism*Age", "Innovativeness*Age", "Discomfort*Age", "Insecurity*Age", "Social_Influence*Age", "Disposition_to_privacy*Age", "Trust*Age", "Perceived_risk*Age",
                        "Perceived_benefit_for_society*Age", "Potential_of_disruption*Age")

predictors_all_gender <- c("Optimism*Gender", "Innovativeness*Gender", "Discomfort*Gender", "Insecurity*Gender", "Social_Influence*Gender", "Disposition_to_privacy*Gender", "Trust*Gender", "Perceived_risk*Gender",
                        "Perceived_benefit_for_society*Gender", "Potential_of_disruption*Gender")

predictors_all_experience <- c("Optimism*Experience", "Innovativeness*Experience", "Discomfort*Experience", "Insecurity*Experience", "Social_Influence*Experience", "Disposition_to_privacy*Experience", "Trust*Experience", "Perceived_risk*Experience",
                        "Perceived_benefit_for_society*Experience", "Potential_of_disruption*Experience")

predictors_all_possCrypto <- c("Optimism*Possession_of_cryptocurrency", "Innovativeness*Possession_of_cryptocurrency", "Discomfort*Possession_of_cryptocurrency", "Insecurity*Possession_of_cryptocurrency", "Social_Influence*Possession_of_cryptocurrency", "Disposition_to_privacy*Possession_of_cryptocurrency", "Trust*Possession_of_cryptocurrency", "Perceived_risk*Possession_of_cryptocurrency",
                        "Perceived_benefit_for_society*Possession_of_cryptocurrency", "Potential_of_disruption*Possession_of_cryptocurrency")

##

predictors_without_Usage_NonModerated <- c("Optimism", "Innovativeness", "Discomfort", "Insecurity", "Social_Influence", "Disposition_to_privacy", "Trust", "Perceived_risk",
                                 "Perceived_benefit_for_society", "Potential_of_disruption", "Perceived_Usefulness")

predictors_without_Usage_age <- c("Optimism*Age", "Innovativeness*Age", "Discomfort*Age", "Insecurity*Age", "Social_Influence*Age", "Disposition_to_privacy*Age", "Trust*Age", "Perceived_risk*Age",
                        "Perceived_benefit_for_society*Age", "Potential_of_disruption*Age", "Perceived_Usefulness*Age")

predictors_without_Usage_gender <- c("Optimism*Gender", "Innovativeness*Gender", "Discomfort*Gender", "Insecurity*Gender", "Social_Influence*Gender", "Disposition_to_privacy*Gender", "Trust*Gender", "Perceived_risk*Gender",
                           "Perceived_benefit_for_society*Gender","Potential_of_disruption*Gender", "Perceived_Usefulness*Gender")

predictors_without_Usage_experience <- c("Optimism*Experience", "Innovativeness*Experience", "Discomfort*Experience", "Insecurity*Experience", "Social_Influence*Experience", "Disposition_to_privacy*Experience", "Trust*Experience", "Perceived_risk*Experience",
                               "Perceived_benefit_for_society*Experience", "Potential_of_disruption*Experience", "Perceived_Usefulness*Experience")

predictors_without_Usage_possCrypto <- c("Optimism*Possession_of_cryptocurrency", "Innovativeness*Possession_of_cryptocurrency", "Discomfort*Possession_of_cryptocurrency", "Insecurity*Possession_of_cryptocurrency", "Social_Influence*Possession_of_cryptocurrency", "Disposition_to_privacy*Possession_of_cryptocurrency", "Trust*Possession_of_cryptocurrency", "Perceived_risk*Possession_of_cryptocurrency",
                               "Perceived_benefit_for_society*Possession_of_cryptocurrency","Potential_of_disruption*Possession_of_cryptocurrency", "Perceived_Usefulness*Possession_of_cryptocurrency")

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
lm_data_usefulness <- copy(predictors_mod_fit)

# 0. Non-moderated
lm_usageIntention_nonMod <- perform_linear_regression(outcome_usage, predictors_without_Usage_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod)
summary(lm_usageIntention_nonMod)
write.csv(tidy(lm_usageIntention_nonMod), "non_moderated.csv")

# 1. Moderated by AGE
lm_usageIntention_age <- perform_linear_regression(outcome_usage, predictors_without_Usage_age, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod, lm_usageIntention_age)
summary(lm_usageIntention_age)
write.csv(tidy(lm_usageIntention_age), "age_moderated.csv")

# 2. Moderated by GENDER
lm_usageIntention_gender <- perform_linear_regression(outcome_usage, predictors_without_Usage_gender, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_gender)
summary(lm_usageIntention_gender)
write.csv(tidy(lm_usageIntention_gender), "gender_moderated.csv")

# 3. Moderated by EXPERIENCE
lm_usageIntention_experience <- perform_linear_regression(outcome_usage, predictors_without_Usage_experience, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_experience)
write.csv(tidy(lm_usageIntention_experience), "exp_moderated.csv")
summary(lm_usageIntention_experience)


# 4. Moderated by POSESSION OF CRYPTO
lm_usageIntention_crypto <- perform_linear_regression(outcome_usage, predictors_without_Usage_possCrypto, data = lm_data_usefulness)
apa.reg.table(lm_usageIntention_nonMod,lm_usageIntention_crypto)
write.csv(tidy(lm_usageIntention_crypto), "poc_moderated.csv")
summary(lm_usageIntention_crypto)

# QQ-Plot: Do residuals follow gaussian assumption?
plot(lm_usageIntention_nonMod) # Yes


############################################# 4.2 Regression: Usefulness Applications (Mean)  ###############################################################
# Setting Dependent variable
outcome_usefulness <- c("Perceived_Usefulness")

# 0. Non-moderated
lm_usefulness_nonMod <- perform_linear_regression(outcome_usefulness, predictors_all_NonModerated, data = lm_data_usefulness )
apa.reg.table(lm_usefulness_nonMod)
car::vif(lm_usefulness_nonMod)

# NO MODERATION EFFECT ON USEFULNESS FOR PAPER

############################################# 4.3 Regression: Usefulness Applications - Separate Applications  ###############################################################


#### 1. Tokenization of Assets ####
# Setup
outcome_token <- c("Usefulness_of_tokenization_of_assets")
lm_data_usefulness <- cbind(predictors_mod_fit, quest_reg_mod[, .(Perceived_Usefulness)], quest_reg_mod[, .(Usefulness_of_tokenization_of_assets = v_265)])

# Non-moderated
lm_token_nonMod <- perform_linear_regression(outcome_token, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_token_nonMod)
summary(lm_token_nonMod)
write.csv(tidy(lm_token_nonMod), "usefulness_token.csv")

#### 2. Fractional Ownership ####
# Setup
outcome_fract <- c("Usefulness_of_fractional_ownership")
lm_data_usefulness <- cbind(lm_data_usefulness, quest_reg_mod[, .(Usefulness_of_fractional_ownership = v_266)])

# Non-moderated
lm_fract_nonMod <- perform_linear_regression(outcome_fract, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_fract_nonMod)
summary(lm_fract_nonMod)
write.csv(tidy(lm_fract_nonMod), "usefulness_fow.csv")

#### 3. Self-Sovereign Identity ####
# Setup
outcome_self <- c("Usefulness_of_self_sovereign_identity")
lm_data_usefulness <- cbind(lm_data_usefulness, quest_reg_mod[, .(Usefulness_of_self_sovereign_identity = v_267)])

# Non-moderated
lm_self_nonMod <- perform_linear_regression(outcome_self, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_self_nonMod)
summary(lm_self_nonMod)
write.csv(tidy(lm_self_nonMod), "usefulness_ssi.csv")

#### 4. Smart Contracts ####
# Setup
outcome_smart <- c("Usefulness_of_smart_contracts")
lm_data_usefulness <- cbind(lm_data_usefulness, quest_reg_mod[, .(Usefulness_of_smart_contracts = v_268)])

# Non-moderated
lm_smart_nonMod <- perform_linear_regression(outcome_smart, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_smart_nonMod)
summary(lm_smart_nonMod)
write.csv(tidy(lm_smart_nonMod), "usefulness_sco.csv")

#### 5. Micropayments ####
# Setup
outcome_micro <- c("Usefulness_of_micropayments")
lm_data_usefulness <- cbind(lm_data_usefulness, quest_reg_mod[, .(Usefulness_of_micropayments = v_269)])

# Non-moderated
lm_micro_nonMod <- perform_linear_regression(outcome_micro, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_micro_nonMod)
summary(lm_micro_nonMod)
write.csv(tidy(lm_micro_nonMod), "usefulness_mpy.csv")

#### 6. Anonymous Transactions ####
# Setup
outcome_anony <- c("Usefulness_of_anonymous_transactions")
lm_data_usefulness <- cbind(lm_data_usefulness, quest_reg_mod[, .(Usefulness_of_anonymous_transactions = v_270)])

# Non-moderated
lm_anony_nonMod <- perform_linear_regression(outcome_anony, predictors_all_NonModerated, data = lm_data_usefulness)
apa.reg.table(lm_anony_nonMod)
summary(lm_anony_nonMod)
write.csv(tidy(lm_anony_nonMod), "usefulness_atr.csv")


############################################# Cleaning ###############################################################
# Clean Environment
rm(list = ls())







