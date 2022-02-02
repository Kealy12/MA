#######################################################################################################################################

# Multivariate Linear Regression on Application Scores 

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

############################################# Extracting relevant predictors ###############################################################

#### Have you heard of BT ####
# (yes = 1,no = 0): v_321
quest_reg <- quest_reg[v_321 == 2, v_321 := 0]
p1 <- quest_reg[, .(v_321)]
colnames(p1) <- c("Heard_of_BT")

#### Contact with BT ####

# v_10: in professional life
# v_11: in personal life
contact<- quest_clean[, c("v_10", "v_11")]
colnames(contact) <- c("Contact_in_professional_life", "Contact_in_personal_life")

#### Knowledge of BT  ####
# (1-10 scale): v_286
p2 <- quest_reg[, .(v_286)]
colnames(p2) <- c("Knowledge_of_BT")

#### Possession of Crypto ####
# v_54: (yes = 1, no = 0)
quest_reg <- quest_reg[v_54 == 2, v_54 := 0]
table(quest_reg[, .(v_54)])
p3 <- quest_reg[, .(v_54)]
colnames(p3) <- c("Possess_Crypto")





####### Constructs ########

#### TRI: Overall TRI (1-7) ####
quest_reg[, .(`Overall_TRI`)] # using single items for CFA
p4 <- quest_reg[, .(`Overall_TRI`)]

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

p5 <- quest_reg[, .(DISPPRIV)]

# Score overall
quest_reg[, .("Privacy score overall" = round(mean(DISPPRIV, na.rm = T),2))]




#### Usage Intention ####
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_reg[v_132 == 0, v_132 := NA]
quest_reg[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_reg[, Usage_Int := round(rowMeans(quest_reg[, .(v_132, v_133)], na.rm = T), 2)]

p6 <- quest_reg[, .(Usage_Int)]

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
quest_reg[, DISPTRUST := round(rowMeans(quest_reg[, .(trust_INT, trust_BEN, trust_ABI)], na.rm = T), 2)]
p7 <- quest_reg[, .(DISPTRUST)]

quest_reg[, .("Disposition to trust score overall" = round(mean(DISPTRUST, na.rm = T),2))]


#### Perceived benefit for society ####

# v_147, v_148: Likert (1-7)
# can be no 0s
quest_reg[v_147 == 0, v_147 := NA]
quest_reg[v_148 == 0, v_148 := NA]

# Reverse code v_148
quest_reg[, Reverse_v_148 := 8 - v_148]

# Calculate Average 
quest_reg[, Perc_Benefit := round(rowMeans(quest_reg[, .(v_147, Reverse_v_148)], na.rm = T), 2)]

p8 <- quest_reg[, .(Perc_Benefit)]

# Score overall
quest_reg[, .("Perceived benefit for society score overall" = round(mean(Perc_Benefit, na.rm = T),2))]


#### Perceived Risk ####

# v_149, v_150: Likert (1-7)
# can be no 0s
quest_reg[v_149 == 0, v_149 := NA]
quest_reg[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_reg[, Perc_Risk := round(rowMeans(quest_reg[, .(v_149, v_150)], na.rm = T), 2)]

p9 <- quest_reg[, .(Perc_Risk)]

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

p10 <- quest_reg[, .(Pot_Dis)]

# Score overall
quest_reg[, .("Potential of disruption score overall" = round(mean(Pot_Dis, na.rm = T),2))]













#### Excluded ####
#### Knowledge of difference between Bitcoin and Blockchain  ####
# (yes = 1, no = 0): v_282
quest_reg <- quest_reg[v_282 == 2, v_282 := 0]
table(quest_reg[, .(v_282)])

quest_reg






############################################# CFA  ###############################################################

factors <- 
'Disposition_Privacy =~ v_104 + v_105 + v_106
Useage_Intent =~ v_132 + v_133
integrity =~ v_247 + v_248 + v_249
benevolence =~ v_250 + v_251 + v_252
ability =~ v_144 + v_145 + v_146
Trust_in_BT =~ integrity + benevolence + ability
Perceived_Risk =~ v_149 + v_150
Perceived_Benefit_S =~ v_147 + Reverse_v_148
Potential_Disruption =~ v_151 + v_152 + v_153 + Reverse_v_154
TRI =~ Opt + Inn + Dis + Ins
Opt =~ OPT2 + OPT4
Inn =~ INN1 + INN2 + INN4 
Dis =~ DIS2 + DIS3
Ins =~ INS1 + INS2 + INS4'

# Performing CFA
cfa <- cfa(factors, quest_reg, std.lv=TRUE)
summary(cfa, standardized=TRUE, fit.measures = T)

# factor loadings
inspect(cfa,what="std")$lambda

#parameterEstimates(cfa)
#semPaths(cfa, "std")
#residuals(cfa)$cov

##### Exkurs - EFA ####
# Performing EFA only on items and contstructs 
cor_pred <- cor(predictors)

efa_5 <- fa(cor_pred, nfactors = 6, rotate = "varimax")
efa_3 <- fa(cor_pred, nfactors = 3, rotate = "varimax")
efa_2 <- fa(cor_pred, nfactors = 2, rotate = "varimax")

efa_5$loadings
efa_3$loadings

fa.diagram(efa_5)
fa.diagram(efa_4)
fa.diagram(efa_3)

fa(r = cor_dt, nfactors = 3)

scree(cor_pred, factors = FALSE)

# semPaths(predictors, "std")

############################################# Correlation Table  ###############################################################
predictors_all <- cbind(p1,p2,p3,contact,p4,p5,p6,p7,p8,p9,p10)

ggcorr(predictors_all, geom = "circle")
cor_all <- apa.cor.table(predictors_all, show.conf.interval = F)
cor_all

# Reduced
predictors_red <- cbind(p2,p3,contact,p4,p5,p6,p7,p9,p10)
cor_red <- apa.cor.table(predictors_red, show.conf.interval = F)
cor_red


############################################# Regression analysis - separate Applications  ###############################################################


#### 1. Tokenization of Assets ####
token_pred <- cbind(predictors_red, quest_reg[, .(v_265)])
lm_token <- lm(v_265 ~  Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = token_pred)

apa.reg.table(lm_token)

token_pred_red <- cbind(predictors_red, quest_reg[, .(v_265)])
lm_token_red <- lm(v_265 ~  Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Risk + Contact_in_professional_life + Contact_in_personal_life, data = token_pred_red)

apa.reg.table(lm_token_red)

anova(lm_token_red, lm_token)
# anova yields that full model models the data significantly better than reduced model 

ggplot(token_pred, aes(Heard_of_BT, v_265)) + geom_jitter() + geom_smooth(method = "lm")

# H1: Usage intention has a positive effect on usefulness of tokenization of assets


#### 2. Fractional Ownership ####
fract_pred <- cbind(predictors_red, quest_reg[, .(v_266)])
lm_fract <- lm(v_266 ~ Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = fract_pred)
apa.reg.table(lm_fract)

#### 3. Self-Sovereign Identity ####
self_pred <- cbind(predictors_red, quest_reg[, .(v_267)])
lm_self <- lm(v_267 ~ Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = self_pred)
apa.reg.table(lm_self)

#### 4. Smart Contracts ####
smart_pred <- cbind(predictors_red, quest_reg[, .(v_268)])
lm_smart <- lm(v_268 ~  Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = smart_pred)
apa.reg.table(lm_smart)

#### 5. Micropayments ####
micro_pred <- cbind(predictors_all, quest_reg[, .(v_269)])
lm_micro <- lm(v_269 ~ Heard_of_BT + Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Benefit + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life , data = micro_pred)
apa.reg.table(lm_micro)

#### 6. Anonymous Transactions ####
anony_pred <- cbind(predictors_all, quest_reg[, .(v_270)])
lm_anony <- lm(v_270 ~ Heard_of_BT + Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Benefit + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = anony_pred)
apa.reg.table(lm_anony)























############################################# Regression analysis - mean Applications Overall  ###############################################################

# Calculating mean over all applications
quest_reg[, Mean_all_Applic := round(rowMeans(quest_reg[, .(v_265, v_266, v_267, v_268, v_269, v_270)], na.rm = T), 2)]

overall_pred <- cbind(predictors_all, quest_reg[, .(Mean_all_Applic)])
lm_overall <- lm(Mean_all_Applic ~ Heard_of_BT + Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                 Usage_Int + DISPTRUST + Perc_Benefit + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, data = overall_pred)

apa.reg.table(lm_overall)

############################################# Regression analysis - Intention to use BT (pre-survey) ###############################################################

intention_pre <- quest_clean[, .(v_28)]
intention_pre <- intention_pre[v_28 == 2, v_28 := 0]
# Turning "Don't knows" into "Nos"
intention_pre <- intention_pre[v_28 == 3, v_28 := 0]
colnames(intention_pre) <- c("Intention_to_use_BT")

#### Logistic Regression, as Y is categorical 
intention_pred <- cbind(predictors_all, intention_pre)

lm_intention <- glm(Intention_to_use_BT ~ Heard_of_BT + Knowledge_of_BT + Possess_Crypto + Overall_TRI + DISPPRIV +
                    DISPTRUST + Perc_Benefit + Perc_Risk + Pot_Dis + Contact_in_professional_life + Contact_in_personal_life, 
                   data = intention_pred,
                   family = "binomial")

summary(lm_intention)





############################################# Regression analysis - Knowledge of BT (post-survey)  ###############################################################




###### Save and Clean ####

# Clean Environment
rm(list = ls())

