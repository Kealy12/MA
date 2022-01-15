#######################################################################################################################################

# Descriptive Stats on blockchain applications and clusters

################################################################ Set Up ###############################################################

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
library(MASS)
library(likert)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/lca_5class_analysis.RData")
load("./../01_Input/01_RData/lca_4class_analysis.RData")

# Binding questionnaire to tri cluster data 
quest_tri_extended <- cbind(quest_clean, tri_comp_all)
quest_tri_extended

# Filter out respondents who had invalid answers on TRI questions, LCA only worked on non-NA answers
quest_tri_extended <- quest_tri_extended[complete.cases(quest_tri_extended)]

# Map clusters to remaining data
quest_tri_extended <- cbind(quest_tri_extended, tri_comp_all_noNA[, "Predicted Class (4Cs)"], 
                            tri_comp_all_noNA[, "Predicted Class (5Cs)"])

# Renaming for further usage
setnames(quest_tri_extended, "Predicted Class (5Cs)", "Predicted_Class_5C")
setnames(quest_tri_extended, "Predicted Class (4Cs)", "Predicted_Class_4C")

# Need to encode clusters as factors
quest_tri_extended$Predicted_Class_5C <- as.factor(quest_tri_extended$Predicted_Class_5C)
quest_tri_extended$Predicted_Class_4C <- as.factor(quest_tri_extended$Predicted_Class_4C)

########## SANITY CHECKS ##########
# 0s on TRI answers in questionnaire need to be NA (they are invalid)
quest_tri_extended[v_108 == 0, v_108 := NA]
quest_tri_extended[v_109 == 0, v_109 := NA]
quest_tri_extended[v_207 == 0, v_207 := NA]
quest_tri_extended[v_208 == 0, v_208 := NA]
quest_tri_extended[v_209 == 0, v_209 := NA]
quest_tri_extended[v_228 == 0, v_228 := NA]
quest_tri_extended[v_229 == 0, v_229 := NA]
quest_tri_extended[v_230 == 0, v_230 := NA]
quest_tri_extended[v_231 == 0, v_231 := NA]
quest_tri_extended[v_232 == 0, v_232 := NA]

mean(quest_tri_extended$v_209, na.rm = T)
mean(quest_tri_extended$INN4, na.rm = T)
sum(is.na(quest_tri_extended)) # Total of 28 NAs
rownames(quest_tri_extended)[!complete.cases(quest_tri_extended)] # Indices of rows with NAs
quest_tri_extended[rowSums(is.na(quest_tri_extended)) > 0] # Overview of rows with NA


################################################################ Scores on Blockchain applications ###############################################################

# Extract Application columns
apps <- quest_tri_extended[, .(v_265, v_266, v_267, v_268, v_269, v_270, Predicted_Class_5C, Predicted_Class_4C)]

apps <- melt(apps, id.vars = c("Predicted_Class_5C", "Predicted_Class_4C"), 
     variable.name = "Applications", value.name = "Score_Factor")

# Need Score as Factor (String) and as Integer for Calculations
apps$Score_Factor <- as.factor(apps$Score_Factor)
apps[, Score_Int := as.numeric(Score_Factor)]

apps

# Renaming
apps[Applications == "v_265", Applications := "Tokenization of Assets"]
apps[Applications == "v_266", Applications := "Fractional ownership"]
apps[Applications == "v_267", Applications := "Self-Sovereign Identity"]
apps[Applications == "v_268", Applications := "Smart Contracts"]
apps[Applications == "v_269", Applications := "Micropayments"]
apps[Applications == "v_270", Applications := "Anonymous Transactions"]

# 7-point Likert Scale: Adding Scores as Strings
apps[Score_Factor == 1, Score_String :="Not usefull at all"]
apps[Score_Factor == 2, Score_String :="Not usefull"]
apps[Score_Factor == 3, Score_String :="Somewhat not usefull"]
apps[Score_Factor == 4, Score_String :="Neutral"]
apps[Score_Factor == 5, Score_String :="Somewhat usefull"]
apps[Score_Factor == 6, Score_String :="Usefull"]
apps[Score_Factor == 7, Score_String :="Very usefull"]

# % distribution of answers
apps[Score_Factor == 1 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 2 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 3 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 4 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 5 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 6 , value := .N / nrow(quest_tri_extended), by = "Applications"]
apps[Score_Factor == 7 , value := .N / nrow(quest_tri_extended), by = "Applications"]

apps$Score_String <- factor(apps$Score_String , levels = c("Not usefull at all", "Not usefull", "Somewhat not usefull", "Neutral", 
                                                           "Somewhat usefull", "Usefull", "Very usefull"))
apps[is.na(Score_Factor)]

apps_summary <- unique(apps[ ,c("Applications", "Score_String", "value")])

apps_summary

#### Scores Overall ####
# Mean Likert Scores of Applications
apps_means <- as.data.table(aggregate(Score_Int ~  Applications, apps, mean))
apps_means$Score_Int <- round(apps_means$Score_Int, 2)
apps_means[order(-Score_Int)]

# Visualization of Scores of each Application
ggplot(apps, aes(Applications, Score_Int)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  geom_text(data = apps_means, aes(label = Score_Int, y = Score_Int + 0.3), size = 3)

# Visualization of answers (%) on applications 
apps_summary <- merge(apps_summary, apps_means, by = "Applications")

ggplot(apps_summary, aes(x = reorder(Applications, Score_Int), y = value, 
                         fill = Score_String)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "PuOr")



  

#### Scores per Cluster ####
# Cluster Means
apps_cluster4C_means <- as.data.table(aggregate(Score_Int ~  Predicted_Class_4C, apps, mean))
apps_cluster4C_means$Score_Int <- round(apps_cluster4C_means$Score_Int, 2)
apps_cluster4C_means[order(-Score_Int)]

ggplot(apps, aes(Predicted_Class_4C, Score_Int)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red")


################################################################ Application Scores by Cluster ###############################################################

# Overview
ggplot(apps, aes(Predicted_Class_4C, Score_Int)) + geom_boxplot() + 
  facet_grid(~ Applications) +
  stat_summary(fun=mean, geom="point", col="red")


##### A Tokenization of Assets ####
# v_265

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_265)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Asset Tokenization")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_265)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Asset Tokenization")

##### B Fractional ownership ####
# v_266

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_266)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Fractional ownership")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_266)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Fractional ownership")


##### C Self-Sovereign Identity ####
# v_267

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_267)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Self-Sovereign Identity")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_267)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Self-Sovereign Identity")


##### D Smart Contracts ####
# v_268

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_268)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Smart Contracts")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_268)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Smart Contracts")

##### E Micropayments ####
# v_269

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_269)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Micropayments")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_269)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Micropayments")

##### F Anonymous Transactions ####
# v_270

# 5 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_5C, v_270)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Anonymous Transactions")

# 4 Cluster Case
ggplot(quest_tri_extended, aes(x = Predicted_Class_4C, v_270)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Anonymous Transactions")





################################################################ Logisitic Regression ###############################################################


# Ordinal Logistic Regression: TBD
# Dependent variable (Y) = Likert Scale -> Ordinal
# Independent vairable (X) = Categorical 
quest_tri_extended$v_265 <- as.factor(quest_tri_extended$v_265)

# 5 Classes OLR
olr_5C <- polr(v_265 ~ Predicted_Class_5C, data = quest_tri_extended, Hess = T)
summary(olr_5C)
olr_5C$coefficients
coeffs_5C <- coef(summary(olr_5C))
p <- pnorm(abs(coeffs_5C[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs_5C, "p value" = round(p,3))

olr_4C <- polr(v_265 ~ Predicted_Class_4C, data = quest_tri_extended, Hess = T)
summary(olr_4C)
olr_4C$coefficients
coeffs_4C <- coef(summary(olr_4C))
p <- pnorm(abs(coeffs_4C[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs_4C, "p value" = round(p,3))




################################################################ Saving and Cleaning ###############################################################


# Clean Environment
rm(list = ls())



