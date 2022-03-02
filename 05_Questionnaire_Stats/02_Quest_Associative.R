#######################################################################################################################################

# Associative Plots between questions / variables related to blockchain

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

################################## Associations ############################################

#### Gender vs. Possession of Crypto / NFT ####

# v_169 -> Male = 1, Female = 2
quest_clean$v_169 <- as.character(quest_clean$v_169)
quest_clean[v_169 == 1, v_169 := "Male"]
quest_clean[v_169 == 2, v_169 := "Female"]
quest_clean$v_169 <- as.factor(quest_clean$v_169)

# Gender - Possession of Crypto
# v_54 (1 = Yes, 2 = No)
ggplot(quest_clean, aes(v_169, fill = v_54 == 1 )) + 
  geom_bar(position = "fill") + scale_fill_brewer( palette = "Paired") + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Possession of Crypto") +
  guides(fill = guide_legend(reverse=TRUE))

  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T)

# Gender - Possession of NFT
# v_331 (1 = Yes, 2 = No, -77 = missing value (conditional question, if person heard of NFT))
ggplot(quest_clean, aes(v_169, fill = v_331 == 1 )) + 
  geom_bar(position = "fill")+ scale_fill_brewer( palette = "Paired") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Possession of NFT") +
  guides(fill = guide_legend(reverse=TRUE))


#### Have you heard of Blockchain vs. Application Scores #####

# Blockchain: v_321
# 1 = yes, 2 = no
heard_app <- quest_clean[, .(v_321, v_265, v_266, v_267, v_268, v_269, v_270)]
colnames(heard_app) <- c("Blockchain_Technology", 
                         "Tokenization of Assets",
                         "Fractional ownership",
                         "Self-Sovereign Identity",
                         "Smart Contracts",
                         "Micropayments",
                         "Anonymous Transactions")

heard_app <- melt(heard_app, id.vars = c("Blockchain_Technology"), 
             variable.name = "Applications", value.name = "Score_Factor")

# Need Score as Factor (String) and as Integer for Calculations
heard_app$Score_Factor <- as.factor(heard_app$Score_Factor)
heard_app[, Score_Int := as.numeric(Score_Factor)]

# Scores on applications by whether people heard of blockchain or not
heard_app_means <- as.data.table(aggregate(Score_Int ~  Blockchain_Technology, heard_app, mean))
heard_app_means[Blockchain_Technology == 1, heard_it := "Yes"]
heard_app_means[Blockchain_Technology == 2, heard_it := "No"]
heard_app_means$Score_Int <- round(heard_app_means$Score_Int, 2)
heard_app_means[order(-Score_Int)]




#### Blockchain usage intention pre-& post-survey ####

# Visualization:
# 1 = Yes, 2 = No, 3 = Don't know enough 
block_usage <- quest_clean[,c("v_28", "v_334")]
colnames(block_usage) <- c("Pre-survey BT usage intention", "Post-survey BT usage intention")

block_usage <- melt(block_usage)

# can be no 0s
block_usage[value == 0, value := NA]
table(block_usage)
block_usage <- block_usage[complete.cases(block_usage)]

# Calculating distribution of answers
block_usage[value == "1", likert :="Yes"]
block_usage[value == "2", likert :="No"]
block_usage[value == "3", likert :="I don't know enough about it"]
block_usage$likert <- factor(block_usage$likert, levels = c("No","I don't know enough about it", "Yes"))

# N per variable after excluding NAs
block_usage[, N:= .N, by = variable]

block_usage[value == 1, dis := .N / N, by = variable]
block_usage[value == 2, dis := .N / N, by = variable]
block_usage[value == 3, dis := .N / N, by = variable]

block_usage_summary <- unique(block_usage)
block_usage_summary

# Plot
plot_prePostSurvey_blockUsage <- 
  ggplot(block_usage_summary, aes(x = reorder(variable, -N), y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Pre- and post-survey usage intention\nof blockchain technology", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))

plot_prePostSurvey_blockUsage

# Fisher Test
# H0: Pre-survey BT usage intention is independent from post-survey BT usage intention

# Don't knows (3) are treated as Nos: 1 = Yes, 0 = No
fisher_blockusage <- copy(block_usage)
fisher_blockusage[value == 3, fisher_score := 0]
fisher_blockusage[value == 2, fisher_score := 0]
fisher_blockusage[value == 1, fisher_score := 1]

table(fisher_blockusage$variable)
table(fisher_blockusage$fisher_score)

f_table <- table(fisher_blockusage$variable, fisher_blockusage$fisher_score)

f_test <- fisher.test(f_table, alternative = "greater")
f_test$p.value
# p-value = 2.018713e-16 -> Reject H0 at alpha = 1% level
# Usage intention pre-survey not independent from usage intention post-survey
# --> the difference in ratios did not arrive by pure chance





























#### Knowledge vs. Gender ####

# Beginning of Questionnaire: v_286 (1-10 scale)
know <- quest_clean[, .("Pre-Knowledge of Blockchain Technology (1-10)" = v_286)]
gender <- quest_clean[, .("Gender" = v_169)]
know_gender <- cbind(know, gender)
know_gender

mean_know_gender <- know_gender[, .("Mean knowledge" = round(mean(`Pre-Knowledge of Blockchain Technology (1-10)`, na.rm = T),2)), by = Gender]
mean_know_gender

#### Mediation: Knowledge -> Perceived risk -> Usage intention ####
# Experience
contact<- quest_clean[, c("v_10", "v_11")]
colnames(contact) <- c("Contact_in_professional_life", "Contact_in_personal_life")

# Knowledge 
# (1-10 scale): v_286
knowledge <- quest_clean[, .(v_286)]
colnames(knowledge) <- c("Knowledge_of_BT")

# Binding together and creating mean
experience <- cbind(contact, knowledge)
experience[, Experience := round(rowMeans(experience[, .(Contact_in_professional_life, Contact_in_personal_life, Knowledge_of_BT)], na.rm = T), 2)]

# perceived risk
# v_149, v_150: Likert (1-7)
# can be no 0s
quest_clean[v_149 == 0, v_149 := NA]
quest_clean[v_150 == 0, v_150 := NA]

# Calculate Average 
quest_clean[, Perceived_risk := round(rowMeans(quest_clean[, .(v_149, v_150)], na.rm = T), 2)]
perceived_risk <- quest_clean[, .(Perceived_risk)]

# Usage intention
# v_132, v_133: Likert (1-7)
# can be no 0s
quest_clean[v_132 == 0, v_132 := NA]
quest_clean[v_133 == 0, v_133 := NA]

# Calculate Average 
quest_clean[, Usage_Intention := round(rowMeans(quest_clean[, .(v_132, v_133)], na.rm = T), 2)]
usage_int <- quest_clean[, .(Usage_Intention)]

# MEDIATION: X → Y, X → M, and X + M → Y
mediation <- cbind(experience,perceived_risk,usage_int)
# X (Experience) → Y (Usage Intention)
summary(lm(Usage_Intention ~ Experience, data = mediation)) # significant: b(experience)= 0.67577
# X (Experience) → M (Perceived_risk)
summary(lm(Perceived_risk ~ Experience, data = mediation)) # significant
# X + M → Y
summary(lm(Usage_Intention ~ Experience + Perceived_risk, data = mediation)) # significant: b(experience) = 0.4211
# Mediation successfully supported, as absolute b-coef is reduced 



################################## Save & Clean ############################################
#### Clean Environment ####
rm(list = ls())











