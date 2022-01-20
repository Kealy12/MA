#######################################################################################################################################

# General Questionnaire Stats 

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

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/01_quest_cluster_extended.RData")

# Loading Theme
source("./../04_Data_Prep/99_APA_Theme.R")

################################################################ General Stats of questionnaire ###############################################################

#### N ####
quest_clean[, .N]


#### Have you heard of the following terms ####
# v_321 - v_325
# 1 = yes, 2 = no
heard <- quest_clean[, .(v_321, v_322, v_323, v_324, v_325)]
colnames(heard) <- c("Blockchain Technology", "Bitcoin", "NFT", "Cryptocurrency", "Ethereum")
heard <- melt(heard)
heard$value <- as.character(heard$value)
heard[value == "1", value := "Yes"]
heard[value == "2", value := "No"]
heard$value <- as.factor(heard$value)

# Trick for ordering the axis
heard[, N := .N, by = c("variable", "value")]
heard[value == "No", N:= 0]
heard

plot_heard <- ggplot(heard, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents knowing the following terms") +
  guides(fill = guide_legend(reverse=TRUE))


#### Can you explain these to a friend ####
# v_326 - v_330
# 1 = yes, 2 = no
explain <- quest_clean[, .(v_326, v_327, v_328, v_329, v_330)]
colnames(explain) <- c("Blockchain Technology", "Bitcoin", "NFT", "Cryptocurrency", "Ethereum")
explain <- melt(explain)
explain$value <- as.character(explain$value)
explain[value == "1", value := "Yes"]
explain[value == "2", value := "No"]
explain$value <- as.factor(explain$value)

# Trick for ordering the axis
explain[, N := .N, by = c("variable", "value")]
explain[value == "No", N:= 0]
explain

plot_explain <- ggplot(explain, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents being able to explain these terms to a friend") +
  guides(fill = guide_legend(reverse=TRUE))

plot_heard
plot_explain



#### Gender - Possession of Crypto / NFT ####

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

# Age distribution



# Have you heard of following terms?
# v_321





# Gender distribution 
quest_clean[, "v_169"]
ggplot(quest_clean[, "v_169"], aes(v_169)) + 
  geom_bar()

# Age distribution 
quest_raw[, "v_285"]
ggplot(quest_raw[, "v_285"], aes(v_285)) + 
  geom_histogram() + theme_apa()

# Difference beteen Bitcoin and Blockchain? 
difference <- quest_raw[, "v_282"]
difference <- difference[, v_282 := gsub(1, "yes", v_282)]
difference <- difference[, v_282 := gsub(2, "no", v_282)]

ggplot(difference, aes(v_282)) + 
  geom_bar()

quest_raw[v_282 == 2, "v_282"][, .N]



# Overall Blockchain knowledge (before and after the survey)






# Clean Environment
rm(list = ls())



#### Age ####

# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
quest_clean[, v_285 := v_285 + 14]
quest_clean[ v_285 == 79, .N]

# Mean Age of respondents 
mean(quest_clean$v_285)

# Age - Heard of Blockchain Technology
# x = Age: Continuous, Y = Heard of Blockchain Tech: Categorical
# Logistic Model
# age_HeardOfBC <- quest_clean[, .(v_321, v_285)]
# age_HeardOfBC[, v_321 := as.numeric(ifelse(v_321 == "1", "1", "0"))]
# glm <- glm(age_HeardOfBC$v_321 ~ age_HeardOfBC$v_285, age_HeardOfBC, family = "binomial")
# 
# coef(gl)


#### Difference between Bitcoin and Blockchain #### 

# v_282
# 1 = Yes, 2 = No
diff <- quest_clean[, "v_282"]
diff <- melt(diff)
diff[, value := as.numeric(ifelse(value == "1", "1", "0"))]
round(diff[, sum(value) / nrow(diff)],2)
# only 25 % know the difference


#### Contact with Blockchain technology ####

# v_10: in professional life
# v_11: in personal life
contact<- quest_clean[, c("v_10", "v_11")]
colnames(contact) <- c("Contact in professional life", "Contact in personal life")
contact <- melt(contact)
# can be no 0s
contact[value == 0, value := NA]
table(contact)
contact <- contact[complete.cases(contact)]


# 7-point Likert Scale: Adding Scores as Strings
contact[value == 1, likert :="Very low"]
contact[value == 2, likert :="Low"]
contact[value == 3, likert :="Below average"]
contact[value == 4, likert :="Average"]
contact[value == 5, likert :="Above average"]
contact[value == 6, likert :="High"]
contact[value == 7, likert :="Very high"]

contact$likert <- factor(contact$likert  , levels = c("Very low", "Low", "Below average", "Average", 
                                                           "Above average", "High", "Very high"))

contact[value == 1, dis := .N / nrow(quest_clean), by = variable]
contact[value == 2, dis := .N / nrow(quest_clean), by = variable]
contact[value == 3, dis := .N / nrow(quest_clean), by = variable]
contact[value == 4, dis := .N / nrow(quest_clean), by = variable]
contact[value == 5, dis := .N / nrow(quest_clean), by = variable]
contact[value == 6, dis := .N / nrow(quest_clean), by = variable]
contact[value == 7, dis := .N / nrow(quest_clean), by = variable]

contact_summary <- unique(contact)

ggplot(contact_summary, aes(x = variable, y = dis, 
                         fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Current contact with Blockchain Technology", y = "%", x = "") +
  theme_apa() + scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  guides(fill = guide_legend(reverse=TRUE))


# Higher Contact in Personal Life
contact[, mean(value), by = variable]



#### Knowledge of Blockchain technology ####

# Beginning of Questionnaire: v_286 (1-10 scale)
# End of Questionnaire: v_333
# Mean
quest_clean[, .("Pre-Knowledge of Blockchain Technology (1-10)" = round(mean(v_286, na.rm = T),2))]
quest_clean[, .("Post-Knowledge of Blockchain Technology (1-10)" = round(mean(v_333, na.rm = T),2))]

#### Tech Usage ####

# v_19 - v_30
# 1 = Yes, 2 = No, 3 = Don't know enough 
tech_usage <- quest_clean[, c("v_19", "v_20", "v_21", "v_22", "v_23", "v_24", 
                              "v_25", "v_26", "v_27", "v_28", "v_29", "v_30")]
colnames(tech_usage) <- c("Cloud computing", "Big data", "Internet of things", "Smart Home products",
                          "3D printing", "Artificial intelligence", "Machine learning", "Neural networks",
                          "Deepfake technology", "Blockchain technology", "5G", "Contactless payments")

tech_usage <- melt(tech_usage)

# can be no 0s
tech_usage[value == 0, value := NA]
table(tech_usage)
tech_usage <- tech_usage[complete.cases(tech_usage)]

# Calculating distribution of answers
tech_usage[value == "1", likert :="Yes"]
tech_usage[value == "2", likert :="No"]
tech_usage[value == "3", likert :="Don't know enough about it"]
tech_usage$likert <- factor(tech_usage$likert, levels = c("No","Don't know enough about it", "Yes"))

# N per variable after excluding NAs
tech_usage[, N:= .N, by = variable]

tech_usage[value == 1, dis := .N / N, by = variable]
tech_usage[value == 2, dis := .N / N, by = variable]
tech_usage[value == 3, dis := .N / N, by = variable]

# Trick for ordering the axis
tech_usage[, N:= .N, by = c("variable", "value")]
tech_usage[value == "2", N:= 0]
tech_usage[value == "3", N:= 0]
tech_usage

tech_usage_summary <- unique(tech_usage)
tech_usage_summary

# Plot
plot_usageIntention <- ggplot(tech_usage_summary, aes(x = reorder(variable, N), y = dis, 
                            fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
                position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Tech Usage Intention", y = "%", x = "") +
  theme_apa() + scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  guides(fill = guide_legend(reverse=TRUE)) 

plot_usageIntention






#### GGplot ####
ggplot(dt, aes(x, y, fill = ..., color = ...)) +
  # USE HISTOGRAMS TO LOOK AT DIFFERENT DISTRIBUTIONS!
  geom_histogram(position = "dodge") + geom_bar() +
  geom_jitter() + geom_point() + geom_smooth()
geom_boxplot(width = ..., factor(x)) + geom_violin() + geom_dotplot(binaxis="y", stackdir="center", dotsize=0.3) +
  geom_line( "x has to be numeric") + geom_text_repel(aes(label = ...))
scale_x_log10() + scale_y_log10() +
  facet_grid(~..., scales = "free") + facet_wrap() +
  stat_summary(fun.y=mean, geom="point", col="darkred") +
  labs(x = "...", y = "...", title = "...") +
  guides(fill/color = guide_legend(title = "..."))

# Clean Environment
rm(list = ls())

