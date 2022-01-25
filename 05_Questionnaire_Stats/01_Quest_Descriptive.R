#######################################################################################################################################

# General Questionnaire Stats 

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
quest_raw <- fread("./../01_Input/raw_data_field_2021_10_21.csv")
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all_noNA_clusters.RData")
load("./../01_Input/01_RData/01_quest_cluster_extended.RData")
load("./../01_Input/01_RData/tri_all.RData")


# Loading Theme
source("./../04_Data_Prep/99_APA_Theme.R")

################################ Descriptive Blockchain Stats ######################

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

# % distribution
heard[, N := .N, by = c("variable", "value")]
heard[, N_total := .N, by = variable]
heard_summary <- unique(heard)
heard_summary[, dis := N / N_total, by =variable]
heard_summary

heard[value == "No", N := 0]

plot_heard <- ggplot(heard, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents knowing\nthe following terms") +
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(data = heard_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T)

plot_heard

####  Conditional: Sector you heard about blockchain technology ####

# Conditional Question
# v_31 - v_35, v_292, v_293
# 1 = quoted, 0 = not quoted
sector <- quest_clean[, .(v_31, v_32, v_33, v_34, v_35, v_292, v_293)]
colnames(sector) <- c("I have not heard of any Blockchain Technology applications", 
                       "Finance and banking", "Transport and logistics", "Energy and utilities", 
                      "Healthcare and pharmaceuticals", "Art and collectibles", "Other")
sector <- melt(sector)

# only look at distribution of selected answers
sector <- sector[value == 1]
sector <- sector[, N := sum(value), by = variable]
sector[, dis := N / nrow(sector) , by = variable]

sector_summary <- unique(sector)
sector_summary

ggplot(sector, aes(x= value, fill = reorder(variable, -N))) + geom_bar(position = "fill") +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_fill_brewer(palette = "Paired") +
  geom_text(data = sector_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman") +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.background = element_blank()) +
  labs( y = "%", title = "Sectors respondents heard about blockchain technology") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25))


####  Conditional: Knowingly used blockchain applications ####
# v_55
# 1 = Yes, 2 = No

# Out of all respondents: 3%
used <- quest_clean[, .(v_55)]
used <- melt(used)
used[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(used[, sum(value) / nrow(used)],2)

# Out of share who heard about blockchain technology: 7% 
used_share <- quest_clean[, .(v_55)]
used_share <- melt(used_share)
used_share[value == -77, value := NA]
used_share <- used_share[complete.cases(used_share)]
used_share[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(used_share[, sum(value) / nrow(used_share)],2)

####  Conditional: Use Crypto as means of payment ####

# v_56
# 1 = Yes, 2 = No

# Out of all respondents: 27%
use_crypto <- quest_clean[, .(v_56)]
use_crypto <- melt(use_crypto)
table(use_crypto)
use_crypto[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(use_crypto[, sum(value) / nrow(use_crypto)],2)

# Out of share who heard about Cryptocurrencyy: 28% 
use_crypto_share <- quest_clean[, .(v_56)]
use_crypto_share <- melt(use_crypto_share)
use_crypto_share[value == -77, value := NA]
use_crypto_share <- use_crypto_share[complete.cases(use_crypto_share)]
use_crypto_share[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(use_crypto_share[, sum(value) / nrow(use_crypto_share)],2)





####  Conditional: Why exclude possibility to use crypto  ####

# v_306, v_307, v_313
exclude <- quest_clean[, .(v_306, v_307, v_313)]
colnames(exclude) <- c("I find it difficult to find something\nwhere I can learn about cryptocurrencies", 
                             "I am not interested in cryptocurrencies", 
                             "Other reason")
exclude <- melt(exclude)

# only look at distribution of selected answers
exclude <- exclude[value == 1]
exclude <- exclude[, N := sum(value), by = variable]
exclude[, dis := N / nrow(exclude) , by = variable]

exclude_summary <- unique(exclude)
exclude_summary

plot_exclude <- ggplot(exclude, aes(x= value, fill = reorder(variable, -N))) + geom_bar(position = "fill") +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_fill_brewer(palette = "Paired") +
  geom_text(data = exclude_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman") +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.background = element_blank()) +
  labs( y = "%", title = "Share of respondents who would not use cryptocurrency \non on the reason why") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25))

plot_exclude


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

# % distribution
explain[, N := .N, by = c("variable", "value")]
explain[, N_total := .N, by = variable]
explain_summary <- unique(explain)
explain_summary[, dis := N / N_total, by =variable]
explain_summary

# Trick for ordering the axis
explain[, N := .N, by = c("variable", "value")]
explain[value == "No", N:= 0]
explain

plot_explain <- ggplot(explain, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents being able\nto explain these terms to a friend") +
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(data = explain_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T)


plot_heard
plot_explain



#### Age ####

# v_285 -> Need to recode data: +14 on score to show age (nobody < 15 and > 85)
quest_clean[, v_285 := v_285 + 14]
quest_clean[ v_285 == 79, .N]

# Mean Age of respondents 
mean(quest_clean$v_285)

# Age distribution 
quest_raw[, "v_285"]
ggplot(quest_raw[, "v_285"], aes(v_285)) + 
  geom_histogram() + theme_apa()


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
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(data = contact_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2, family = "Times New Roman",
            check_overlap = T)


# Higher Contact in Personal Life
contact[, mean(value), by = variable]



#### Knowledge of Blockchain technology ####

# Beginning of Questionnaire: v_286 (1-10 scale)
quest_clean[, .("Pre-Knowledge of Blockchain Technology (1-10)" = round(mean(v_286, na.rm = T),2))]

# End of Questionnaire: v_333
quest_clean[, .("Post-Knowledge of Blockchain Technology (1-10)" = round(mean(v_333, na.rm = T),2))]


#### Friend's knowledge of blockchain technology ####

# v_287
# Scale 1 (they never heard of it) - 10 (they are experts)
friends_know <- quest_clean[, .(v_287)]
friends_know[, .("Friend's knowledge of blockchain technology (1-10)" = round(mean(v_287, na.rm = T),2))]
# they have low knowledge about blockchain

#### Social influence on blockchain usage ####

# v_296
# Scale 1 (they never heard of it) - 10 (they are experts)
social <- quest_clean[, .(v_296)]
social[, .("Social influence on my blockchain usage (1-10)" = round(mean(v_296, na.rm = T),2))]
# They would rather discourage me



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
plot_usageIntention <- 
  ggplot(tech_usage_summary, aes(x = reorder(variable, N), y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
                position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Usage intention of technologies", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))

plot_usageIntention


#### Ability to explain the internet and blockchain ####

## Mean: Explain the Internet
# v_49 (1-10 scale)
internet <- quest_clean[, .("Ability to Explain the Internet (1-10)" = round(mean(v_49, na.rm = T),2))]
internet
## Mean: Explain Blockchain
# v_50 (1-10 scale)
blockchain <- quest_clean[, .("Ability to Explain Blockchain Technology (1-10)" = round(mean(v_50, na.rm = T),2))]
blockchain

## Likert visualization
ability <- quest_clean[, c("v_50", "v_49")]
colnames(ability) <- c("Blockchain technology", "The internet")
ability <- melt(ability)

# can be no 0s
ability[value == 0, value := NA]
table(ability)
ability <- ability[complete.cases(ability)]
table(ability$variable)

# N per variable after excluding NAs
ability[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
ability[value == 1, likert :="I don't know exactly how it works"]
ability[value == 2, likert :="I don't know how it works"]
ability[value == 3, likert :="I somewhat don't know how it works"]
ability[value == 4, likert :="Neutral"]
ability[value == 5, likert :="I somewhat can explain how it works"]
ability[value == 6, likert :="I can explain how it works"]
ability[value == 7, likert :="I can fully explain how it works"]

ability$likert <- factor(ability$likert  , levels = c("I don't know exactly how it works", "I don't know how it works",
                                                      "I somewhat don't know how it works", "Neutral", 
                                                      "I somewhat can explain how it works", "I can explain how it works",
                                                      "I can fully explain how it works"))

ability[value == 1, dis := .N / N, by = variable]
ability[value == 2, dis := .N / N, by = variable]
ability[value == 3, dis := .N / N, by = variable]
ability[value == 4, dis := .N / N, by = variable]
ability[value == 5, dis := .N / N, by = variable]
ability[value == 6, dis := .N / N, by = variable]
ability[value == 7, dis := .N / N, by = variable]

ability_summary <- unique(ability)

plot_abilityExplain <- ggplot(ability_summary, aes(x = variable, y = dis, 
                            fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Self-rated ability to explain the following technologies", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_abilityExplain


#### Possession of Crypto / NFT ####
# Crypto
# v_54 (1 = Yes, 2 = No)
crypto <- quest_clean[, "v_54"]
round(crypto[ v_54 == 1, .("Possession of Crypto (%)" = .N / nrow(crypto))],2) 

# Possession of a NFT #
# v_331 (1 = Yes, 2 = No, -77 = missing value (conditional question, if person heard of NFT))
nft <- quest_clean[, "v_331"]
round(nft[ v_331 == 1, .("Possession of Crypto (%)" = .N / nrow(crypto))],2)



####  Conditional: When first became crypto owner ####

# v_297
when <- quest_clean[, "v_297"]
when[v_297 == -77, v_297 := NA]

# Missing values (-77) to NAs
when <- when[complete.cases(when)]
table(when)

# scale it to years
when[, "Year" := v_297 + 2005]

when[, value := .N, by = v_297]
when <- when[, .(max(value)), by = Year][order(Year)]
when[, dis := V1 / when[, sum(V1)], by = Year]
when

ggplot(when, aes(Year, dis)) + 
  geom_line() + 
 # geom_text_repel(aes(label = scales::percent(dis,accuracy = 1, trim = F)), 
  #           size = 3, family = "Times New Roman", direction = "y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Point of time when share of respondents who possess/possessed \ncryptocurrency first became a cryptocurrency owner", y = "%", x = "Years") +
  theme_apa(remove.y.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))




####  Conditional: Difficulty buying crypto ####
# v_298 (Scale: 1 (Very easy) - 7 (Very hard))

diff <- quest_clean[, .(v_298)]
diff[v_298 == -77 | v_298 == 0, v_298 := NA]
diff[, mean(v_298, na.rm = T)] # relatively easy



####  Conditional: How manage crypto ####

manage_crypto <- quest_clean[, .(v_316, v_317, v_318, v_319)]
colnames(manage_crypto) <- c("On Coinbase, Binance or other exchange", 
                      "On MetaMask or other digital (browser) wallet", 
                      "On a piece of paper, USB-Storage or other offline wallet", 
                      "I do not know or do not want to tell")
manage_crypto <- melt(manage_crypto)

# only look at distribution of selected answers
manage_crypto <- manage_crypto[value == 1]
manage_crypto <- manage_crypto[, N := sum(value), by = variable]
manage_crypto[, dis := N / nrow(manage_crypto) , by = variable]

manage_crypto_summary <- unique(manage_crypto)
manage_crypto_summary

ggplot(manage_crypto, aes(x= value, fill = reorder(variable, -N))) + geom_bar(position = "fill") +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_fill_brewer(palette = "Paired") +
  geom_text(data = manage_crypto_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman") +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.background = element_blank()) +
  labs( y = "%", title = "Share of respondents who possess/possessed cryptocurrency \non how they manage their cryptocurrencies") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25))










#### 














################################## Trust, Personal Innovativeness & TRI   ###################################

#### Trust overall ####

# Disposition to trust other people
# v_314
trust <- quest_clean[, .(v_314)]
trust <- melt(trust)

# can be no 0s
trust[value == 0, value := NA]
trust <- trust[complete.cases(trust)]

# Preparing Plot
trust[value == 1, String := "Most people can be trusted"]
trust[value == 2, String := "You cannot be careful enough"]

trust <- trust[, N := .N, by = String]
trust[, dis := N / nrow(trust) , by = String]

trust_summary <- unique(trust)

# Score
trust_summary

# Likert
plot_trust <- ggplot(trust, aes(x= variable, fill = reorder(String, -N))) + geom_bar(position = "fill") +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_fill_brewer(palette = "Paired") +
  geom_text(data = trust_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman") +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.background = element_blank()) +
  labs( y = "%", title = "Disposition to trust other people") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25))


#### Privacy ####

# v_104, v_105, v_106 (reverse coded), v_107, v_205, v_206
privacy <- quest_clean[, .(v_106, v_107, v_104, v_105)]
colnames(privacy) <- c("Compared to others, I am less concerned about\npotential threats to my personal privacy",
                       "I am not bothered by data collection, because my\npersonal information is publicly available anyway", 
                       "Compared to others, I am more sensitive about\nthe way other people or organizations handle my\npersonal information",
                       "Compared to others, I see more importance in\nkeeping personal information private")

privacy <- melt(privacy)

# can be no 0s
privacy[value == 0, value := NA]
table(privacy)
privacy <- privacy[complete.cases(privacy)]
table(privacy$variable)

# N per variable after excluding NAs
privacy[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
privacy[value == 1, likert :="Strongly disagree"]
privacy[value == 2, likert :="Disagree"]
privacy[value == 3, likert :="Somewhat disagree"]
privacy[value == 4, likert :="Neither agree or disagree"]
privacy[value == 5, likert :="Somewhat agree"]
privacy[value == 6, likert :="Agree"]
privacy[value == 7, likert :="Strongly agree"]

privacy$likert <- factor(privacy$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                           "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
privacy[value == 1 , dis := .N / N, by = variable]
privacy[value == 2 , dis := .N / N, by = variable]
privacy[value == 3 , dis := .N / N, by = variable]
privacy[value == 4 , dis := .N / N, by = variable]
privacy[value == 5 , dis := .N / N, by = variable]
privacy[value == 6 , dis := .N / N, by = variable]
privacy[value == 7 , dis := .N / N, by = variable]

privacy_summary <- unique(privacy)

ggplot(privacy_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to privacy", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))


# Scores
# Average out of all statements -> Own index 
privacy_scores <- quest_clean[, .(v_104, v_105, v_106, v_107)]
privacy_scores[privacy_scores == 0] <- NA

# Reverse code v_106
privacy_scores[, Reverse_v_106 := 8 - v_106]
privacy_scores[, Privacy := round(rowMeans(privacy_scores[, .(v_104, v_105, v_107, Reverse_v_106)], na.rm = T), 2)]

# Score overall
privacy_scores[, .("Privacy score overall" = round(mean(Privacy, na.rm = T),2))]







#### Personal innovativeness ####
# cf. Agarwal & Prasad, 1998 
# v_205, v_206

## Likert
pers_inn <- quest_clean[, .(v_205, v_206)]
colnames(pers_inn) <- c("In general, I am hesitant to try out new\ninformation technologies",
                        "I like to experiment with new\ninformation technologies")
pers_inn <- melt(pers_inn)
pers_inn[pers_inn == 0] <- NA
pers_inn <- pers_inn[complete.cases(pers_inn)]

# N per variable after excluding NAs
pers_inn[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
pers_inn[value == 1, likert :="Strongly disagree"]
pers_inn[value == 2, likert :="Disagree"]
pers_inn[value == 3, likert :="Somewhat disagree"]
pers_inn[value == 4, likert :="Neither agree or disagree"]
pers_inn[value == 5, likert :="Somewhat agree"]
pers_inn[value == 6, likert :="Agree"]
pers_inn[value == 7, likert :="Strongly agree"]

pers_inn$likert <- factor(pers_inn$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                               "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
pers_inn[value == 1 , dis := .N / N, by = variable]
pers_inn[value == 2 , dis := .N / N, by = variable]
pers_inn[value == 3 , dis := .N / N, by = variable]
pers_inn[value == 4 , dis := .N / N, by = variable]
pers_inn[value == 5 , dis := .N / N, by = variable]
pers_inn[value == 6 , dis := .N / N, by = variable]
pers_inn[value == 7 , dis := .N / N, by = variable]

pers_inn_summary <- unique(pers_inn)

ggplot(pers_inn_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers on personal innovativeness", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))


## Score: Average out of statements
pers_inn_score <- quest_clean[, .(v_205, v_206)]
pers_inn_score[pers_inn_score == 0] <- NA

# Reverse code v_205
pers_inn_score[, Reverse_v_205 := 8 - v_205]
pers_inn_score[, PIIT := round(rowMeans(pers_inn_score[, .(v_206, Reverse_v_205)], na.rm = T), 2)]

# Score overall
pers_inn_score[, .("Personal innovativeness overall" = round(mean(PIIT, na.rm = T),2))]





#### TRI ####

tri <- quest_clean[, .(v_108, v_109, v_207, v_208, v_209, 
                       v_228, v_229, v_230, v_231, v_232)]
colnames(tri) <- c("Optimism:\nNew technology gives me more freedom of\nmobility",
                   "Optimism:\nNew technology makes me more productive",
                   "Innovativeness:\nOther people come to me for advice on new\ntechnologies",
                   "Innovativeness:\nIn general, I am among the first in my circle of\nfriends to acquire new technology when it\nappears",
                   "Innovativeness:\nI keep up with the latest technological\ndevelopments in my areas of interest",
                   "Discomfort:\nI can usually figure out new high-tech products\nand services without help from others",
                   "Discomfort:\nSometimes, I think that technology systems are\nnot designed for use by ordinary people",
                   "Insecurity:\nPeople are too dependent on technology to do\nthings for them",
                   "Insecurity:\nToo much technology distracts people to a point\nthat is harmful",
                   "Insecurity:\nI don’t feel comfortable doing business if the\nother party is only available online")

tri <- tri[, c(10,9,8,7,6,5,4,3,2,1)]

tri <- melt(tri)
tri[tri == 0] <- NA
tri <- tri[complete.cases(tri)]
table(tri)

# N per variable after excluding NAs
tri[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
tri[value == 1, likert :="Strongly disagree"]
tri[value == 2, likert :="Disagree"]
tri[value == 3, likert :="Somewhat disagree"]
tri[value == 4, likert :="Neither agree or disagree"]
tri[value == 5, likert :="Somewhat agree"]
tri[value == 6, likert :="Agree"]
tri[value == 7, likert :="Strongly agree"]

tri$likert <- factor(tri$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                       "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
tri[value == 1 , dis := .N / N, by = variable]
tri[value == 2 , dis := .N / N, by = variable]
tri[value == 3 , dis := .N / N, by = variable]
tri[value == 4 , dis := .N / N, by = variable]
tri[value == 5 , dis := .N / N, by = variable]
tri[value == 6 , dis := .N / N, by = variable]
tri[value == 7 , dis := .N / N, by = variable]

tri_summary <- unique(tri)

ggplot(tri_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers on TRI statements", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

tri_comp_all




















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

