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


################################ Descriptive Blockchain Stats ##########################

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
  labs(y = "%", x = "", title = "Distribution of respondents knowing the following terms") +
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
  theme_apa() +
  theme(text=element_text(family="Times New Roman", size=12))+
  scale_fill_brewer(palette = "Paired") +
  geom_text(data = sector_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman") +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.background = element_blank()) +
  labs(y = "%", title = "Sectors respondents heard about blockchain technology") +
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

# Who knows the difference? 
# Categorizing them by Cluster:
quest_tri_extended[, v_282 := as.numeric(ifelse(v_282 == "1", "1", "0"))]
diff_clus <- quest_tri_extended[, round(sum(v_282) / nrow(quest_tri_extended),2), by = Cluster]
diff_clus

# v_169 -> Male = 1, Female = 2
quest_tri_extended[, .("Know difference between Bitcoin and BT (%)" = round(sum(v_282) / nrow(quest_tri_extended),2)), by = v_169]


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
# they have low knowledge about blockchain, similar to mine

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
crypto[v_54 == 1, .N] # 101 possess crypto in sample
round(crypto[ v_54 == 1, .("Possession of Crypto (%)" = .N / nrow(crypto))],2) 

# Possession of a NFT #
# v_331 (1 = Yes, 2 = No, -77 = missing value (conditional question, if person heard of NFT))
nft <- quest_clean[, "v_331"]
round(nft[ v_331 == 1, .("Possession of NFT (%)" = .N / nrow(crypto))],2)



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

plot_trust
# Vertrauensgrundeinstellung eher negativ

#### Disposition to privacy ####

# v_104, v_105, v_106 (reverse coded)
privacy <- quest_clean[, .(v_106, v_104, v_105)]
colnames(privacy) <- c("Compared to others, I am less concerned about\npotential threats to my personal privacy\n(reverse coded)",
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

plot_privacy <- ggplot(privacy_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to disposition to privacy", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_privacy 
  
# Scores
# Average out of all statements -> Own index 
privacy_scores <- quest_clean[, .(v_104, v_105, v_106)]
privacy_scores[privacy_scores == 0] <- NA

# Reverse code v_106
privacy_scores[, Reverse_v_106 := 8 - v_106]
privacy_scores[, Privacy := round(rowMeans(privacy_scores[, .(v_104, v_105, Reverse_v_106)], na.rm = T), 2)]

# Score overall
privacy_scores[, .("Privacy score overall" = round(mean(Privacy, na.rm = T),2))]



#### Cynism ####
# v_107
# "I am not bothered by data collection, because my\npersonal information is publicly available anyway", 
cynism <- quest_clean[, .(v_107)]
colnames(cynism) <- c("I am not bothered by data collection, because my\npersonal information is publicly available anyway")

cynism <- melt(cynism)

# can be no 0s
cynism[value == 0, value := NA]
table(cynism)
cynism <- cynism[complete.cases(cynism)]

# N per variable after excluding NAs
cynism[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
cynism[value == 1, likert :="Strongly disagree"]
cynism[value == 2, likert :="Disagree"]
cynism[value == 3, likert :="Somewhat disagree"]
cynism[value == 4, likert :="Neither agree or disagree"]
cynism[value == 5, likert :="Somewhat agree"]
cynism[value == 6, likert :="Agree"]
cynism[value == 7, likert :="Strongly agree"]

cynism$likert <- factor(cynism$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                     "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
cynism[value == 1 , dis := .N / N, by = variable]
cynism[value == 2 , dis := .N / N, by = variable]
cynism[value == 3 , dis := .N / N, by = variable]
cynism[value == 4 , dis := .N / N, by = variable]
cynism[value == 5 , dis := .N / N, by = variable]
cynism[value == 6 , dis := .N / N, by = variable]
cynism[value == 7 , dis := .N / N, by = variable]

cynism_summary <- unique(cynism)

plot_cynism <- plot_privacy <- ggplot(cynism_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to cynism", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_cynism 


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























################################## BT Deep-Dive ###################################

#### Bank account statement on street ####
# v_126: (yes = 1, no = 2)

bank <- quest_clean[, .(v_126)]
bank <- melt(bank)
bank[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(bank[, sum(value) / nrow(bank)],2)
# Only 5% would put bank account statement on the street, without their name on 



#### BT to buy items ####
# v_85, v_86, v_87, v_88

# 1 = yes, 2 = no
buy <- quest_clean[, .(v_85, v_86, v_87, v_88)]
colnames(buy) <- c("A pizza", "A jacket", "A car", "A house")
buy <- melt(buy)
buy$value <- as.character(buy$value)
buy[value == "1", value := "Yes"]
buy[value == "2", value := "No"]
buy$value <- as.factor(buy$value)

# can be no 0s
buy[value == 0, value := NA]
table(buy)
buy <- buy[complete.cases(buy)]

# % distribution
buy[, N := .N, by = c("variable", "value")]
buy[, N_total := .N, by = variable]
buy_summary <- unique(buy)
buy_summary[, dis := N / N_total, by =variable]
buy_summary

buy[value == "No", N := 0]

plot_buy <- ggplot(buy, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents using blockchain technology\nto buy the following items") +
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(data = buy_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T)

plot_buy













#### Verified seller without name ####

# v_234: (yes = 1, no = 2)
seller <- quest_clean[, .(v_234)]
seller <- melt(seller)
seller[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(seller[, sum(value) / nrow(seller)],2)
# 38% would transfer money to a verified seller without name for buying a bluetooth speaker 

#### 



####  Conditional: Knowing that person's name 














####  Conditional: If you knew the real name ####
# v_233: (yes = 1, no = 2)
real_name <- quest_clean[, .(v_233)]
real_name <- melt(real_name)
real_name[value == -77, value := NA]
real_name <- real_name[complete.cases(real_name)]
real_name[, value := as.numeric(ifelse(value == "1", "1", 0))]
round(real_name[, sum(value) / nrow(real_name)],2)

# 52% of people who wouldn't send money to a seller without a name would change their opinion,
# if they knew the real name











#### Personal details - String of numbers & letters ####

# v_100
# Scale 1 (not comfortable at all) - 7 (very comfortable)
details <- quest_clean[, .(v_100)]
details[, .("Comfortability concering BT personal details as string of numbers and letters " = round(mean(v_100, na.rm = T),2))]
# Rather uncomfortable




#### Privacy concerns - BT Financial transactions ####

# v_99
# Scale 1 (fully disagree) - 7 (fully agree)
concerns <- quest_clean[, .(v_99)]
concerns[, .("Privacy concerns of BT for financial transactions" = round(mean(v_99, na.rm = T),2))]
# Rather concerned 



#### Losing Blockchain PIN ####

# v_101
# Scale 1 (not comfortable at all) - 7 (very comfortable)
losing.pin <- quest_clean[, .(v_101)]
losing.pin[, .("Comfortability regarding losing blockchain PIN" = round(mean(v_101, na.rm = T),2))]
# Not comfortable 






#### BT self-selected use cases ####

# v_120, v_121, v_122, v_284
# 1 = quoted, 0 = not quoted
statements <- quest_clean[, .(v_120, v_121, v_122, v_284)]
colnames(statements) <- c("I know use cases for Blockchain Technology\nOTHER THAN cryptocurrencies (Bitcoin is a\ncryptocurrency)", 
                      "I have installed an app related to Blockchain\nTechnology on my phone or desktop computer\n(e.g. Metamask)", 
                      "I advise people on how to use Blockchain\nTechnology applications or have coded some\nmyself (e.g. a real Smart Contract)",
                      "None of the statements apply to me")
statements <- melt(statements)
statements$value <- as.character(statements$value)
statements[value == "1", value := "Yes"]
statements[value == "0", value := "No"]
statements$value <- as.factor(statements$value)

# % distribution
statements[, N := .N, by = c("variable", "value")]
statements[, N_total := .N, by = variable]
statements_summary <- unique(statements)
statements_summary[, dis := N / N_total, by =variable]
statements_summary


plot_statements <- ggplot(statements, aes(reorder(variable, N), fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution of respondents on BT use cases") +
  guides(fill = guide_legend(reverse=TRUE)) +
  geom_text(data = statements_summary, aes(label = scales::percent(dis,accuracy = 1, trim = FALSE), y = dis), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T)

plot_statements





################################## BT Constructs ###################################

#### Usage intention ####
# v_132, v_133
usage.int <- quest_clean[, .(v_132, v_133)]
colnames(usage.int) <- c("Given the chance, I would use Blockchain\nTechnology applications",
                       "Given the chance, it is very likely that\nI would use Blockchain Technology")

usage.int <- melt(usage.int)

# can be no 0s
usage.int[value == 0, value := NA]
table(usage.int)
usage.int <- usage.int[complete.cases(usage.int)]
table(usage.int$variable)

# N per variable after excluding NAs
usage.int[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
usage.int[value == 1, likert :="Strongly disagree"]
usage.int[value == 2, likert :="Disagree"]
usage.int[value == 3, likert :="Somewhat disagree"]
usage.int[value == 4, likert :="Neither agree or disagree"]
usage.int[value == 5, likert :="Somewhat agree"]
usage.int[value == 6, likert :="Agree"]
usage.int[value == 7, likert :="Strongly agree"]

usage.int$likert <- factor(usage.int$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                     "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
usage.int[value == 1 , dis := .N / N, by = variable]
usage.int[value == 2 , dis := .N / N, by = variable]
usage.int[value == 3 , dis := .N / N, by = variable]
usage.int[value == 4 , dis := .N / N, by = variable]
usage.int[value == 5 , dis := .N / N, by = variable]
usage.int[value == 6 , dis := .N / N, by = variable]
usage.int[value == 7 , dis := .N / N, by = variable]

usage.int_summary <- unique(usage.int)

plot_usageint <- ggplot(usage.int_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to BT usage intention", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_usageint

# Scores
# Average out of all statements
usage.int.mean <- quest_clean[, .(v_132, v_133)]
usage.int.mean[usage.int.mean == 0] <- NA

usage.int.mean[, Usage_Intention := round(rowMeans(usage.int.mean[, .(v_132, v_133)], na.rm = T), 2)]

# Score overall
usage.int.mean[, .("Usage Intention score overall" = round(mean(Usage_Intention, na.rm = T),2))]
# 3.22: Rather not use BT applications 





#### Trust BT Users ####
# v_134, v_135, v_136
trust_users <- quest_clean[, .(v_134, v_135, v_136)]
colnames(trust_users) <- c("I would trust people, that use \nBlockchain Technology",
                       "I would trust organizations that use\nBlockchain Technology",
                       "I would trust machines that are connected to a\nBlockchain Technology")

trust_users <- melt(trust_users)

# can be no 0s
trust_users[value == 0, value := NA]
table(trust_users)
trust_users <- trust_users[complete.cases(trust_users)]
table(trust_users$variable)

# N per variable after excluding NAs
trust_users[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
trust_users[value == 1, likert :="Strongly disagree"]
trust_users[value == 2, likert :="Disagree"]
trust_users[value == 3, likert :="Somewhat disagree"]
trust_users[value == 4, likert :="Neither agree or disagree"]
trust_users[value == 5, likert :="Somewhat agree"]
trust_users[value == 6, likert :="Agree"]
trust_users[value == 7, likert :="Strongly agree"]

trust_users$likert <- factor(trust_users$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                     "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
trust_users[value == 1 , dis := .N / N, by = variable]
trust_users[value == 2 , dis := .N / N, by = variable]
trust_users[value == 3 , dis := .N / N, by = variable]
trust_users[value == 4 , dis := .N / N, by = variable]
trust_users[value == 5 , dis := .N / N, by = variable]
trust_users[value == 6 , dis := .N / N, by = variable]
trust_users[value == 7 , dis := .N / N, by = variable]

trust_users_summary <- unique(trust_users)

plot_trustUsers <- ggplot(trust_users_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to trust in\nblockchain users", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_trustUsers
# Low trust independent of machine or person 

# Scores
# Average out of all statements -> Own index 
trust_user_score <- quest_clean[, .(v_134, v_135, v_136)]
trust_user_score[trust_user_score == 0] <- NA

trust_user_score[, trust_user := round(rowMeans(trust_user_score[, .(v_134, v_135, v_136)], na.rm = T), 2)]

# Score overall
trust_user_score[, .("Trust in users score overall" = round(mean(trust_user, na.rm = T),2))]

# 3.61: rather low trust in other BT users


#### Trust BT (integrity, benevolence, ability) ####

# v_247 - v_252, v_144 - v_146
trust_iba <- quest_clean[, .(v_247, v_248, v_249, v_250, v_251, v_252,
                              v_144, v_145, v_146)]
colnames(trust_iba) <- c("Integrity: Blockchain Technology\nprovides reliable information",
                          "Integrity: Blockchain Technology\nis honest in dealing with my private data",
                          "Integrity: Blockchain Technology\nadheres to rules and principles", 
                          "Benevolence: Blockchain Technology\nacts in the interests of its users",
                          "Benevolence: In general Blockchain\nTechnology is not malicious",
                          "Benevolence: Blockchain Technology\nhas no bad intentions towards its users",
                          "Ability: Blockchain Technology\nserves its purpose", 
                          "Ability: Blockchain Technology\noperates flawlessly",
                          "Ability: Blockchain Technology\nis capable to offer me a good service")
trust_iba <- trust_iba[, c(9,8,7,6,5,4,3,2,1)]
trust_iba <- melt(trust_iba)

# can be no 0s
trust_iba[value == 0, value := NA]
table(trust_iba)
trust_iba <- trust_iba[complete.cases(trust_iba)]

# N per variable after excluding NAs
trust_iba[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
trust_iba[value == 1, likert :="Strongly disagree"]
trust_iba[value == 2, likert :="Disagree"]
trust_iba[value == 3, likert :="Somewhat disagree"]
trust_iba[value == 4, likert :="Neither agree or disagree"]
trust_iba[value == 5, likert :="Somewhat agree"]
trust_iba[value == 6, likert :="Agree"]
trust_iba[value == 7, likert :="Strongly agree"]

trust_iba$likert <- factor(trust_iba$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                             "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
trust_iba[value == 1 , dis := .N / N, by = variable]
trust_iba[value == 2 , dis := .N / N, by = variable]
trust_iba[value == 3 , dis := .N / N, by = variable]
trust_iba[value == 4 , dis := .N / N, by = variable]
trust_iba[value == 5 , dis := .N / N, by = variable]
trust_iba[value == 6 , dis := .N / N, by = variable]
trust_iba[value == 7 , dis := .N / N, by = variable]

trust_iba_summary <- unique(trust_iba)

# Plot
plot_trust_IBA <- 
  ggplot(trust_iba_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Trust in blockchain's integrity, benevolence and ability", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))

plot_trust_IBA

# Scores
# Average out of all statements 
trust_iba_score <- quest_clean[, .(v_247, v_248, v_249, v_250, v_251, v_252,
                                                v_144, v_145, v_146)]
trust_iba_score[trust_iba_score == 0] <- NA
trust_iba_score[, trust := round(rowMeans(trust_iba_score[, .(v_247, v_248, v_249, v_250, v_251, v_252,
                                                              v_144, v_145, v_146)], na.rm = T), 2)]
# Scores:
trust_iba_score[, "Integrity" := round(rowMeans(trust_iba_score[, c(1,2,3)], na.rm = T), 2)]
trust_iba_score[, "Benevolence" := round(rowMeans(trust_iba_score[, c(4,5,6)], na.rm = T), 2)]
trust_iba_score[, "Ability" := round(rowMeans(trust_iba_score[, c(7,8,9)], na.rm = T), 2)]

# Score overall
trust_iba_score[, .("Trust score overall" = round(mean(trust, na.rm = T),2))]

# Overall
trust_iba_score_table <- trust_iba_score[, .("Integrity" = mean(Integrity),
                               "Benevolence" = mean(Benevolence),
                               "Ability" = mean(Ability),
                               "Trust Overall" = mean(trust))]

trust_iba_score_table <- melt(trust_iba_score_table, variable.name = "Trust Components", value.name = "Mean")
trust_iba_score_table$Mean <- format(trust_iba_score_table$Mean, digits = 3)
trust_iba_score_table
# Overall = 4.29: Rather neutral to rather trustworthy score

#### Perceived benefit for society ####

# v_147, v_148
sbenefit <- quest_clean[, .(v_148, v_147)]
colnames(sbenefit) <- c("Using Blockchain Technology has\nmany disadvantages for society",
                           "Using Blockchain Technology has\nmany advantages for society")

sbenefit <- melt(sbenefit)

# can be no 0s
sbenefit[value == 0, value := NA]
table(sbenefit)
sbenefit <- sbenefit[complete.cases(sbenefit)]
table(sbenefit$variable)

# N per variable after excluding NAs
sbenefit[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
sbenefit[value == 1, likert :="Strongly disagree"]
sbenefit[value == 2, likert :="Disagree"]
sbenefit[value == 3, likert :="Somewhat disagree"]
sbenefit[value == 4, likert :="Neither agree or disagree"]
sbenefit[value == 5, likert :="Somewhat agree"]
sbenefit[value == 6, likert :="Agree"]
sbenefit[value == 7, likert :="Strongly agree"]

sbenefit$likert <- factor(sbenefit$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                             "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
sbenefit[value == 1 , dis := .N / N, by = variable]
sbenefit[value == 2 , dis := .N / N, by = variable]
sbenefit[value == 3 , dis := .N / N, by = variable]
sbenefit[value == 4 , dis := .N / N, by = variable]
sbenefit[value == 5 , dis := .N / N, by = variable]
sbenefit[value == 6 , dis := .N / N, by = variable]
sbenefit[value == 7 , dis := .N / N, by = variable]

sbenefit_summary <- unique(sbenefit)

plot_benefits_society <- ggplot(sbenefit_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to\nperceived benefits to society", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_benefits_society

# Scores
# Reverse code v_148
sbenefit_scores <- quest_clean[, .(v_148, v_147)]
sbenefit_scores[, Reverse_v_148 := 8 - v_148]

# Calculate Average 
sbenefit_scores[, Perc_Benefit := round(rowMeans(sbenefit_scores[, .(v_147, Reverse_v_148)], na.rm = T), 2)]

sbenefit_scores[, .("Perceived benefit to society" = round(mean(Perc_Benefit, na.rm = T),2))]
# not negative, rather neutral



#### Perceived Risk ####

# Low score = not risky, high score risky 
# v_149, v_150
risk <- quest_clean[, .(v_150, v_149)]
colnames(risk) <- c("I would feel unsafe using\nBlockchain Technology",
                    "In general, it seems risky to\nuse Blockchain Technology")

risk <- melt(risk)

# can be no 0s
risk[value == 0, value := NA]
table(risk)
risk <- risk[complete.cases(risk)]
table(risk$variable)

# N per variable after excluding NAs
risk[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
risk[value == 1, likert :="Strongly disagree"]
risk[value == 2, likert :="Disagree"]
risk[value == 3, likert :="Somewhat disagree"]
risk[value == 4, likert :="Neither agree or disagree"]
risk[value == 5, likert :="Somewhat agree"]
risk[value == 6, likert :="Agree"]
risk[value == 7, likert :="Strongly agree"]

risk$likert <- factor(risk$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                       "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
risk[value == 1 , dis := .N / N, by = variable]
risk[value == 2 , dis := .N / N, by = variable]
risk[value == 3 , dis := .N / N, by = variable]
risk[value == 4 , dis := .N / N, by = variable]
risk[value == 5 , dis := .N / N, by = variable]
risk[value == 6 , dis := .N / N, by = variable]
risk[value == 7 , dis := .N / N, by = variable]

risk_summary <- unique(risk)

plot_risk <- ggplot(risk_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.5, family = "Times New Roman",
            check_overlap = T) +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Distribution of answers related to\nperceived risk of BT", y = "%", x = "") +
  theme_apa(remove.x.gridlines = F) + 
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme(text=element_text(family="Times New Roman", size=12))

plot_risk

# Scores
risk_scores <- quest_clean[, .(v_149, v_150)]
risk_scores[risk_scores == 0] <- NA

# Calculate Average 
risk_scores[, Perc_Risk := round(rowMeans(risk_scores[, .(v_149, v_150)], na.rm = T), 2)]

risk_scores[, .("Perceived risk of BT" = round(mean(Perc_Risk, na.rm = T),2))]
# rather risky







#### Feeling of disruptive potential ####

# v_151, v_152, v_153, v_154
dis_pot <- quest_clean[, .(v_154, v_153, v_152, v_151)]
colnames(dis_pot) <- c("...has no disruptive potential at all",
                         "...to be as disruptive as the\nintroduction of the internet",
                         "...to disrupt everyday life", 
                         "...to disrupt the business world")

dis_pot <- melt(dis_pot)

# can be no 0s
dis_pot[value == 0, value := NA]
table(dis_pot)
dis_pot <- dis_pot[complete.cases(dis_pot)]

# N per variable after excluding NAs
dis_pot[, N:= .N, by = variable]

# 7-point Likert Scale: Adding Scores as Strings
dis_pot[value == 1, likert :="Strongly disagree"]
dis_pot[value == 2, likert :="Disagree"]
dis_pot[value == 3, likert :="Somewhat disagree"]
dis_pot[value == 4, likert :="Neither agree or disagree"]
dis_pot[value == 5, likert :="Somewhat agree"]
dis_pot[value == 6, likert :="Agree"]
dis_pot[value == 7, likert :="Strongly agree"]

dis_pot$likert <- factor(dis_pot$likert , levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree or disagree", 
                                                         "Somewhat agree", "Agree", "Strongly agree"))

# % distribution of answers
dis_pot[value == 1 , dis := .N / N, by = variable]
dis_pot[value == 2 , dis := .N / N, by = variable]
dis_pot[value == 3 , dis := .N / N, by = variable]
dis_pot[value == 4 , dis := .N / N, by = variable]
dis_pot[value == 5 , dis := .N / N, by = variable]
dis_pot[value == 6 , dis := .N / N, by = variable]
dis_pot[value == 7 , dis := .N / N, by = variable]

dis_pot_summary <- unique(dis_pot)

# Plot
plot_disPotential <- 
  ggplot(dis_pot_summary, aes(x = variable, y = dis, fill = likert)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Feeling of disruptive potential of BT", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))

plot_disPotential
# Contact in professional life lower than in personal life -> However, people
# see more disruptive potential in the business world (in the future)

# Scores
# Average out of all statements 
dis_pot_score <- quest_clean[, .(v_154, v_153, v_152, v_151)]
dis_pot_score[dis_pot_score == 0] <- NA

# Reverse code v_154
dis_pot_score[, Reverse_v_154 := 8 - v_154]

dis_pot_score[, Pot_Dis := round(rowMeans(dis_pot_score[, .(v_151, v_152, v_153, Reverse_v_154)], na.rm = T), 2)]

# Score overall
dis_pot_score[, .("Potential of disruption score overall" = round(mean(Pot_Dis, na.rm = T),2))]



################################## Application scores in 03_Applications_Descriptive ###################################

################################## Post-Survey BT questions ###################################

#### Post-survey knowledge of BT ####
#  v_333
quest_clean[, .("Post-Knowledge of Blockchain Technology (1-10)" = round(mean(v_333, na.rm = T),2))]

# Beginning of Questionnaire: v_286 (1-10 scale)
quest_clean[, .("Pre-Knowledge of Blockchain Technology (1-10)" = round(mean(v_286, na.rm = T),2))]


#### More opportunities or risks from BT ####

#  v_288
# scale 1 (=more risks) - 10 (more opportunities)
quest_clean[, .("More risks (1) or more opportunities (10) from BT" = round(mean(v_288, na.rm = T),2))]
# more opportunities











#### German federal ministry BT campaign####

# v_196 (1 = Yes, 2 = No)
camp <- quest_clean[, .(v_196)]
camp[v_196 == 1, .N] # 40 know about it
round(camp[ v_196 == 1, .("Knowledge about German ministry BT campaign" = .N / nrow(camp))],2) 
# 5% know about it









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


#### Clean Environment ####
rm(list = ls())

