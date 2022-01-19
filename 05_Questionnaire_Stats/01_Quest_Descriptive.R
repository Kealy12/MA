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


ggplot(heard, aes(variable, fill = factor(value))) + geom_bar(position = "fill") +
  coord_flip() + scale_fill_brewer( palette = "Paired") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12)) + 
  labs(y = "%", x = "", title = "Distribution on respondents knowing the following terms") +
  guides(fill = guide_legend(reverse=TRUE))

table(heard)

##### Gender ####
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
  geom_bar(position = "fill")+ scale_fill_brewer( palette = "Paired")

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


