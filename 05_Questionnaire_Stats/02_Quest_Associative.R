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

################################## Associations ####################################

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
  guides(fill = guide_legend(reverse=TRUE)) +
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







