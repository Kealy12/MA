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


############################################# Predictors ###############################################################


############################################# Moderators ###############################################################


############################################# CFA, C's alpha, AVE, Correlations ###############################################################


############################################# Regression results incl. VIF ###############################################################















