#######################################################################################################################################

# Latent Class Analysis based on TRI Scale
# Cf. Parasuraman and Colby (2015)

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

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
quest_raw_UK <- fread("./01_Input/raw_data_field_UK_2022_02_24.csv")
load("./01_Input/00_clean_data_field_UK.RData")
load("./01_Input/tri_all.RData")

# Loading LCAs: 4 Cluster chosen
load("./01_Input/lca_4class_analysis.RData")
load("./01_Input/lca_6class_analysis.RData")
load("./01_Input/lca_5class_analysis.RData")
load("./01_Input/lca_3class_analysis.RData")

# Result of this Segmentation
load("./01_Input/tri_all_noNA_clusters_UK.RData")


########################################## LCA Predicted Classes on scores of 10 TRI items ##########################################################
f1 <- as.formula(cbind(OPT2, OPT4, INN1, INN2, INN4, DIS2, DIS3, INS1, INS2, INS4)~1)

# LCA can only be computed on rows with no NA, so these rows need to be filtered out
tri_comp_all_noNA_UK<- tri_comp_all_UK[complete.cases(tri_comp_all_UK)]

# N of LCA = 830 due to invalid answers
# (Parasuraman and Colby (2015): N of survey to LCA also reduced from 878 to 782)
nrow(tri_comp_all_noNA_UK)

# 5 classes, Cf. Parasuraman and Colby (2015)
# nrep = 15 to ensure to ensure that the algorithm finds a global rather than local maximum of the log-likelihood function (greatest value)
lca_5class_UK <- poLCA(f1, data = tri_comp_all_noNA_UK, nclass = 5, na.rm = T, nrep = 15)

# 4 classes: hard to distinguish Pioneers and Skeptics 
# 4 class model has lowest BIC score = best model fit
lca_4class_UK <- poLCA(f1, data = tri_comp_all_noNA_UK, nclass = 4, na.rm = T, nrep = 15)

# For Comparison Purpose: 6 & 3 class model
lca_6class_UK <- poLCA(f1, data = tri_comp_all_noNA_UK, nclass = 6, na.rm = T, nrep = 15)
lca_3class_UK <- poLCA(f1, data = tri_comp_all_noNA_UK, nclass = 3, na.rm = T, nrep = 15)

# Predicted Classes -> Do for each analysis
pred_class <- as.data.table(lca_3class_UK$predclass)
colnames(pred_class) <- c("Predicted Class (3Cs)")
unique(pred_class)

# Binding Results together
tri_comp_all_noNA_UK <- cbind(tri_comp_all_noNA_UK, pred_class)
tri_comp_all_noNA_UK

# Saving LCA Analysis
save(lca_6class_UK, file = "./01_Input/lca_6class_analysis.RData")
save(lca_5class_UK, file = "./01_Input/lca_5class_analysis.RData")
save(lca_4class_UK, file = "./01_Input/lca_4class_analysis.RData")
save(lca_3class_UK, file = "./01_Input/lca_3class_analysis.RData")


########################################## Model Summaries ##########################################################
tri_segments_UK <- tri_comp_all_noNA_UK[, c(11:14,17,18,19,20,21)]

tri_segments_4C_summary_UK <- tri_segments_UK[, .("N" = .N,
                                            "%" = .N / nrow(tri_segments_UK),
                                            "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                            "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                            "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                            "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                            "Overall TRI" = mean(`Overall TRI`)), 
                                        by = "Predicted Class (4Cs)"]

tri_segments_4C_summary_UK<- round(tri_segments_4C_summary_UK[order(-`Overall TRI`)],2)

#### Other Models for Comparison ####
tri_segments_6C_summary <- tri_segments_UK[, .("N" = .N,
                                            "%" = .N / nrow(tri_segments_UK),
                                            "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                            "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                            "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                            "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                            "Overall TRI" = mean(`Overall TRI`)), 
                                        by = "Predicted Class (6Cs)"]

tri_segments_5C_summary <- tri_segments_UK[, .("N" = .N,
                                            "%" = .N / nrow(tri_segments_UK),
                                            "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                            "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                            "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                            "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                            "Overall TRI" = mean(`Overall TRI`)), 
                                        by = "Predicted Class (5Cs)"]



tri_segments_3C_summary <- tri_segments_UK[, .("N" = .N,
                                            "%" = .N / nrow(tri_segments_UK),
                                            "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                            "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                            "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                            "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                            "Overall TRI" = mean(`Overall TRI`)), 
                                        by = "Predicted Class (3Cs)"]

#### Summaries of other models ####

round(tri_segments_6C_summary[order(-`Overall TRI`)],2)
round(tri_segments_5C_summary[order(-`Overall TRI`)],2)
round(tri_segments_3C_summary[order(-`Overall TRI`)],2)

# Show key information of lca models in table for paper

########################################## Statistical Comparison of Latent Class Analysis Models ##########################################################

model_3 <- data.table("Number of Clusters" = "3", "Log-Likelihood" = lca_3class_UK$llik, "BIC" = lca_3class_UK$bic, "AIC" = lca_3class_UK$aic)
model_4 <- data.table("Number of Clusters" = "4", "Log-Likelihood" = lca_4class_UK$llik, "BIC" = lca_4class_UK$bic, "AIC" = lca_4class_UK$aic)
model_5 <- data.table("Number of Clusters" = "5", "Log-Likelihood" = lca_5class_UK$llik, "BIC" = lca_5class_UK$bic, "AIC" = lca_5class_UK$aic)
model_6 <- data.table("Number of Clusters" = "6", "Log-Likelihood" = lca_6class_UK$llik, "BIC" = lca_6class_UK$bic, "AIC" = lca_6class_UK$aic)

model_summary <- rbind(model_3, model_4, model_5, model_6)
model_summary

# Following Parasuraman and Colby (2015): We concluded 4 Clusters, due to:
# Best model fit (lowest BIC) statistically and more clearly distinguishable cluster TRI characteristics for our purposes

#### Writing to Excel and Printing Table ####
print(xtable(model_summary, type = "latex"), file = "./02_Output/lca_model_summary.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(model_summary)))
sheets <- list("model_summary" = model_summary)
write_xlsx(sheets, "./02_Output/lca_model_summary.xlsx")

########################################## Going Forward: 4 Cluster Segmentation ##########################################################

# Preparing Table
colnames(tri_segments_4C_summary_UK)[1] <- "Cluster"
tri_segments_4C_summary_UK$Cluster <- as.character(tri_segments_4C_summary_UK$Cluster)
tri_segments_4C_summary_UK[ Cluster == "3", Cluster := "Explorers"]
tri_segments_4C_summary_UK[ Cluster == "4", Cluster := "Hesitators"]
tri_segments_4C_summary_UK[ Cluster == "2", Cluster := "Avoiders"]
tri_segments_4C_summary_UK[ Cluster == "1", Cluster := "Pioneers"]

tri_segments_4C_summary_UK

# Writing to Excel and Printing Table
print(xtable(model_summary, type = "latex"), file = "./02_Output/tri_segments_4C_summary_UK.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(tri_segments_4C_summary_UK)))
sheets <- list("tri_segments_4C_summary_UK" = tri_segments_4C_summary_UK)
write_xlsx(sheets, "./02_Output/tri_segments_4C_summary_UK.xlsx")

# Saving
save(tri_segments_4C_summary_UK, file = "./01_Input/tri_segments_4C_summary_UK.RData")

########################################## Interpretation ##########################################################


# 5 CLASSES -> Problem: skeptics and pioneers not clearly distinguishable 

# class 5: EXPLORER 
# highest TRI, highest OPT, highest INN, lowest discomfort/inhibition 

# class 2: SKEPTICS
# less positive and negative views 

# class 1: PIONEERS
# 

# class 3: HESITATORS -> PASST
# 

# class 4: AVOIDERS -> PASST
# lowest in motivation, highest in resistance 

####### 4 CLASSES ########

# class 3: EXPLORER

# class 2: AVOIDERS

# class 4: HESITATORS

# class 1: PIONEERS


########################################## Saving and Cleaning ##########################################################

# Saving TRI Components, Overall TRI and Class Predictions on non-NA data
save(tri_comp_all_noNA_UK, file = "./01_Input/tri_all_noNA_clusters_UK.RData")

# Clean Environment
rm(list = ls())
