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
load("./../01_Input/01_RData/00_clean_data_field.RData")
load("./../01_Input/01_RData/tri_all.RData")

# Loading LCAs
load("./../01_Input/01_RData/lca_5class_analysis.RData")
load("./../01_Input/01_RData/lca_4class_analysis.RData")


########################################## LCA Predicted Classes on scores of 10 TRI items ##########################################################
f1 <- as.formula(cbind(OPT2, OPT4, INN1, INN2, INN4, DIS2, DIS3, INS1, INS2, INS4)~1)

# LCA can only be computed on rows with no NA, so these rows need to be filtered out
tri_comp_all_noNA<- tri_comp_all[complete.cases(tri_comp_all)]

# N of LCA = 822 due to invalid answers
# (Parasuraman and Colby (2015): N of survey to LCA also reduced from 878 to 782)
nrow(tri_comp_all_noNA)

# 5 classes, Cf. Parasuraman and Colby (2015)
# nrep = 15 to ensure to ensure that the algorithm finds a global rather than local maximum of the log-likelihood function (greatest value)
lca_5class <- poLCA(f1, data = tri_comp_all_noNA, nclass = 5, na.rm = T, nrep = 15)

# 4 classes: hard to distinguish Pioneers and Skeptics 
# 4 class model has lowest BIC score = best model fit
lca_3class <- poLCA(f1, data = tri_comp_all_noNA, nclass = 3, na.rm = T, nrep = 15)

# Predicted Classes
pred_class <- as.data.table(lca_5class$predclass)
colnames(pred_class) <- c("Predicted Class (5Cs)")
pred_class

tri_comp_all_noNA <- cbind(tri_comp_all_noNA, pred_class)
tri_comp_all_noNA

# Saving LCA Analysis
save(lca_4class, file = "./../01_Input/01_RData/lca_4class_analysis.RData")

########################################## TRI Components of Predicted Classes ##########################################################
#tri_comp_lca_predclass <- tri_comp_all_noNA
#tri_comp_lca_predclass[, "Optimism (OPT) 2" := round(rowMeans(tri_comp_lca_predclass[, c(1,2)], na.rm = T), 2)]
#tri_comp_lca_predclass[, "Innovativeness (INN)" := round(rowMeans(tri_comp_lca_predclass[, c(3,4,5)], na.rm = T), 2)]
#tri_comp_lca_predclass[, "Discomfort (DIS)" := round(rowMeans(tri_comp_lca_predclass[, c(6,7)], na.rm = T), 2)]
#tri_comp_lca_predclass[, "Insecurity (INS)"  := round(rowMeans(tri_comp_lca_predclass[, c(8,9,10)], na.rm = T), 2)]

# Overall TRI score for each respondent is the average score on the four dimensions
# (after reverse coding the scores on discomfort and insecurity).
# tri_comp_lca_predclass[, "Discomfort_reversed2" := 8 - `Discomfort (DIS)`]
# tri_comp_lca_predclass[, "Insecurity_reversed" := 8 - `Insecurity (INS)`]
# tri_comp_lca_predclass[, "Overall TRI" := round(rowMeans(tri_comp_lca_predclass[, c(12,13,16,17)], na.rm = T), 2)]
# tri_comp_lca_predclass[, mean(tri_comp_lca_predclass$`Overall TRI`)]


########################################## Segmentation Summary ##########################################################
tri_segments <- tri_comp_all_noNA[, c(11:14,17,18,19)]

tri_segments_5C_summary <- tri_segments[, .("N" = .N,
                                         "%" = .N / nrow(tri_segments),
                                         "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                         "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                         "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                         "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                         "Overall TRI" = mean(`Overall TRI`)), 
                                     by = "Predicted Class (5Cs)"]

tri_segments_4C_summary <- tri_segments[, .("N" = .N,
                                            "%" = .N / nrow(tri_segments),
                                            "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                            "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                            "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                            "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                            "Overall TRI" = mean(`Overall TRI`)), 
                                        by = "Predicted Class (4Cs)"]

round(tri_segments_5C_summary[order(-`Overall TRI`)],2)
round(tri_segments_4C_summary[order(-`Overall TRI`)],2)



########################################## Interpretation of Results ##########################################################

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

# class 3: EXPLORER -> PASST

# class 2: AVOIDERS -> PASST

# class 4: HESITATORS -> PASST

# class 1: SKEPTICS OR PIONEERS -> 


########################################## Saving and Cleaning ##########################################################

# Saving TRI Components, Overall TRI and Class Predictions on non-NA data
save(tri_comp_all_noNA, file = "./../01_Input/01_RData/tri_all_noNA_clusters.RData")

# Clean Environment
rm(list = ls())
