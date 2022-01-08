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
load("./../01_Input/01_RData/01_lca_analysis.RData")

########################################## LCA Predicted Classes on scores of 10 TRI items ##########################################################
tri_comp_raw <- tri_comp_all[, 1:10]
f1 <- as.formula(cbind(OPT2, OPT4, INN1, INN2, INN4, DIS2, DIS3, INS1, INS2, INS4)~1)

# LCA can only be computed on rows with no NA, so these rows need to be filtered out
tri_comp_raw_noNA<- tri_comp_raw[complete.cases(tri_comp_raw)]

# N of LCA = 822 due to invalid answers
# (Parasuraman and Colby (2015): N of survey to LCA also reduced from 878 to 782)
nrow(tri_comp_raw_noNA)

# 5 classes, Cf. Parasuraman and Colby (2015)
# nrep = 15 to ensure to ensure that the algorithm finds a global rather than local maximum of the log-likelihood function (greatest value)
lca <- poLCA(f1, data = tri_comp_raw_noNA, nclass = 5, na.rm = T, nrep = 15)

# Predicted Classes
pred_class <- as.data.table(lca$predclass)
colnames(pred_class) <- c("Predicted Class")
pred_class

tri_comp_raw_noNA <- cbind(tri_comp_raw_noNA, pred_class)

########################################## TRI Components of Predicted Classes ##########################################################
tri_comp_lca_predclass <- tri_comp_raw_noNA
tri_comp_lca_predclass[, "Optimism (OPT)" := round(rowMeans(tri_comp_lca_predclass[, c(1,2)], na.rm = T), 2)]
tri_comp_lca_predclass[, "Innovativeness (INN)" := round(rowMeans(tri_comp_lca_predclass[, c(3,4,5)], na.rm = T), 2)]
tri_comp_lca_predclass[, "Discomfort (DIS)" := round(rowMeans(tri_comp_lca_predclass[, c(6,7)], na.rm = T), 2)]
tri_comp_lca_predclass[, "Insecurity (INS)"  := round(rowMeans(tri_comp_lca_predclass[, c(8,9,10)], na.rm = T), 2)]

# Overall TRI score for each respondent is the average score on the four dimensions
tri_comp_lca_predclass[, "Overall TRI" := round(rowMeans(tri_comp_lca_predclass[, c(12,13,14,15)], na.rm = T), 2)]
tri_comp_lca_predclass[, mean(tri_comp_lca_predclass$`Overall TRI`)]

tri_comp_lca_predclass

########################################## Segmentation Summary ##########################################################
tri_segments <- tri_comp_lca_predclass[, 11:16]

tri_segments_summary <- tri_segments[, .("N" = .N,
                                         "%" = .N / nrow(tri_segments),
                                         "Optimism (OPT)" = mean(`Optimism (OPT)`),
                                         "Innovativeness (INN)" = mean(`Innovativeness (INN)`),
                                         "Discomfort (DIS)" = mean(`Discomfort (DIS)`),
                                         "Insecurity (INS)" = mean(`Insecurity (INS)`),
                                         "Overall TRI" = mean(`Overall TRI`)), 
                                     by = "Predicted Class"][order(`Predicted Class`)]

tri_segments_summary[order(`Discomfort (DIS)`)]

########################################## Interpretation of Results ##########################################################

# class 3: EXPLORER (14%)
# highest TRI, highest OPT, highest INN, highest discomfort, Resistance 
# (both strong positive and negative views about technology) -> PIONEER (13,6%)

# class 1: PIONEER (23%)
# high degree of motivation and low degree of resistance (relatively low )

# class 2: SKEPTICS
# largest segment, 


# class 4:
# second highest motivation, lowest resistance  -> EXPLORER (25%)?

# class 5: 
# lowest in motivation, very high resistance-> AVOIDERS (19%) 



# class 2:
# highest resistance, low INN, 

Pioneers
Explorers
Skeptics
Hesitators 



# Saving LCA Analysis
save(lca, file = "./../01_Input/01_lca_analysis.RData")
