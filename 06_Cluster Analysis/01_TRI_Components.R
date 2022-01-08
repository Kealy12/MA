#######################################################################################################################################

# Technology Readiness Index 2.0 
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
load("./../01_Input/00_clean_data_field.RData")

########################################## Extracting TRI 2.0 Components ##########################################################

# Extracting relevant questions for TRI subcomponents
tri_comp_all <- quest_clean[, .(OPT2 = v_108, OPT4 = v_109, INN1 = v_207, INN2 = v_208, INN4 = v_209, 
                            DIS2 = v_228, DIS3 = v_229, INS1 = v_230, INS2 = v_231, INS4 = v_232)]

tri_comp_all

# 0s are invalid responses that need to be filtered out
tri_comp_all[tri_comp_all == 0] <- NA

# within N=847, 25 respondents have at least 1 invalid answer (Total of 28 NAs, so multiple NAs in 1 column)
sum(is.na(tri_comp_all[, 1:10])) # Total of 28 NAs
rownames(tri_comp_all)[!complete.cases(tri_comp_all)] # Indices of rows with NAs
tri_comp_all[rowSums(is.na(tri_comp_all)) > 0] # Overview of rows with NA

########################################## TRI Components for each Respondent ##########################################################

tri_comp_all[, "Optimism (OPT)" := round(rowMeans(tri_comp_all[, c(1,2)], na.rm = T), 2)]
tri_comp_all[, "Innovativeness (INN)" := round(rowMeans(tri_comp_all[, c(3,4,5)], na.rm = T), 2)]
tri_comp_all[, "Discomfort (DIS)" := round(rowMeans(tri_comp_all[, c(6,7)], na.rm = T), 2)]
tri_comp_all[, "Insecurity (INS)"  := round(rowMeans(tri_comp_all[, c(8,9,10)], na.rm = T), 2)]

tri_comp_all

########################################## TRI Correlations ##########################################################

# Using Spearman, because we cannot assume Gaussian distribution
tri_comp <- tri_comp_all[, 11:14]
tri_comp
cor(tri_comp, method = "spearman")

#apa cor.table uses pearson correlation
apa.cor.table(tri_comp)

# visualisation of correlations
names(tri_comp) <- sub(" ", ".", names(tri_comp))
tri_comp

ggplot(tri_comp, aes(tri_comp$`Optimism.(OPT)`, tri_comp$`Discomfort.(DIS)`)) + 
  geom_point() + geom_smooth(method = "lm")

ggplot(tri_comp, aes(tri_comp$`Optimism.(OPT)`, tri_comp$`Innovativeness.(INN)`)) + 
  geom_point() + geom_smooth(method = "lm")

ggplot(tri_comp, aes(tri_comp$`Discomfort.(DIS)`, tri_comp$`Innovativeness.(INN)`)) + 
  geom_point() + geom_smooth(method = "lm")

ggplot(tri_comp, aes(tri_comp$`Optimism.(OPT)`, tri_comp$`Insecurity.(INS)`)) + 
  geom_point() + geom_smooth(method = "lm")


########################################## Means & Overall TRI for each respondent ##########################################################
# Overall TRI score for each respondent is the average score on the four dimensions
tri_comp[, "Overall.TRI" := round(rowMeans(tri_comp[, c(1,2,3,4)], na.rm = T), 2)]
tri_comp[, mean(tri_comp$`Overall.TRI`)]

# Means
tri_comp_means <- tri_comp[, .("Optimism (OPT)" = mean(`Optimism.(OPT)`),
             "Innovativeness (INN)" = mean(`Innovativeness.(INN)`),
             "Discomfort (DIS)" = mean(`Discomfort.(DIS)`),
             "Insecurity (INS)" = mean(`Insecurity.(INS)`),
             "Overall TRI" = mean(Overall.TRI))]

tri_comp_means <- melt(tri_comp_means, variable.name = "TR Components", value.name = "Mean")
tri_comp_means$Mean <- format(tri_comp_means$Mean, digits = 3)
tri_comp_means

# Printing Means Table
print(xtable(tri_comp_means, type = "latex"), file = "./../02_Output/TRI_Components_Means.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(tri_comp_means)))


########################################## Saving and Cleaning ##########################################################
# Saving
save(tri_comp_all, file = "./../01_Input/01_RData/01_lca_analysis.RData")

# Clean Environment
rm(list = ls())

