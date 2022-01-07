#######################################################################################################################################

# Technology Readiness Index 2.0 
# Cf. Parasuraman and Colby (2015)

################################################################ Set Up ###############################################################

# Packages
library(data.table)
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
load("./01_Input/00_clean_data_field.RData")

########################################## Extracting TRI 2.0 Components ##########################################################

# Extracting relevant questions for TRI subcomponents
tri_comp_all <- quest_clean[, .(OPT2 = v_108, OPT4 = v_109, INN1 = v_207, INN2 = v_208, INN4 = v_209, 
                            DIS2 = v_228, DIS3 = v_229, INS1 = v_230, INS2 = v_231, INS4 = v_232)]

tri_comp_all

# Mean in each SUBcomponent
means_tri_subcomp <- tri_comp_all[, .(Mean_OPT2 = round(mean(OPT2, na.rm = T), 2),
                                  Mean_OPT4 = round(mean(OPT4, na.rm = T), 2), 
                                  Mean_INN1 = round(mean(INN1, na.rm = T), 2),
                                  Mean_INN2 = round(mean(INN2, na.rm = T), 2),
                                  Mean_INN4 = round(mean(INN4, na.rm = T), 2),
                                  Mean_DIS2 = round(mean(DIS2, na.rm = T), 2),
                                  Mean_DIS3 = round(mean(DIS3, na.rm = T), 2), 
                                  Mean_INS1 = round(mean(INS1, na.rm = T), 2),
                                  Mean_INS2 = round(mean(INS2, na.rm = T), 2),
                                  Mean_INS4 = round(mean(INS4, na.rm = T), 2)
                                   )]
means_tri_subcomp

# Mean in each TRI component
means_tri_subcomp <- melt(means_tri_subcomp, variable.name = "Component", value.name = "Mean")
means_tri_subcomp

opt_mean <- means_tri_subcomp[ 1:2, .("Optimism (OPT)" = round(mean(Mean),2))]
inn_mean <- means_tri_subcomp[ 3:5, .("Innovativeness (INN)" = round(mean(Mean),2))]
dis_mean <- means_tri_subcomp[ 6:7, .("Discomfort (DIS)" = round(mean(Mean), 2))]
ins_mean <- means_tri_subcomp[ 8:10,.("Insecurity (INS)" = round(mean(Mean), 2))]

tri_comp_mean <- cbind(opt_mean, inn_mean, dis_mean, ins_mean)
tri_comp_mean <- melt(tri_comp_mean, variable.name = "TR Components", value.name = "Mean")

tri_comp_mean

# Printing Table
print(xtable(tri_comp_mean, type = "latex"), file = "./02_Output/TRI_Components_Mean.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(tri_comp_mean)))

########################################## TRI Components for each Respondent ##########################################################

tri_comp_all[, "Optimism (OPT)" := round((tri_comp_all$OPT2 + tri_comp_all$OPT4) / 2,2)]
tri_comp_all[, "Innovativeness (INN)" := round((tri_comp_all$INN1 + tri_comp_all$INN2 + tri_comp_all$INN4) / 3,2)]
tri_comp_all[, "Discomfort (DIS)" := round((tri_comp_all$DIS2 + tri_comp_all$DIS3) / 2,2)]
tri_comp_all[, "Insecurity (INS)"  := round((tri_comp_all$INS1 + tri_comp_all$INS2 + tri_comp_all$INS4) / 3,2)]

tri_comp_all
tri_comp_short <- copy(tri_comp_all)

########################################## TRI Correlations ##########################################################

# Using Spearman, because we cannot assume Gaussian distribution
tri_comp <- tri_comp_short[, 1:10 := NULL]
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


########################################## Overall TRI for each respondent ##########################################################
# Overall TRI score for each respondent is the average score on the four dimensions
tri_comp[, "Overall.TRI" := round((tri_comp$`Optimism.(OPT)` + tri_comp$`Innovativeness.(INN)` +
                                     tri_comp$`Discomfort.(DIS)` + tri_comp$`Insecurity.(INS)`) / 4,2)]

tri_comp[, mean(tri_comp$`Overall.TRI`)]

tri_comp

########################################## LCA on scores of 10 TRI items ##########################################################
# 0 -> have to change 0s to positve number!
tri_comp_all
f1 <- as.formula(cbind(tri_comp_all$OPT2, tri_comp_all$OPT4, tri_comp_all$INN1, 
                       tri_comp_all$INN2, tri_comp_all$INN4, tri_comp_all$DIS2,
                       tri_comp_all$DIS3, tri_comp_all$INS1, tri_comp_all$INS2,
                       tri_comp_all$INS4)~1)
poLCA(f1, data = tri_comp_all, nclass = 5)

tri_comp_all[,.N, by = OPT4]




# Clean Environment
rm(list = ls())

