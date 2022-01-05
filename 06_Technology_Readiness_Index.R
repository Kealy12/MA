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

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
load("./01_Input/00_clean_data_field.RData")

########################################## Extracting TRI 2.0 Components ##########################################################

# Extracting relevant questions for TRI subcomponents
tri_comp <- quest_clean[, .(OPT2 = v_108, OPT4 = v_109, INN1 = v_207, INN2 = v_208, INN4 = v_209, 
                            DIS2 = v_228, DIS3 = v_229, INS1 = v_230, INS2 = v_231, INS4 = v_232)]

tri_comp

# Mean in each SUBcomponent
means_tri_subcomp <- tri_comp[, .(Mean_OPT2 = round(mean(OPT2, na.rm = T), 2),
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

########################################## TRI Components for each Respondent ##########################################################




# Clean Environment
rm(list = ls())

