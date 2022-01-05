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

# Extracting relevant questions for TRI components
tri_comp <- quest_clean[, .(OPT2 = v_108, OPT4 = v_109, INN1 = v_207, INN2 = v_208, INN4 = v_209, 
                            DIS2 = v_228, DIS3 = v_229, INS1 = v_230, INS2 = v_231, INS4 = v_232)]

tri_comp

tri_comp[, .(Mean_OPT2 = round(mean(OPT2, na.rm = T), 2),
             Mean_OPT4 = round(mean(OPT4, na.rm = T), 2))]



tri_comp[, mean = ]

