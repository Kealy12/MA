#######################################################################################################################################

# Technology Readiness Index 2.0 - Components
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
quest_raw <- fread("./01_Input/raw_data_field_UK_2022_02_24.csv")
load("./01_Input/00_clean_data_field_UK.RData")

########################################## Extracting TRI 2.0 Components ##########################################################

# Extracting relevant questions for TRI subcomponents
tri_comp_all_UK <- quest_clean_UK[, .(OPT2 = v_108, OPT4 = v_109, INN1 = v_207, INN2 = v_208, INN4 = v_209, 
                                DIS2 = v_228, DIS3 = v_229, INS1 = v_230, INS2 = v_231, INS4 = v_232)]

tri_comp_all_UK

# 0s are invalid responses that need to be filtered out
tri_comp_all_UK[tri_comp_all_UK == 0] <- NA

# within N=847, 25 respondents have at least 1 invalid answer (Total of 28 NAs, so multiple NAs in 1 column)
sum(is.na(tri_comp_all_UK[, 1:10])) # Total of 28 NAs
rownames(tri_comp_all_UK)[!complete.cases(tri_comp_all_UK)] # Indices of rows with NAs
tri_comp_all_UK[rowSums(is.na(tri_comp_all_UK)) > 0] # Overview of 25 rows with NA

########################################## Reverse Coding of Discomfort ##########################################################
# Reverse Coding of DIS2 necessary: Uncomfortable = 7, Comfortable = 1
tri_comp_all_UK$DIS2 <- tri_comp_all_UK[, 8 - DIS2]


########################################## TRI Components for each Respondent ##########################################################

tri_comp_all_UK[, "Optimism (OPT)" := round(rowMeans(tri_comp_all_UK[, c(1,2)], na.rm = T), 2)]
tri_comp_all_UK[, "Innovativeness (INN)" := round(rowMeans(tri_comp_all_UK[, c(3,4,5)], na.rm = T), 2)]
tri_comp_all_UK[, "Discomfort (DIS)" := round(rowMeans(tri_comp_all_UK[, c(6,7)], na.rm = T), 2)]
tri_comp_all_UK[, "Insecurity (INS)"  := round(rowMeans(tri_comp_all_UK[, c(8,9,10)], na.rm = T), 2)]

tri_comp_all_UK

ggplot(tri_comp_all_UK, aes(INS4)) + geom_histogram()

########################################## TRI Correlations ##########################################################

# Using Spearman, because we cannot assume Gaussian distribution
tri_comp <- tri_comp_all_UK[, 11:14]
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

# The overall TRI score for each respondent was the average score on the four dimensions 
# (after reverse coding the scores on discomfort and insecurity). Cf. Parasuraman and Colby (2015)
tri_comp[, "Discomfort_reversed" := 8 - `Discomfort.(DIS)`]
tri_comp[, "Insecurity_reversed" := 8 - `Insecurity.(INS)`]
tri_comp[, "Overall TRI" := round(rowMeans(tri_comp[, c(1,2,5,6)], na.rm = T), 2)]
tri_comp[, mean(tri_comp$`Overall TRI`)]

# Means
tri_comp_means <- tri_comp[, .("Optimism (OPT)" = mean(`Optimism.(OPT)`),
                               "Innovativeness (INN)" = mean(`Innovativeness.(INN)`),
                               "Discomfort (DIS)" = mean(`Discomfort.(DIS)`),
                               "Insecurity (INS)" = mean(`Insecurity.(INS)`),
                               "Overall TRI" = mean(`Overall TRI`))]

tri_comp_means <- melt(tri_comp_means, variable.name = "TR Components", value.name = "Mean")
tri_comp_means$Mean <- format(tri_comp_means$Mean, digits = 3)
tri_comp_means

##########################################  Writing to Excel and Printing Table ########################################## 
print(xtable(tri_comp_means, type = "latex"), file = "./02_Output/TRI_Components_Means.tex",include.rownames = F, only.contents = T, include.colnames = T, hline.after = c(nrow(tri_comp_means)))
sheets <- list("tri_comp_means" = tri_comp_means)
write_xlsx(sheets, "./02_Output/TRI_Components_Means.xlsx")


########################################## Saving and Cleaning ##########################################################
# Merging Results 
tri_comp_all_UK <- cbind(tri_comp_all_UK,tri_comp)
tri_comp_all_UK[, c("Optimism.(OPT)", "Innovativeness.(INN)", "Discomfort.(DIS)", "Insecurity.(INS)") := NULL]
tri_comp_all_UK

# Saving
save(tri_comp_all_UK, file = "./01_Input/tri_all.RData")

# Clean Environment
rm(list = ls())

