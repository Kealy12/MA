#######################################################################################################################################

# General Questionnaire Stats 

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

################################################################ General Stats of questionnaire ###############################################################

# N
quest_clean[, .N]

# Gender distribution



# Age distribution



# Have you heard of following terms?
# v_321





# Gender distribution 
quest_clean[, "v_169"]
ggplot(quest_clean[, "v_169"], aes(v_169)) + 
  geom_bar()

# Age distribution 
quest_raw[, "v_285"]
ggplot(quest_raw[, "v_285"], aes(v_285)) + 
  geom_histogram() + theme_apa()

# Difference beteen Bitcoin and Blockchain? 
difference <- quest_raw[, "v_282"]
difference <- difference[, v_282 := gsub(1, "yes", v_282)]
difference <- difference[, v_282 := gsub(2, "no", v_282)]

ggplot(difference, aes(v_282)) + 
  geom_bar()

quest_raw[v_282 == 2, "v_282"][, .N]



# Overall Blockchain knowledge (before and after the survey)






# Clean Environment
rm(list = ls())


#### GGplot ####
ggplot(dt, aes(x, y, fill = ..., color = ...)) +
  # USE HISTOGRAMS TO LOOK AT DIFFERENT DISTRIBUTIONS!
  geom_histogram(position = "dodge") + geom_bar() +
  geom_jitter() + geom_point() + geom_smooth()
geom_boxplot(width = ..., factor(x)) + geom_violin() + geom_dotplot(binaxis="y", stackdir="center", dotsize=0.3) +
  geom_line( "x has to be numeric") + geom_text_repel(aes(label = ...))
scale_x_log10() + scale_y_log10() +
  facet_grid(~..., scales = "free") + facet_wrap() +
  stat_summary(fun.y=mean, geom="point", col="darkred") +
  labs(x = "...", y = "...", title = "...") +
  guides(fill/color = guide_legend(title = "..."))


