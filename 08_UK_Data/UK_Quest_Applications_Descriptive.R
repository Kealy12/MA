#######################################################################################################################################

# Descriptive Stats on blockchain applications and clusters

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
library(MASS)

# Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading Data 
quest_raw <- fread("./01_Input/raw_data_field_UK_final.csv")
load("./01_Input/00_clean_data_field_UK.RData")

# Loading Data 
load("./01_Input/tri_all.RData")
load("./01_Input/tri_all_noNA_clusters_UK.RData")
load("./01_Input/lca_4class_analysis.RData")

source("./../04_Data_Prep/99_APA_Theme.R")

# Binding questionnaire to tri cluster data 
quest_tri_extended_UK <- cbind(quest_clean_UK, tri_comp_all_UK)
quest_tri_extended_UK

# Filter out respondents who had invalid answers on TRI questions, LCA only worked on non-NA answers
# quest_tri_extended_UK <- quest_tri_extended_UK[complete.cases(quest_tri_extended_UK)]

# # Map clusters to remaining data
# quest_tri_extended_UK <- cbind(quest_tri_extended_UK, tri_comp_all_noNA_UK[, "Predicted Class (4Cs)"])
# setnames(quest_tri_extended_UK, "Predicted Class (4Cs)", "Cluster")
# 
# # Setting names
# quest_tri_extended_UK$Cluster <- as.character(quest_tri_extended_UK$Cluster)
# quest_tri_extended_UK[ Cluster == "4", Cluster := "Explorers"]
# quest_tri_extended_UK[ Cluster == "1", Cluster := "Hesitators"]
# quest_tri_extended_UK[ Cluster == "2", Cluster := "Avoiders"]
# quest_tri_extended_UK[ Cluster == "3", Cluster := "Pioneers"]

# # Setting Cluster as Factor and Levels corresponding Overall TRI
# quest_tri_extended_UK$Cluster <- as.factor(quest_tri_extended_UK$Cluster)
# quest_tri_extended_UK$Cluster <- factor(quest_tri_extended_UK$Cluster , 
#                                      levels = c("Explorers", "Pioneers", "Hesitators", "Avoiders"))

# Extract Application columns
apps <- quest_tri_extended_UK[, .(v_265, v_266, v_267, v_268, v_269, v_270)]
apps <- melt(apps,
             variable.name = "Applications", value.name = "Score_Factor")

# Need Score as Factor (String) and as Integer for Calculations
apps$Score_Factor <- as.factor(apps$Score_Factor)
apps[, Score_Int := as.numeric(Score_Factor)]

# Renaming
apps[Applications == "v_265", Applications := "Tokenization of assets"]
apps[Applications == "v_266", Applications := "Fractional ownership"]
apps[Applications == "v_267", Applications := "Self-sovereign identity"]
apps[Applications == "v_268", Applications := "Smart contracts"]
apps[Applications == "v_269", Applications := "Micropayments"]
apps[Applications == "v_270", Applications := "Anonymous transactions"]

apps

# 7-point Likert Scale: Adding Scores as Strings
apps[Score_Factor == 1, Score_Factor :="Not useful at all"]
apps[Score_Factor == 2, Score_Factor :="Not useful"]
apps[Score_Factor == 3, Score_Factor :="Somewhat not useful"]
apps[Score_Factor == 4, Score_Factor :="Neutral"]
apps[Score_Factor == 5, Score_Factor :="Somewhat useful"]
apps[Score_Factor == 6, Score_Factor :="Useful"]
apps[Score_Factor == 7, Score_Factor :="Very useful"]

apps$Score_Factor <- factor(apps$Score_Factor , levels = c("Not useful at all", "Not useful", "Somewhat not useful", "Neutral", 
                                                           "Somewhat useful", "Useful", "Very useful"))

# % distribution of answers
apps[Score_Int == 1 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 2 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 3 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 4 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 5 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 6 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]
apps[Score_Int == 7 , dis_by_app := .N / nrow(quest_tri_extended_UK), by = "Applications"]

apps_summary <- unique(apps[ ,c("Applications", "Score_Factor", "dis_by_app")])

apps
apps_summary

########################################################### Scores on Applications ###############################################################

#### Scores Overall ####
# Mean Likert (1-7) Scores of Applications
apps_means <- as.data.table(aggregate(Score_Int ~  Applications, apps, mean))
apps_means$Score_Int <- round(apps_means$Score_Int, 2)
apps_means[order(-Score_Int)]

# Visualization of Scores of each Application
ggplot(apps, aes(Applications, Score_Int)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  geom_text(data = apps_means, aes(label = Score_Int, y = Score_Int + 0.3), size = 3)

# Visualization of answers (%) on applications 
apps_summary <- merge(apps_summary, apps_means, by = "Applications")

plot_apps_UK <- ggplot(apps_summary, aes(x = reorder(Applications, Score_Int), y = dis_by_app, fill = Score_Factor)) +
  geom_bar(position = "stack", stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::percent(dis_by_app,accuracy = 1, trim = FALSE)), 
            position = position_stack(vjust = 0.5), size = 4, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "UK", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=25))+
  theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=12))

plot_apps_UK
ggsave("plot_apps_UK.png", plot_apps_UK)


##### Significant difference between Applications - Welch t-Tests #####
# Welch T-Test because we cannot assume equal variance
apps_t_test <- quest_clean_UK[, .(v_265, v_266, v_267, v_268, v_269, v_270)]
# Renaming
colnames(apps_t_test) <- c("Tokenization", "Fractional", "Identity",
                           "Smart", "Micropayments", "Anonymous")

apps_t_test <- melt(apps_t_test)

# SSI vs. Tokenization of Assets
ssi_token <- apps_t_test[variable %in% c("Identity", "Tokenization")]
t.test(value ~ variable, data = ssi_token, alternative = "two.sided" , var.equal=F)
# p-value < 0.05 -> significant difference

# Tokenization of Assets vs. Anonymous Transactions
token_anony <- apps_t_test[variable %in% c("Anonymous", "Tokenization")]
t.test(value ~ variable, data = token_anony, alternative = "two.sided" , var.equal=F)
# p-value > 0.05 -> NO significant difference

# Anonymous Transactions vs. Smart Contracts
anony_smart <- apps_t_test[variable %in% c("Anonymous", "Smart")]
t.test(value ~ variable, data = anony_smart, alternative = "two.sided" , var.equal=F)
# p-value < 0.05 -> Significant difference

# Smart Contracts vs. Micropayments
smart_micro <- apps_t_test[variable %in% c("Smart", "Micropayments")]
t.test(value ~ variable, data = smart_micro, alternative = "two.sided" , var.equal=F)
# p-value > 0.05 -> NO Significant difference

# Micropayments vs. Fractional Ownership
micro_fractional <- apps_t_test[variable %in% c("Micropayments", "Fractional")]
t.test(value ~ variable, data = micro_fractional, alternative = "two.sided" , var.equal=F)
# p-value < 0.05 -> Significant difference

# Tests between groups_
beyond_group <- apps_t_test[variable %in% c("Anonymous", "Micropayments")]
t.test(value ~ variable, data = beyond_group, alternative = "two.sided" ,var.equal=F)
# p-value < 0.05 -> Significant difference

# Conclusion:
# -> 3 Groups, where there is no significant difference: SSI & token, Anony & Smart Cs, Microp & Frac. Own
# -> Significant Differences between these groups

dim(quest_clean_UK)

########################################################### NOT in Paper anymore: Scores by Cluster ###############################################################

# #### Scores per Cluster ####
# # Cluster Means on Applications
# apps_cluster4C_means <- as.data.table(aggregate(Score_Int ~  Cluster, apps, mean))
# apps_cluster4C_means$Score_Int <- round(apps_cluster4C_means$Score_Int, 2)
# apps_cluster4C_means[order(-Score_Int)]
# 
# ggplot(apps, aes(Cluster, Score_Int)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red")
# 
# # Overview (Boxplots)
# ggplot(apps, aes(Cluster, Score_Int)) + geom_boxplot() + 
#   facet_grid(~ Applications) +
#   stat_summary(fun=mean, geom="point", col="red")
# 
# ##### A Tokenization of Assets ####
# # v_265
# 
# ggplot(quest_tri_extended_UK, aes(Cluster, v_265)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Asset Tokenization")
# 
# # Likert visualization
# token <- apps[Applications == "Tokenization of Assets"]
# 
# a <- token[Score_Int == 1, .("Not usefull at all (N)" = .N), by = c("Cluster")]
# n_clus <- token[, .(N = .N), by = c("Cluster")]
# a <- merge(a, n_clus, by = c("Cluster"))
# a[, "Not usefull at all" := round(`Not usefull at all (N)` / `N`,2)][, `Not usefull at all (N)` := NULL]
# a[, N := NULL]
# 
# b <- token[Score_Int == 2, .("Not usefull (N)" = .N), by = c("Cluster")]
# b <- merge(b, n_clus, by = c("Cluster"))
# b[, "Not usefull" := round(`Not usefull (N)` / `N`,2)][, `Not usefull (N)` := NULL]
# b[, N := NULL]
# a <- merge(a, b, by = "Cluster", all.x = T)
# 
# c <- token[Score_Int == 3, .("Somewhat not usefull (N)" = .N), by = c("Cluster")]
# c <- merge(c, n_clus, by = c("Cluster"))
# c[, "Somewhat not usefull" := round(`Somewhat not usefull (N)` / `N`,2)][, `Somewhat not usefull (N)` := NULL]
# c[, N := NULL]
# a <- merge(a, c, by = "Cluster", all.x = T)
# 
# d <- token[Score_Int == 4, .("Neutral (N)" = .N), by = c("Cluster")]
# d <- merge(d, n_clus, by = c("Cluster"))
# d[, "Neutral" := round(`Neutral (N)` / `N`,2)][, `Neutral (N)` := NULL]
# d[, N := NULL]
# a <- merge(a, d, by = "Cluster", all.x = T)
# 
# e <- token[Score_Int == 5, .("Somewhat usefull (N)" = .N), by = c("Cluster")]
# e <- merge(e, n_clus, by = c("Cluster"))
# e[, "Somewhat usefull" := round(`Somewhat usefull (N)` / `N`,2)][, `Somewhat usefull (N)` := NULL]
# e[, N := NULL]
# a <- merge(a, e, by = "Cluster", all.x = T)
# 
# f <- token[Score_Int == 6, .("Usefull (N)" = .N), by = c("Cluster")]
# f <- merge(f, n_clus, by = c("Cluster"))
# f[, "Usefull" := round(`Usefull (N)` / `N`,2)][, `Usefull (N)` := NULL]
# f[, N := NULL]
# a <- merge(a, f, by = "Cluster", all.x = T)
# 
# g <- token[Score_Int == 7, .("Very usefull (N)" = .N), by = c("Cluster")]
# g <- merge(g, n_clus, by = c("Cluster"))
# g[, "Very usefull" := round(`Very usefull (N)` / `N`,2)][, `Very usefull (N)` := NULL]
# g[, N := NULL]
# a <- merge(a, g, by = "Cluster", all.x = T)
# a
# 
# a <- melt(a, id.vars = "Cluster")
# 
# # Final Likert visualization
# ggplot(a, aes(x = factor(Cluster, levels = rev(levels(Cluster))), y = value, 
#               fill = variable)) +
#   geom_bar(position = "stack", stat = "identity") +
#   coord_flip() +
#   scale_fill_brewer(palette = "PuOr") +
#   guides(fill = guide_legend(reverse=TRUE)) + 
#   labs(x = "Clusters", y = "Percentage", title = "Usefulness of Asset Tokenizations")
# 
# n_clus
# 
# 
# 
# ##### B Fractional ownership ####
# # v_266
# 
# ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_266)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Fractional ownership")
# 
# 
# ##### C Self-Sovereign Identity ####
# # v_267
# 
# ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_267)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Self-Sovereign Identity")
# 
# 
# ##### D Smart Contracts ####
# # v_268
# 
# ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_268)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Smart Contracts")
# 
# ##### E Micropayments ####
# # v_269
# 
# ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_269)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Micropayments")
# 
# ##### F Anonymous Transactions ####
# # v_270
# 
# ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_270)) + geom_boxplot() +
#   stat_summary(fun=mean, geom="point", col="red") +
#   labs(x = "Clusters", y = "Usefulness of Anonymous Transactions")
# 
# 
# 
# 
# 
# 
# 
# 
# 


################################################################ Saving and Cleaning ###############################################################

# Saving updated questionnaire with clusters
save(quest_tri_extended_UK, file = "./01_Input/01_quest_cluster_extended_UK.RData")

# Clean Environment
rm(list = ls())




