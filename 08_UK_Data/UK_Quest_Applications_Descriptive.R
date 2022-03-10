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
quest_raw <- fread("./01_Input/raw_data_field_UK_2022_02_24.csv")
load("./01_Input/00_clean_data_field_UK.RData")

quest_clean[, p_0001]
quest_clean_UK[, p_0001]


# Loading Data 
load("./01_Input/tri_all.RData")
load("./01_Input/tri_all_noNA_clusters_UK.RData")
load("./01_Input/lca_4class_analysis.RData")

source("./../04_Data_Prep/99_APA_Theme.R")

# Binding questionnaire to tri cluster data 
quest_tri_extended_UK <- cbind(quest_clean_UK, tri_comp_all_UK)
quest_tri_extended_UK

# Filter out respondents who had invalid answers on TRI questions, LCA only worked on non-NA answers
quest_tri_extended_UK <- quest_tri_extended_UK[complete.cases(quest_tri_extended_UK)]

# Map clusters to remaining data
quest_tri_extended_UK <- cbind(quest_tri_extended_UK, tri_comp_all_noNA_UK[, "Predicted Class (4Cs)"])
setnames(quest_tri_extended_UK, "Predicted Class (4Cs)", "Cluster")

# Setting names
quest_tri_extended_UK$Cluster <- as.character(quest_tri_extended_UK$Cluster)
quest_tri_extended_UK[ Cluster == "3", Cluster := "Explorers"]
quest_tri_extended_UK[ Cluster == "4", Cluster := "Hesitators"]
quest_tri_extended_UK[ Cluster == "2", Cluster := "Avoiders"]
quest_tri_extended_UK[ Cluster == "1", Cluster := "Pioneers"]

# Setting Cluster as Factor and Levels corresponding Overall TRI
quest_tri_extended_UK$Cluster <- as.factor(quest_tri_extended_UK$Cluster)
quest_tri_extended_UK$Cluster <- factor(quest_tri_extended_UK$Cluster , 
                                     levels = c("Explorers", "Pioneers", "Hesitators", "Avoiders"))

# Extract Application columns
apps <- quest_tri_extended_UK[, .(v_265, v_266, v_267, v_268, v_269, v_270, Cluster)]

apps <- melt(apps, id.vars = c("Cluster"), 
             variable.name = "Applications", value.name = "Score_Factor")

# Need Score as Factor (String) and as Integer for Calculations
apps$Score_Factor <- as.factor(apps$Score_Factor)
apps[, Score_Int := as.numeric(Score_Factor)]

# Renaming
apps[Applications == "v_265", Applications := "Tokenization of Assets"]
apps[Applications == "v_266", Applications := "Fractional ownership"]
apps[Applications == "v_267", Applications := "Self-Sovereign Identity"]
apps[Applications == "v_268", Applications := "Smart Contracts"]
apps[Applications == "v_269", Applications := "Micropayments"]
apps[Applications == "v_270", Applications := "Anonymous Transactions"]

apps

# 7-point Likert Scale: Adding Scores as Strings
apps[Score_Factor == 1, Score_Factor :="Not usefull at all"]
apps[Score_Factor == 2, Score_Factor :="Not usefull"]
apps[Score_Factor == 3, Score_Factor :="Somewhat not usefull"]
apps[Score_Factor == 4, Score_Factor :="Neutral"]
apps[Score_Factor == 5, Score_Factor :="Somewhat usefull"]
apps[Score_Factor == 6, Score_Factor :="Usefull"]
apps[Score_Factor == 7, Score_Factor :="Very usefull"]

apps$Score_Factor <- factor(apps$Score_Factor , levels = c("Not usefull at all", "Not usefull", "Somewhat not usefull", "Neutral", 
                                                           "Somewhat usefull", "Usefull", "Very usefull"))

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


########## SANITY CHECKS ##########
mean(quest_tri_extended_UK$v_209, na.rm = T)
mean(quest_tri_extended_UK$INN4, na.rm = T)
sum(is.na(quest_tri_extended_UK)) # Total of 28 NAs
rownames(quest_tri_extended_UK)[!complete.cases(quest_tri_extended_UK)] # Indices of rows with NAs
quest_tri_extended_UK[rowSums(is.na(quest_tri_extended_UK)) > 0] # Overview of rows with NA


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
            position = position_stack(vjust = 0.5), size = 2.1, family = "Times New Roman") +
  coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs( title = "Usefulness of BT application", y = "%", x = "") +
  scale_y_continuous(labels = scales::percent, minor_breaks = seq(1,25,25)) + 
  theme_apa(remove.x.gridlines = F) +
  theme(text=element_text(family="Times New Roman", size=12))

plot_apps_UK

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

########################################################### Scores by Cluster ###############################################################

#### Scores per Cluster ####
# Cluster Means on Applications
apps_cluster4C_means <- as.data.table(aggregate(Score_Int ~  Cluster, apps, mean))
apps_cluster4C_means$Score_Int <- round(apps_cluster4C_means$Score_Int, 2)
apps_cluster4C_means[order(-Score_Int)]

ggplot(apps, aes(Cluster, Score_Int)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red")

# Overview (Boxplots)
ggplot(apps, aes(Cluster, Score_Int)) + geom_boxplot() + 
  facet_grid(~ Applications) +
  stat_summary(fun=mean, geom="point", col="red")

##### A Tokenization of Assets ####
# v_265

ggplot(quest_tri_extended_UK, aes(Cluster, v_265)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Asset Tokenization")

# Likert visualization
token <- apps[Applications == "Tokenization of Assets"]

a <- token[Score_Int == 1, .("Not usefull at all (N)" = .N), by = c("Cluster")]
n_clus <- token[, .(N = .N), by = c("Cluster")]
a <- merge(a, n_clus, by = c("Cluster"))
a[, "Not usefull at all" := round(`Not usefull at all (N)` / `N`,2)][, `Not usefull at all (N)` := NULL]
a[, N := NULL]

b <- token[Score_Int == 2, .("Not usefull (N)" = .N), by = c("Cluster")]
b <- merge(b, n_clus, by = c("Cluster"))
b[, "Not usefull" := round(`Not usefull (N)` / `N`,2)][, `Not usefull (N)` := NULL]
b[, N := NULL]
a <- merge(a, b, by = "Cluster", all.x = T)

c <- token[Score_Int == 3, .("Somewhat not usefull (N)" = .N), by = c("Cluster")]
c <- merge(c, n_clus, by = c("Cluster"))
c[, "Somewhat not usefull" := round(`Somewhat not usefull (N)` / `N`,2)][, `Somewhat not usefull (N)` := NULL]
c[, N := NULL]
a <- merge(a, c, by = "Cluster", all.x = T)

d <- token[Score_Int == 4, .("Neutral (N)" = .N), by = c("Cluster")]
d <- merge(d, n_clus, by = c("Cluster"))
d[, "Neutral" := round(`Neutral (N)` / `N`,2)][, `Neutral (N)` := NULL]
d[, N := NULL]
a <- merge(a, d, by = "Cluster", all.x = T)

e <- token[Score_Int == 5, .("Somewhat usefull (N)" = .N), by = c("Cluster")]
e <- merge(e, n_clus, by = c("Cluster"))
e[, "Somewhat usefull" := round(`Somewhat usefull (N)` / `N`,2)][, `Somewhat usefull (N)` := NULL]
e[, N := NULL]
a <- merge(a, e, by = "Cluster", all.x = T)

f <- token[Score_Int == 6, .("Usefull (N)" = .N), by = c("Cluster")]
f <- merge(f, n_clus, by = c("Cluster"))
f[, "Usefull" := round(`Usefull (N)` / `N`,2)][, `Usefull (N)` := NULL]
f[, N := NULL]
a <- merge(a, f, by = "Cluster", all.x = T)

g <- token[Score_Int == 7, .("Very usefull (N)" = .N), by = c("Cluster")]
g <- merge(g, n_clus, by = c("Cluster"))
g[, "Very usefull" := round(`Very usefull (N)` / `N`,2)][, `Very usefull (N)` := NULL]
g[, N := NULL]
a <- merge(a, g, by = "Cluster", all.x = T)
a

a <- melt(a, id.vars = "Cluster")

# Final Likert visualization
ggplot(a, aes(x = factor(Cluster, levels = rev(levels(Cluster))), y = value, 
              fill = variable)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "PuOr") +
  guides(fill = guide_legend(reverse=TRUE)) + 
  labs(x = "Clusters", y = "Percentage", title = "Usefulness of Asset Tokenizations")

n_clus



##### B Fractional ownership ####
# v_266

ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_266)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Fractional ownership")


##### C Self-Sovereign Identity ####
# v_267

ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_267)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Self-Sovereign Identity")


##### D Smart Contracts ####
# v_268

ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_268)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Smart Contracts")

##### E Micropayments ####
# v_269

ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_269)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Micropayments")

##### F Anonymous Transactions ####
# v_270

ggplot(quest_tri_extended_UK, aes(x = Predicted_Class_4C, v_270)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", col="red") +
  labs(x = "Clusters", y = "Usefulness of Anonymous Transactions")








################################################################ Logistic Regression ###############################################################


# Ordinal Logistic Regression: TBD
# Dependent variable (Y) = Likert Scale -> Ordinal
# Independent vairable (X) = Categorical 
quest_tri_extended_UK$v_265 <- as.factor(quest_tri_extended_UK$v_265)

# 5 Classes OLR
olr_5C <- polr(v_265 ~ Predicted_Class_5C, data = quest_tri_extended_UK, Hess = T)
summary(olr_5C)
olr_5C$coefficients
coeffs_5C <- coef(summary(olr_5C))
p <- pnorm(abs(coeffs_5C[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs_5C, "p value" = round(p,3))

olr_4C <- polr(v_265 ~ Predicted_Class_4C, data = quest_tri_extended_UK, Hess = T)
summary(olr_4C)
olr_4C$coefficients
coeffs_4C <- coef(summary(olr_4C))
p <- pnorm(abs(coeffs_4C[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs_4C, "p value" = round(p,3))




################################################################ Saving and Cleaning ###############################################################

# Saving updated questionnaire with clusters
save(quest_tri_extended_UK, file = "./../01_Input/01_RData/01_quest_cluster_extended.RData")

# Clean Environment
rm(list = ls())




