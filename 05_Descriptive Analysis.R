
# Gender distribution 
quest_raw[, "v_169"]
ggplot(quest_raw[, "v_169"], aes(v_169)) + 
  geom_bar() + theme_apa()

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


