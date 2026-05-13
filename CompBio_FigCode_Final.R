setwd("~/Desktop/DDIT3_Project") #set working directory

#load libraries
library(TCGAbiolinks) #used to analyze TCGA genome data
library(SummarizedExperiment) #store TCGA
library(tidyverse)
library(ggpubr) #for stat_compare_mean

plot_data <- readRDS("DDIT3_Figure1_plot_data.rds") #load data file

# switch PAM50 order to match original fig
plot_data$pam50_clean <- factor(
  plot_data$pam50_clean,
  levels = c("Normal", "LumB", "LumA", "Her2", "Basal")
)

#Figure 1A (by stage)
fig1A_data <- plot_data %>% 
  filter(!is.na(stage_clean)) #prevent warnings for incomplete data

fig1A <- ggplot(fig1A_data, aes(x = stage_clean, y = DDIT3, fill = stage_clean)) + #fill=colors
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
    # trim = FALSE ~ full shape of distribution
    # alpha = 0.75 ~ slightly transparent.
    # color ~NA removes outline
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "gray40") +
  stat_compare_means(method = "anova", label = "p.signif", size = 5, family = "Times New Roman") +
  scale_fill_manual( #manually set colors
    values = c(
      "I" = "#F8866D",
      "II" = "#A5E0F1",
      "III" = "#39AA9B",
      "IV" = "#9EA9C3"
    )
  ) +
  guides(fill = guide_legend(title = "Stage")) +
  ggtitle("A") +
  xlab("DDIT3") +
  ylab("Expression") + 
  theme_bw() + #black/white theme
  theme( #Extra Formating
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14, face = "bold"), #title
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank() #remove minor grid lines
  )

fig1A


#Figure 1B (by age)
fig1B_data <- plot_data %>% 
  filter(!is.na(age_group))

fig1B <- ggplot(fig1B_data, aes(x = age_group, y = DDIT3, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "gray40") +
  stat_compare_means(method = "t.test", label = "p.signif", size = 5, family = "Times New Roman") +
  scale_fill_manual(
    values = c(
      "<60" = "#F8866D",
      ">=60" = "#A5E0F1"
    )
  ) +
  guides(fill = guide_legend(title = "Age")) +
  ggtitle("B") +
  xlab("DDIT3") +
  ylab("Expression") + 
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

fig1B



#Figure 1C (by PAM50 subtype)

fig1C_data <- plot_data %>% 
  filter(!is.na(pam50_clean))

fig1C <- ggplot(fig1C_data, aes(x = pam50_clean, y = DDIT3, fill = pam50_clean)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "gray40") +
  stat_compare_means(method = "anova", label = "p.signif", size = 5, family = "Times New Roman") +
  scale_fill_manual(
    values = c(
      "Normal" = "#F8866C",
      "LumB" = "#A5E0F1",
      "LumA" = "#39AA9B",
      "Her2" = "#9EA9C3",
      "Basal" = "#F9CDBF"
    )
  ) +
  guides(fill = guide_legend(title = "Pam50")) +
  ggtitle("C") +
  xlab("DDIT3") +
  ylab("Expression") + 
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank() 
  )

fig1C

#save figures
ggsave(filename = "Figure_1A_Stage.png", plot = fig1A, width = 6, height = 4, dpi = 300)
ggsave(filename = "Figure_1B_Age.png", plot = fig1B, width = 6,height = 4, dpi = 300)
ggsave(filename = "Figure_1C_PAM50.png", plot = fig1C, width = 6, height = 4, dpi = 300)

list.files() #check files
