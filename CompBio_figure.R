setwd("~/Desktop/DDIT3_Project") #set working directory

#load libraries
library(TCGAbiolinks)
library(SummarizedExperiment)
library(tidyverse)
library(ggpubr)
library(patchwork)

plot_data <- readRDS("DDIT3_Figure1_plot_data.rds") #load datafile

# fix PAM50 order to match original figure
plot_data$pam50_clean <- factor(
  plot_data$pam50_clean,
  levels = c("Normal", "LumB", "LumA", "Her2", "Basal")
)

#check data
head(plot_data)
table(plot_data$stage_clean)
table(plot_data$age_group)
table(plot_data$pam50_clean)


#Figure 1A (by stage)
fig1A_data <- plot_data %>% 
  filter(!is.na(stage_clean))

fig1A <- ggplot(fig1A_data, aes(x = stage_clean, y = DDIT3, fill = stage_clean)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "gray40") +
  stat_compare_means(method = "anova", label = "p.signif", size = 5) +
  scale_fill_manual(
    values = c(
      "I" = "#F8866D",
      "II" = "#A5E0F1",
      "III" = "#39AA9B",
      "IV" = "#9EA9C3"
    )
  ) +
  labs(
    title = "A",
    x = "DDIT3",
    y = "Expression",
    fill = "Stage"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

fig1A


#Figure 1B (by age)
fig1B_data <- plot_data %>% 
  filter(!is.na(age_group))

fig1B <- ggplot(fig1B_data, aes(x = age_group, y = DDIT3, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "gray40") +
  stat_compare_means(method = "t.test", label = "p.signif", size = 5) +
  scale_fill_manual(
    values = c(
      "<60" = "#F8866D",
      ">=60" = "#A5E0F1"
    )
  ) +
  labs(
    title = "B",
    x = "DDIT3",
    y = "Expression",
    fill = "Age"
  ) +
  theme_bw() +
  theme(
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
  stat_compare_means(method = "anova", label = "p.signif", size = 5) +
  scale_fill_manual(
    values = c(
      "Normal" = "#F8866C",
      "LumB" = "#A5E0F1",
      "LumA" = "#39AA9B",
      "Her2" = "#9EA9C3",
      "Basal" = "#F9CDBF"
    )
  ) +
  labs(
    title = "C",
    x = "DDIT3",
    y = "Expression",
    fill = "Pam50"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

fig1C


#save figures
ggsave(
  filename = "Figure_1A_DDIT3_Stage.png",
  plot = fig1A,
  width = 6,
  height = 4,
  dpi = 300
)

ggsave(
  filename = "Figure_1B_DDIT3_Age.png",
  plot = fig1B,
  width = 6,
  height = 4,
  dpi = 300
)

ggsave(
  filename = "Figure_1C_DDIT3_PAM50.png",
  plot = fig1C,
  width = 6,
  height = 4,
  dpi = 300
)


#Save combined figure
combined_fig1 <- fig1A / fig1B / fig1C
combined_fig1
ggsave(
  filename = "Figure_1ABC_DDIT3_combined.png",
  plot = combined_fig1,
  width = 7,
  height = 11,
  dpi = 300
)

list.files() #check files
