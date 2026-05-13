setwd("~/Desktop/DDIT3_Project")

library(TCGAbiolinks)
library(SummarizedExperiment)
library(tidyverse)

exists("brca")

#TPM expression
expr <- assay(brca, "tpm_unstrand")
gene_info <- rowData(brca)

expr_df <- as.data.frame(expr)
expr_df$gene <- gene_info$gene_name


#make expression data long
expr_long <- expr_df %>% 
  pivot_longer(
    cols = -gene,
    names_to = "sample",
    values_to = "expression"
  )


#only DDIT3 expression
ddit3_df <- expr_long %>% 
  filter(gene == "DDIT3") %>% 
  select(sample, DDIT3 = expression)

ddit3_df <- ddit3_df %>% 
  mutate(DDIT3 = log2(DDIT3 + 1))


#Clinical data from brca
clinical <- as.data.frame(colData(brca))

clinical_df <- data.frame(
  sample = rownames(clinical),
  age = clinical$age_at_index,
  stage = clinical$ajcc_pathologic_stage,
  pam50 = clinical$paper_BRCA_Subtype_PAM50
)


#Combine expression and clinical data
plot_data <- ddit3_df %>% 
  left_join(clinical_df, by = "sample")


#clean data for figures
plot_data <- plot_data %>% 
  mutate(
    age_group = case_when(
      age < 60 ~ "<60",
      age >= 60 ~ ">=60",
      TRUE ~ NA_character_
    ),
    
    stage_clean = case_when(
      stage == "Stage I" ~ "I",
      stage == "Stage IA" ~ "I",
      stage == "Stage IB" ~ "I",
      stage == "Stage II" ~ "II",
      stage == "Stage IIA" ~ "II",
      stage == "Stage IIB" ~ "II",
      stage == "Stage III" ~ "III",
      stage == "Stage IIIA" ~ "III",
      stage == "Stage IIIB" ~ "III",
      stage == "Stage IIIC" ~ "III",
      stage == "Stage IV" ~ "IV",
      TRUE ~ NA_character_
    ),
    
    pam50_clean = pam50
  )


#change order of groups
plot_data$stage_clean <- factor(
  plot_data$stage_clean,
  levels = c("I", "II", "III", "IV")
)

plot_data$age_group <- factor(
  plot_data$age_group,
  levels = c("<60", ">=60")
)

plot_data$pam50_clean <- factor(
  plot_data$pam50_clean,
  levels = c("Normal", "LumB", "LumA", "Her2", "Basal")
)


saveRDS(plot_data, file = "DDIT3_Figure1_plot_data.rds") #Save
list.files() 

#remove large files
rm(brca)
rm(expr)
rm(expr_df)
rm(expr_long)
rm(gene_info)
rm(clinical)
gc()