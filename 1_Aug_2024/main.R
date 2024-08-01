pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)

library(readxl)
linelist <- read_excel("r-training/1_Aug_2024/linelist_cleaned.xlsx")
View(linelist)

skim(linelist)

summary(linelist)

linelist %>% 
  get_summary_stats(
    age, wt_kg, ht_cm, ct_blood, temp,  # columns to calculate for
    type = "common")                    # summary stats to return

linelist %>% tabyl(age_cat)
