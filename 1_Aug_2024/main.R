# R Lesson for August 1st, 2024
library(readxl)
linelist <- read_excel("/Users/andrew/r-training/1_Aug_2024/linelist_cleaned.xlsx")
View(linelist)

summary(linelist)

summary(linelist$age)

report <- summary(linelist$age)

report["Max."]

report[[6]]

pacman::p_load(skimr)

skim(linelist)

pacman::p_load(rstatix)

report <- linelist %>% get_summary_stats(type="common")
report[c("variable","n","min","max","mean","sd")]

library(janitor)
frequency_table <- tabyl(linelist$age,show_na=FALSE)
frequency_table['Cumulative Frequency'] <- cumsum(frequency_table$n)
frequency_table['Cumulative Percent'] <- cumsum(frequency_table$percent)

cross_tab <- linelist %>% tabyl(age,gender)

linelist %>% tabyl(age,gender,hospital)
