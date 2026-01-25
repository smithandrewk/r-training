# Pertussis SCION Report (1 year back, 5 years back)
library(readxl)
library(dplyr)

df <- read_excel("pertussis_5year.xlsx")
df$CREATE_DATE <- as.Date(df$CREATE_DATE,format="%m/%d/%Y")
df <- df %>% select(CREATE_DATE,AGE,DISEASE_STATUS)
df <- df %>% mutate(CASE_COUNT = 1)
# table(df$DISEASE_STATUS)
df <- df[df$DISEASE_STATUS != "Not a case",]
# Resampling
case_df <- df %>% 
  group_by(month=floor_date(CREATE_DATE,'month')) %>%
  summarize(CASE_COUNT = sum(CASE_COUNT))

library(ggplot2)
ggplot(case_df, aes(x = month, y = CASE_COUNT)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Pertussis Cases",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(lubridate)
cases_ts <- ts(case_df$CASE_COUNT, start = c(year(min(case_df$month)), month(min(case_df$month))), frequency = 12)

