# Pertussis SCION Report (1 year back, 5 years back)
library(readxl)
library(dplyr)
library(tsibble)

df <- read_excel("pertussis_5year.xlsx")
df$CREATE_DATE <- as.Date(df$CREATE_DATE,format="%m/%d/%Y")
df <- df %>% select(CREATE_DATE,AGE,DISEASE_STATUS)
df <- df %>% filter(DISEASE_STATUS != "Not a case")
df <- df %>% mutate(CASE_COUNT = 1)
df <- df %>% mutate(epiweek=yearweek(df$CREATE_DATE))
df <- df %>% group_by(epiweek) %>% summarize(case=sum(CASE_COUNT))
counts <- tsibble(df,index=epiweek)

library(ggplot2)
ggplot(counts, aes(x = epiweek, y = case)) + 
  geom_line()

counts <- counts %>% fill_gaps(case = 0)

library(feasts)
library(tsibble)
library(ggplot2)
library(dplyr)
library(lubridate)

# Decompose the time series
decomp <- counts %>% model(classical_decomposition(case, type = "multiplicative")) %>% components()

# Add numeric index for modeling
decomp <- decomp %>% mutate(numeric_index = as.numeric(row_number()))
trend_model <- lm(trend ~ numeric_index, data = decomp)
future_index <- max(decomp$numeric_index) + seq_len(10)
future_trend <- predict(trend_model, newdata = data.frame(numeric_index = future_index), se.fit = TRUE)
future_trend_values <- future_trend$fit
future_trend_se <- future_trend$se.fit
seasonality <- decomp$seasonal
future_seasonality <- rep(seasonality, length.out = 10)

# Combine trend and seasonal components
forecast_cases <- future_trend_values * future_seasonality

# Calculate residual variance (from remainder component)
residual_sd <- sd(decomp$random, na.rm = TRUE)

# Adjust confidence intervals to include residual variance
ci_upper <- (future_trend_values + 1.96 * sqrt(future_trend_se^2 + residual_sd^2)) * future_seasonality
ci_lower <- (future_trend_values - 1.96 * sqrt(future_trend_se^2 + residual_sd^2)) * future_seasonality

# Prepare the forecast data frame
forecast_df <- tibble(
  epiweek = seq(max(counts$epiweek) + 1, length.out = 10, by = 1),
  case = forecast_cases,
  lower = ci_lower,
  upper = ci_upper
)

case_df <- counts %>%
  filter(year(epiweek) > 2022) %>%
  select(epiweek, case) %>%
  bind_rows(forecast_df %>% select(epiweek, case, lower, upper))

# Plot observed and forecasted case counts with confidence intervals
ggplot() +
  geom_line(data = case_df, aes(x = epiweek, y = case), color = "black", size = 1) +
  geom_line(data = forecast_df, aes(x = epiweek, y = case), color = "red", size = 1) +
  geom_ribbon(data = forecast_df, aes(x = epiweek, ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
  labs(
    title = "Year-to-Date Pertussis Cases with Forecast and Confidence Interval",
    x = "Epidemiological Week",
    y = "Case Count"
  ) +
  theme_minimal()
