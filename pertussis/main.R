# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Step 1: Read the Excel file
df <- read_excel("PertussisExtract_20241107084342.xlsx")

# Step 2: Add a 'CASE_COUNT' column with a value of 1 for each row
df <- df %>%
  mutate(CASE_COUNT = 1) %>%
  select(CREATE_DATE, CASE_COUNT)

# Step 3: Convert 'CREATE_DATE' to Date format
df <- df %>%
  mutate(CREATE_DATE = as.Date(CREATE_DATE, format = "%m/%d/%Y"))

# Step 4: Resample to weekly totals
linelist <- df %>%
  group_by(week_start = floor_date(CREATE_DATE, "week")) %>%
  summarize(cases = sum(CASE_COUNT))

# Step 5: Convert to a weekly time series object for forecasting
cases_ts <- ts(linelist$cases, start = c(year(min(linelist$week_start)), week(min(linelist$week_start))), frequency = 52)

# Step 6: Perform seasonal decomposition using STL
decomposition <- stl(cases_ts, s.window = "periodic")

# Plot the decomposition
autoplot(decomposition) +
  ggtitle("Seasonal Decomposition of Weekly Pertussis Cases") +
  xlab("Year") + ylab("Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

# Step 7: Forecast with confidence intervals
trend <- na.omit(decomposition$time.series[, "trend"])
seasonal <- decomposition$time.series[1:52, "seasonal"]

# Fit a linear model to the trend component
X <- 1:length(trend)
trend_model <- lm(trend ~ X)

# Project the trend into the future (e.g., for 52 additional weeks)
future_weeks <- 52
future_X <- (length(trend) + 1):(length(trend) + future_weeks)
future_trend <- predict(trend_model, newdata = data.frame(X = future_X))
future_seasonal <- rep(seasonal, length.out = future_weeks)

# Combine trend and seasonal components
future_prediction <- future_trend + future_seasonal

# Step 8: Calculate residuals and confidence intervals
residuals <- decomposition$time.series[, "remainder"]
residual_std <- sd(residuals, na.rm = TRUE)  # Standard deviation of residuals
confidence_interval <- 1.96 * residual_std  # 95% confidence interval

# Calculate upper and lower bounds for the forecast
upper_bound <- future_prediction + confidence_interval
lower_bound <- future_prediction - confidence_interval

# Create forecast dates
future_dates <- seq(from = max(linelist$week_start) + 7, by = "week", length.out = future_weeks)
forecast_df <- data.frame(date = future_dates, cases = future_prediction, 
                          upper_bound = upper_bound, lower_bound = lower_bound)

# Combine observed data and forecast data for plotting
plot_data <- linelist %>%
  rename(date = week_start) %>%
  select(date, cases) %>%
  bind_rows(forecast_df)

# Plot the observed cases, forecast, and confidence intervals
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue", size = 1, na.rm = TRUE) +  # Observed data
  geom_line(data = forecast_df, aes(y = cases), color = "orange", size = 1, linetype = "dashed") +  # Forecast
  geom_ribbon(data = forecast_df, aes(ymin = lower_bound, ymax = upper_bound), 
              fill = "orange", alpha = 0.2) +  # Confidence interval
  labs(title = "Pertussis Case Forecast with 95% Confidence Interval (Weekly)",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


# Filter data to include only 2024 and 2025
plot_data_filtered <- plot_data %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2025-12-31"))

# Plot the observed cases, forecast, and confidence intervals for 2024 and 2025 only
ggplot(plot_data_filtered, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue", size = 1, na.rm = TRUE) +  # Observed data
  geom_line(data = forecast_df, aes(x = date, y = cases), color = "orange", size = 1, linetype = "dashed") +  # Forecast
  geom_ribbon(data = forecast_df, aes(ymin = lower_bound, ymax = upper_bound), 
              fill = "orange", alpha = 0.2) +  # Confidence interval
  labs(title = "Pertussis Case Forecast with 95% Confidence Interval (2024-2025)",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

