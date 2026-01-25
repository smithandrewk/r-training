# Load necessary libraries
library(tidyverse)
library(lubridate)

# Step 1: Load the CSV file
linelist <- read.csv("Pertussis_Line_List.csv")  # Replace with your actual file path

# Step 2: Convert the 'datetime' column to Date format
linelist$datetime <- as.Date(linelist$datetime)

# Step 3: Resample the data to monthly totals
linelist_monthly <- linelist %>%
  mutate(month = floor_date(datetime, "month")) %>%  # Floor to the start of each month
  group_by(month) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

# Step 4: Plot the monthly cases
ggplot(linelist_monthly, aes(x = month, y = cases)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Pertussis Cases",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

# Load necessary libraries
library(forecast)
library(ggplot2)

# Assuming 'linelist_monthly' is already set up with 'month' as the date column and 'cases' as the target column
# Convert the data to a time series object with a monthly frequency
cases_ts <- ts(linelist_monthly$cases, start = c(year(min(linelist_monthly$month)), month(min(linelist_monthly$month))), frequency = 12)

# Perform seasonal decomposition using STL
decomposition <- stl(cases_ts, s.window = "periodic")  # 'periodic' automatically detects seasonality

# Plot the decomposition
autoplot(decomposition) +
  ggtitle("Seasonal Decomposition of Monthly Pertussis Cases") +
  xlab("Year") + ylab("Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

# Load necessary libraries
library(forecast)
library(tidyverse)

# Assuming 'cases_ts' is the monthly time series object created from 'linelist_monthly'
# Step 1: Perform seasonal decomposition
decomposition <- stl(cases_ts, s.window = "periodic")
trend <- na.omit(decomposition$time.series[, "trend"])  # Remove NAs from trend
seasonal <- decomposition$time.series[1:12, "seasonal"]  # Get one year of seasonality (assuming monthly data)

# Step 2: Fit a linear regression model to the trend component
X <- 1:length(trend)  # Time as predictor
trend_model <- lm(trend ~ X)

# Project the trend into the future (e.g., for 12 additional months)
future_days <- 12
future_X <- (length(trend) + 1):(length(trend) + future_days)
future_trend <- predict(trend_model, newdata = data.frame(X = future_X))

# Step 3: Extend the seasonality into the future
future_seasonal <- rep(seasonal, length.out = future_days)

# Step 4: Combine future trend and seasonal components to get the forecast
future_prediction <- future_trend + future_seasonal

# Step 5: Create a data frame for plotting or further analysis
future_dates <- seq(from = as.Date(paste0(year(max(linelist_monthly$month)), "-", month(max(linelist_monthly$month)), "-01")),
                    by = "month", length.out = future_days)
forecast_df <- data.frame(date = future_dates, cases = future_prediction)

# Print or plot the forecast
print(forecast_df)

# Load necessary library for plotting
library(ggplot2)

# Combine observed data and forecast into a single data frame for plotting
plot_data <- linelist_monthly %>%
  rename(date = month) %>%
  select(date, cases) %>%
  bind_rows(forecast_df)

# Plot the observed and forecasted cases
ggplot(plot_data, aes(x = date, y = cases)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +         # Observed data
  geom_line(data = forecast_df, aes(x = date, y = cases),     # Forecasted data
            color = "orange", size = 1, linetype = "dashed") +
  labs(title = "Pertussis Case Forecast",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Load necessary libraries
library(ggplot2)

# Calculate the standard deviation of the residuals
residuals <- decomposition$time.series[, "remainder"]
residual_std <- sd(residuals, na.rm = TRUE)  # Standard deviation of the residuals
confidence_interval <- 1.96 * residual_std  # 95% confidence interval

# Calculate the upper and lower bounds
upper_bound <- future_prediction + confidence_interval
lower_bound <- future_prediction - confidence_interval

# Combine forecast and confidence intervals into a data frame
forecast_df <- data.frame(
  date = future_dates,
  cases = future_prediction,
  upper_bound = upper_bound,
  lower_bound = lower_bound
)

# Combine observed data and forecast data for plotting
plot_data <- linelist_monthly %>%
  rename(date = month) %>%
  select(date, cases) %>%
  bind_rows(forecast_df)

# Plot the observed cases, forecast, and confidence intervals
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = cases), color = "blue", size = 1, na.rm = TRUE) +  # Observed data
  geom_line(data = forecast_df, aes(y = cases), color = "orange", size = 1, linetype = "dashed") +  # Forecast
  geom_ribbon(data = forecast_df, aes(ymin = lower_bound, ymax = upper_bound), 
              fill = "orange", alpha = 0.2) +  # Confidence interval
  labs(title = "Pertussis Case Forecast with 95% Confidence Interval",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
