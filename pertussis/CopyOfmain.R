# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)

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

# Step 5: Plot the weekly cases
library(ggplot2)
ggplot(linelist, aes(x = week_start, y = cases)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Pertussis Cases",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

# Step 6: Convert to a weekly time series object for forecasting
cases_ts <- ts(linelist$cases, start = c(year(min(linelist$week_start)), week(min(linelist$week_start))), frequency = 52)

# Step 7: Perform seasonal decomposition using STL
decomposition <- stl(cases_ts, s.window = "periodic")

# Plot the decomposition
autoplot(decomposition) +
  ggtitle("Seasonal Decomposition of Weekly Pertussis Cases") +
  xlab("Year") + ylab("Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))

# Step 8: Forecast with confidence intervals
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

# Create forecast dates
future_dates <- seq(from = max(linelist$week_start) + 7, by = "week", length.out = future_weeks)
forecast_df <- data.frame(date = future_dates, cases = future_prediction)

# Plot observed data and forecast
plot_data <- linelist %>%
  rename(date = week_start) %>%
  select(date, cases) %>%
  bind_rows(forecast_df)

# Plot the observed and forecasted cases
ggplot(plot_data, aes(x = date, y = cases)) +
  geom_line(color = "blue", size = 1, na.rm = TRUE) +         # Observed data
  geom_line(data = forecast_df, aes(x = date, y = cases),     # Forecasted data
            color = "orange", size = 1, linetype = "dashed") +
  labs(title = "Pertussis Case Forecast (Weekly)",
       x = "Date",
       y = "Cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")