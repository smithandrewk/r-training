# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
file_path <- "data.xlsx"
df <- read_excel(file_path, sheet = "2024Cleaned_Schools & Childcare")

# View the first few rows
head(df)

# Convert interaction dates to Date type
df <- df %>%
  mutate(`Interaction Date` = as.Date(`Interaction Date`))

# Filter for site visits only
df_visits <- df %>%
  filter(`Method of Communication` == "Site Visit")

# Check the number of site visits per month
df_visits %>%
  count(month = format(`Interaction Date`, "%Y-%m"))

# Aggregate data by month
df_visits_monthly <- df_visits %>%
  group_by(month = format(`Interaction Date`, "%Y-%m")) %>%
  summarise(visits = n())

# Convert month to Date format for plotting
df_visits_monthly$month <- as.Date(paste0(df_visits_monthly$month, "-01"))

ggplot(df_visits_monthly, aes(x = month, y = visits)) +
  geom_line() +  # Connect points with a line
  geom_point() +  # Show data points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add regression line
  labs(title = "Site Visits Over Time with Linear Trend",
       x = "Month", y = "Number of Visits") +
  theme_minimal()

# Fit a linear regression model
model <- lm(visits ~ as.numeric(month), data = df_visits_monthly)
summary(model)

df_visits_type <- df_visits %>%
  group_by(month = format(`Interaction Date`, "%Y-%m"), `Initiation of Interaction`) %>%
  summarise(visits = n())

df_visits_type$month <- as.Date(paste0(df_visits_type$month, "-01"))

ggplot(df_visits_type, aes(x = month, y = visits, color = `Initiation of Interaction`)) +
  geom_line() +
  geom_point() +
  labs(title = "Proactive vs. Reactive Site Visits", x = "Month", y = "Number of Visits") +
  theme_minimal()


# Ensure 'Initiation of Interaction' is treated as a factor
df_visits_type <- df_visits %>%
  group_by(month = format(`Interaction Date`, "%Y-%m"), `Initiation of Interaction`) %>%
  summarise(visits = n(), .groups = "drop")

# Convert month to Date format
df_visits_type$month <- as.Date(paste0(df_visits_type$month, "-01"))

# Create the plot
ggplot(df_visits_type, aes(x = month, y = visits, color = factor(`Initiation of Interaction`))) +
  geom_line() +
  geom_point() +
  labs(title = "Proactive vs. Reactive Site Visits", x = "Month", y = "Number of Visits", color = "Interaction Type") +
  theme_minimal()