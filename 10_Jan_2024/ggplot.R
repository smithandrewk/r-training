# Install ggplot2 package (if not already installed)
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)
# Basic scatter plot
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()
mpg
# Scatter plot with customizations
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Engine Displacement vs. Highway MPG",
       x = "Engine Displacement (liters)",
       y = "Highway Miles per Gallon") +
  theme_minimal()
# Bar plot
ggplot(data = mpg, aes(x = class)) +
  geom_bar()

# Histogram
ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 1)

# Boxplot
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot()
# Adding a smooth line to the scatter plot
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Faceted scatter plot
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class)

ggplot(data = mpg, aes(x = displ, y = hwy, color = class, size = cyl)) +
  geom_point(alpha = 0.7) +
  labs(title = "Displacement vs. Highway MPG by Car Class and Cylinder Count",
       x = "Engine Displacement (liters)",
       y = "Highway Miles per Gallon") +
  theme_light()

# Assuming 'timeseries_data' is a dataset with 'date' and 'value' columns
ggplot(data = timeseries_data, aes(x = date, y = value)) +
  geom_line(color = "darkblue") +
  labs(title = "Time Series Example",
       x = "Date",
       y = "Value") +
  theme_classic()



