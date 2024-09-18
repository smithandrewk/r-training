library(tidyverse)

data(mtcars)
# dplyr
filtered_data <- mtcars %>% filter(mpg > 20)

mutated_data <- filtered_data %>% mutate(kilometers_per_gallon=mpg*1.61)

selected_data <- mutated_data %>% select(mpg,cyl,kilometers_per_gallon)

summary_data <- selected_data %>% group_by(cyl) %>% summarize(avg_kilometers_per_gallon=mean(kilometers_per_gallon))


# ggplot
ggplot(mtcars, aes(x=wt,y=mpg)) + geom_point() + geom_line()

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "Scatter plot of MPG vs. Weight", x = "Weight (1000 lbs)", y = "Miles per Gallon")

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "MPG vs. Weight with Cylinders as Color", color = "Cylinders")

wide_data <- data.frame(
  id = c("group1_subject1", "group2_subject2", "group3_subject3"),
  height = c(150, 160, 170),
  weight = c(60, 70, 80)
)

long_data <- wide_data %>%
  gather(key = "measurement", value = "value", height, weight)

separated_data <- wide_data %>%
  separate(id, into = c("group", "subject"), sep = "_")

united_data <- separated_data %>%
  unite("group_subject", group, subject, sep = "-")

write_csv(united_data,'out.csv')

df <- read_csv('out.csv')


numbers <- list(1, 2, 3, 4, 5)
map(numbers, ~ .x^2)


