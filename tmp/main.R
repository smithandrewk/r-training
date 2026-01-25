print("hello world")
args <- commandArgs(trailingOnly = TRUE)
name <- args[1]

cat("Name:", name, "\n")

# Example datasets
library(dplyr)
cases <- data.frame(region = c("A", "B", "C","D"), cases = c(50, 100, 200, 300))
population <- data.frame(region = c("A", "B", "C"), population = c(1000, 2000, 3000))

# Merge tables
merged_data <- cases %>%
  right_join(population, by = "region") %>%
  mutate(attack_rate = cases / population * 100)
print(merged_data)