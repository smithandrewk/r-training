# Load the dataset
df <- read.csv('./nicer_epidemiology_data.csv')

# Convert categorical variables to factors
df$fever <- as.factor(df$fever)
df$cough <- as.factor(df$cough)
df$exposure <- as.factor(df$exposure)
df$outcome <- as.factor(df$outcome)

# Fit the model
model <- glm(outcome ~ age + fever, data = df, family = binomial)

# View the summary of the model
summary(model)
# Make predictions on the dataset
predictions <- predict(model, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Infected", "Not Infected")

# Compare predictions with actual outcomes
table(predicted_classes, df$outcome)

# Calculate accuracy
accuracy <- mean(predicted_classes == df$outcome)
paste("Accuracy:", round(accuracy, 3))

predicted_classes
df$outcome
