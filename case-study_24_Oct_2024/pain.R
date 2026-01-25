
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(randomForest)
library(caTools)
library(mlbench)
# Load dataset
data("PimaIndiansDiabetes2")
df <- PimaIndiansDiabetes2
# View the dataset structure
str(df)

# Check for missing values
colSums(is.na(df))

# View the summary of the dataset
summary(df)

# Convert the 'diabetes' column to a binary factor (0 for negative, 1 for positive)
df$diabetes <- ifelse(df$diabetes == "pos", 1, 0)

# Alternatively, convert it to factors (Yes/No or 0/1)
#df$diabetes <- factor(df$diabetes, levels = c(0, 1), labels = c("No", "Yes"))

# Ensure the conversion is correct
table(df$diabetes)
# KDE plot equivalent: Distribution of pregnancies by diabetes status
ggplot(df, aes(x = pregnant, color = diabetes)) +
  geom_histogram() +
  labs(title = "KDE plot of Pregnancies by Diabetes Status")

# Pair plot equivalent
pairs(df[, -9], col = df$diabetes)

# Boxplots for different features by diabetes status
ggplot(df, aes(x = diabetes, y = glucose, fill = diabetes)) +
  geom_boxplot() +
  labs(title = "Boxplot of Glucose by Diabetes Status")

ggplot(df, aes(x = diabetes, y = mass, fill = diabetes)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mass by Diabetes Status")

ggplot(df, aes(x = diabetes, y = age, fill = diabetes)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Diabetes Status")

# Impute missing values with median
df <- df %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Verify that there are no missing values left
colSums(is.na(df))


# Set seed for reproducibility
set.seed(42)

# Split data into train (80%) and test (20%)
split <- sample.split(df$diabetes, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Features and target variable for training
X_train <- train_data[, -9]
y_train <- train_data$diabetes

X_test <- test_data[, -9]
y_test <- test_data$diabetes

# Fit logistic regression model
log_model <- glm(diabetes ~ ., data = train_data, family = binomial)

# Predict on the test set
pred_prob <- predict(log_model, newdata = X_test, type = "response")
y_pred <- ifelse(pred_prob > 0.5, 1, 0)

# Make sure that both y_pred and y_test are factors with the same levels
y_test <- factor(y_test, levels = c(0, 1))
y_pred <- factor(y_pred, levels = c(0, 1))

# Confusion matrix and performance metrics
confusionMatrix(y_pred, y_test)

# ROC Curve and AUC
library(pROC)
# Extract confusion matrix as a table
cm_table <- as.table(conf_mat$table)

# Convert confusion matrix table to a data frame
cm_df <- as.data.frame(cm_table)

# View the data frame to ensure it’s in the right format
print(cm_df)

# Plot confusion matrix using ggplot2
ggplot(cm_df, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()
roc_curve <- roc(as.factor(y_test), as.numeric(pred_prob))
plot(roc_curve, main = "ROC Curve for Logistic Regression")
auc(roc_curve)