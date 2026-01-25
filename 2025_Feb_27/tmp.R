# Load necessary libraries
library(rpart)        # For building decision trees
library(rpart.plot)   # For visualizing decision trees
library(caret)        # For data splitting and evaluation
library(ggplot2)      # For plotting the heatmap and decision boundary
library(GGally)       # For creating the pairplot

# Step 1: Simulate a dataset for heart disease prediction
set.seed(123)  # For reproducibility
n <- 200  # Number of samples
age <- runif(n, 30, 80)  # Age between 30 and 80
cholesterol <- runif(n, 150, 300)  # Cholesterol between 150 and 300
blood_pressure <- runif(n, 100, 180)  # Blood pressure between 100 and 180

# Create binary outcome: heart_disease (1 = yes, 0 = no) with some noise
heart_disease <- ifelse(age > 50 & cholesterol > 200 & blood_pressure > 140, 1, 0)
noise <- rbinom(n, 1, 0.1) * sample(c(-1, 1), n, replace = TRUE)
heart_disease <- pmax(pmin(heart_disease + noise, 1), 0)  # Keep between 0 and 1

# Combine into a data frame
data <- data.frame(age, cholesterol, blood_pressure, heart_disease = as.factor(heart_disease))

# Step 2: Create a pairplot of the features
# Shows scatterplots and histograms for all pairs of features, colored by heart_disease
ggpairs(data, columns = 1:3, aes(color = heart_disease), 
        title = "Pairplot of Features by Heart Disease Status")

# Step 3: Split the data into training (70%) and testing (30%) sets
set.seed(123)
trainIndex <- createDataPartition(data$heart_disease, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Step 4: Build a decision tree model
tree_model <- rpart(heart_disease ~ ., data = train_data, method = "class")

# Visualize the tree (optional, for understanding the model)
rpart.plot(tree_model, main = "Decision Tree for Heart Disease Prediction")

# Step 5: Make predictions on the test set
predictions <- predict(tree_model, test_data, type = "class")

# Step 6: Create and plot the confusion matrix as a heatmap
conf_matrix <- confusionMatrix(predictions, test_data$heart_disease)
conf_df <- as.data.frame(conf_matrix$table)  # Convert to data frame for plotting

# Plot the confusion matrix as a heatmap
ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +  # Create the heatmap tiles
  geom_text(aes(label = Freq), color = "black") +  # Add frequency labels
  scale_fill_gradient(low = "white", high = "blue") +  # Color gradient
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()  # Clean theme

# Step 7: Plot the decision boundary and classification space for age vs cholesterol
# Fix blood_pressure to its median value
median_bp <- median(train_data$blood_pressure)

# Create a grid of age and cholesterol values
age_seq <- seq(min(data$age), max(data$age), length.out = 100)
chol_seq <- seq(min(data$cholesterol), max(data$cholesterol), length.out = 100)
grid <- expand.grid(age = age_seq, cholesterol = chol_seq)
grid$blood_pressure <- median_bp  # Fix third feature

# Predict class for each point in the grid
grid$pred <- predict(tree_model, grid, type = "class")

# Plot decision boundary with training data overlaid
ggplot() +
  geom_tile(data = grid, aes(x = age, y = cholesterol, fill = pred), alpha = 0.5) +  # Decision regions
  geom_point(data = train_data, aes(x = age, y = cholesterol, color = heart_disease), size = 2) +  # Data points
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"), name = "Predicted") +  # Fill colors
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Actual") +  # Point colors
  labs(title = "Decision Boundary: Age vs Cholesterol",
       subtitle = paste("Blood Pressure fixed at", round(median_bp, 2)),
       x = "Age", y = "Cholesterol") +
  theme_minimal()

# Optional: Repeat for other pairs (e.g., age vs blood_pressure or cholesterol vs blood_pressure)
# Example for age vs blood_pressure (fixing cholesterol)
median_chol <- median(train_data$cholesterol)
grid2 <- expand.grid(age = age_seq, blood_pressure = seq(min(data$blood_pressure), max(data$blood_pressure), length.out = 100))
grid2$cholesterol <- median_chol
grid2$pred <- predict(tree_model, grid2, type = "class")

ggplot() +
  geom_tile(data = grid2, aes(x = age, y = blood_pressure, fill = pred), alpha = 0.5) +
  geom_point(data = train_data, aes(x = age, y = blood_pressure, color = heart_disease), size = 2) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightcoral"), name = "Predicted") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Actual") +
  labs(title = "Decision Boundary: Age vs Blood Pressure",
       subtitle = paste("Cholesterol fixed at", round(median_chol, 2)),
       x = "Age", y = "Blood Pressure") +
  theme_minimal()
