library(tidyverse)
library(RCurl)
library(class)
library(caret)
library(ggplot2)

# Importing data from repository
url1 <- "https://raw.githubusercontent.com/shoun-lo/asrm-432-fp/main/train.txt"
train_data <- read_delim(url1, delim = "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# Split the training data
set.seed(7)  # Ensure reproducibility
train_ind <- sample(seq_len(nrow(train_data)), size = 4000)
train <- train_data[train_ind, ]
test <- train_data[-train_ind, ]

# Normalize the data since KNN uses distance metrics.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the training and testing datasets
train_normalized <- as.data.frame(lapply(train[, -86], normalize))
test_normalized <- as.data.frame(lapply(test[, -86], normalize))

# Split the data into features and target variable
train_features <- train_normalized
train_target <- as.factor(train[[86]])  # Convert target variable to factor

# Set up cross-validation for k optimization
control <- trainControl(method = "cv", number = 10)

# Define the range of k values to test
grid <- expand.grid(.k = 1:10)  # Testing k from 1 to 10

# Train the model with different values of k and select the best k
knn_train <- train(x = train_features, y = train_target,
                   method = "knn", tuneGrid = grid,
                   trControl = control)

# Print the best k value
best_k <- knn_train$bestTune$k
cat("Best k:", best_k, "\n")

# Fit the KNN model with the best k to the training data
knn_model <- knn(train = train_features, test = test_normalized,
                 cl = train_target, k = best_k)

# Evaluate the model by comparing its predictions to actual outcomes from test
test_actual <- as.factor(test[[86]])  # Actual target values for the test set
conf_matrix <- table(predicted = knn_model, actual = test_actual)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate sensitivity and specificity
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")

# Plot accuracy versus different K values
accuracy_plot <- ggplot(knn_train$results,
                        aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point(shape = 21, fill = "blue") +
  labs(title = "KNN Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Cross-Validated Accuracy") +
  theme_minimal()

print(accuracy_plot)
