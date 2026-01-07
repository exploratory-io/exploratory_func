# Test script to reproduce the prediction_binary error with SMOTE and test mode
# This script tests the fix for the issue where prediction_binary fails
# when SMOTE is applied with test mode on

library(dplyr)

# Create test data with imbalanced target
set.seed(123)
n <- 1029  # Use the exact number from the user's error
test_data <- data.frame(
  id = 1:n,
  feature1 = rnorm(n),
  feature2 = rnorm(n),
  feature3 = sample(c("A", "B", "C"), n, replace = TRUE),
  # Create imbalanced target: ~10% minority class
  target = c(rep(FALSE, floor(n * 0.9)), rep(TRUE, ceiling(n * 0.1)))
)

cat("Original data size:", nrow(test_data), "\n")
cat("Target distribution:", table(test_data$target), "\n\n")

# Test with build_lm.fast (GLM)
cat("Testing build_lm.fast with SMOTE and test mode...\n")
tryCatch({
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.3,
      smote = TRUE,
      seed = 123
    )
  
  cat("Model created successfully!\n")
  cat("Source data size:", nrow(model_df$source.data[[1]]), "\n")
  
  # This is where the error occurred for the user
  cat("Testing prediction_binary...\n")
  predictions <- model_df %>% 
    prediction_binary(data = 'training_and_test', threshold = 0.5, pretty.name = TRUE)
  
  cat("SUCCESS: prediction_binary works! Predictions created with", nrow(predictions), "rows\n")
  cat("Training rows:", sum(predictions$`Data Type` == "Training"), "\n")
  cat("Test rows:", sum(predictions$`Data Type` == "Test"), "\n")
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  stop(e)
})

cat("\n\nTesting exp_xgboost with SMOTE and test mode...\n")
tryCatch({
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.3,
      smote = TRUE,
      nrounds = 10,
      seed = 123
    )
  
  cat("Model created successfully!\n")
  
  # Test prediction
  cat("Testing prediction...\n")
  predictions <- model_df %>% 
    prediction(data = 'training_and_test')
  
  cat("SUCCESS: prediction works! Predictions created with", nrow(predictions), "rows\n")
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  stop(e)
})

cat("\nAll tests passed successfully!\n")

