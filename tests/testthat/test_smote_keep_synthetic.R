# Test script to verify smote_keep_synthetic parameter works correctly
# This script tests that when smote_keep_synthetic = TRUE, the output includes
# SMOTE-enhanced data with synthetic samples marked by the 'synthesized' column

library(dplyr)

cat("Testing smote_keep_synthetic parameter...\n\n")

# Create test data with imbalanced target
set.seed(123)
n <- 500
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

# Test 1: GLM with smote_keep_synthetic = TRUE (default)
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("Test 1: build_lm.fast (GLM) with smote_keep_synthetic = TRUE (default)\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
tryCatch({
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.3,
      smote = TRUE,
      # smote_keep_synthetic = TRUE by default
      seed = 123
    )
  
  source_data <- model_df$source.data[[1]]
  cat("✓ Model created successfully!\n")
  cat("✓ Source data size:", nrow(source_data), "(should be > ", n, ")\n")
  
  # Check for synthesized column
  if ("synthesized" %in% colnames(source_data)) {
    cat("✓ 'synthesized' column present\n")
    cat("✓ Synthetic samples:", sum(source_data$synthesized), "\n")
    cat("✓ Real samples:", sum(!source_data$synthesized), "\n")
    
    # Verify test data is not synthetic
    test_index <- model_df$.test_index[[1]]
    if (all(!source_data$synthesized[test_index])) {
      cat("✓ Test data contains NO synthetic samples (correct!)\n")
    } else {
      cat("✗ ERROR: Test data contains synthetic samples\n")
    }
  } else {
    cat("✗ ERROR: 'synthesized' column NOT present\n")
  }
  
  # Test prediction
  cat("\n✓ Testing prediction_binary...\n")
  predictions <- model_df %>% 
    prediction_binary(data = 'training_and_test', threshold = 0.5, pretty.name = TRUE)
  
  cat("✓ Predictions created with", nrow(predictions), "rows\n")
  cat("✓ Training rows:", sum(predictions$`Data Type` == "Training"), "\n")
  cat("✓ Test rows:", sum(predictions$`Data Type` == "Test"), "\n")
  
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
})

# Test 2: GLM with smote_keep_synthetic = FALSE
cat("\n", "=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("Test 2: build_lm.fast (GLM) with smote_keep_synthetic = FALSE\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
tryCatch({
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.3,
      smote = TRUE,
      smote_keep_synthetic = FALSE,
      seed = 123
    )
  
  source_data <- model_df$source.data[[1]]
  cat("✓ Model created successfully!\n")
  cat("✓ Source data size:", nrow(source_data), "(should be =", n, ")\n")
  
  # Check that synthesized column is NOT present
  if ("synthesized" %in% colnames(source_data)) {
    cat("✗ ERROR: 'synthesized' column should NOT be present when smote_keep_synthetic = FALSE\n")
  } else {
    cat("✓ 'synthesized' column NOT present (correct!)\n")
  }
  
  # Test prediction
  cat("\n✓ Testing prediction_binary...\n")
  predictions <- model_df %>% 
    prediction_binary(data = 'training_and_test', threshold = 0.5, pretty.name = TRUE)
  
  cat("✓ Predictions created with", nrow(predictions), "rows\n")
  
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
})

# Test 3: XGBoost with smote_keep_synthetic = TRUE (default)
cat("\n", "=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("Test 3: exp_xgboost with smote_keep_synthetic = TRUE (default)\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
tryCatch({
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.3,
      smote = TRUE,
      # smote_keep_synthetic = TRUE by default
      nrounds = 5,
      seed = 123
    )
  
  source_data <- model_df$source.data[[1]]
  cat("✓ Model created successfully!\n")
  cat("✓ Source data size:", nrow(source_data), "\n")
  
  if ("synthesized" %in% colnames(source_data)) {
    cat("✓ 'synthesized' column present\n")
    cat("✓ Synthetic samples:", sum(source_data$synthesized), "\n")
  } else {
    cat("✗ ERROR: 'synthesized' column NOT present\n")
  }
  
  # Test prediction
  predictions <- model_df %>% prediction(data = 'training_and_test')
  cat("✓ Predictions created with", nrow(predictions), "rows\n")
  
}, error = function(e) {
  cat("✗ ERROR:", e$message, "\n")
})

cat("\n", "=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("All tests completed!\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")

