
context("test ttest power analysis")
test_that("ttest power analysis density plot data generation", {
  library(lavaan)
  # Load the HolzingerSwineford1939 dataset
  data(HolzingerSwineford1939)
  
  # Specify the CFA model
  model_desc <- '
    # Factor 1: Visual
    visual  =~ x1 + x2 + x3
  
    # Factor 2: Verbal
    verbal  =~ x4 + x5 + x6
  
    # Factor 3: Quantitative
    quantitative  =~ x7 + x8 + x9
  '
  
  model_df <- HolzingerSwineford1939 %>% exp_sem(model_desc)
  
  res <- model_df %>% tidy_rowwise(model, type="summary")
  res <- model_df %>% tidy_rowwise(model, type="loadings")
})