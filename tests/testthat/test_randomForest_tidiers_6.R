context("Test cases from past issues")

test_that("Random Forest: Group with fewer samples should not be skipped in the result.", {

  # Steps to produce the output
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/zm1p1o0s87spn3t/Organic_Store_Customers.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
    readr::type_convert() %>%
    exploratory::clean_data_frame() %>%
    filter(ChurnedWithinYear != "Unknown") %>%
    mutate(ChurnedWithinYear = str_logical(ChurnedWithinYear))
  model_df <- df %>% dplyr::group_by(`Store`) %>% calc_feature_imp(`ChurnedWithinYear`, `Region`, `Gender`, `Age`, `JobField`, `MaritalStatus`, `ContactType`, `MemberCard`, `MonthlySpending`, `BeerTastingClass`, `CookingClass`, `FlowerArrangementClass`, `PaperCraftClass`, `WineClass`, `DistanceFromStore`, `NumberOfStoresVisited`, `StoreVisitsPerMonth`, `NewsLetterOpened`, `WebSiteEngagement`, `SurveyOverallRating`, `FacebookPageLikes`, `EnvironmentCareness`, smote = TRUE, importance_measure = "permutation", with_boruta = TRUE)
  res <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(res), 9) # There used to be an issue that Store with fewer samples (after SMOTE) was skipped in the result due to edarf error. #14070
})
