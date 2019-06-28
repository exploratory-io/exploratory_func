# how to run this test:
# devtools::test(filter="kmeans_2")

context("test kmeans analytics view functions with California Election data.")

test_that("exp_kmeans result is properly separated out.", {
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/5dhy5hcq1b8zla4/CA_Election_spreaded.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
    readr::type_convert() %>%
    exploratory::clean_data_frame()
  
  model_df <- df %>% exp_kmeans(`Adult Film Condom Requirements`, `Ban on Single-use Plastic Bags`, `Carryout Bag Charges`, `Cigarette Tax`, `Corporate Political Spending Advisory Question`, `Criminal Sentences & Juvenile Crime Proceedings`, `Death Penalty Procedure Time Limits`, `English Proficiency. Multilingual Education.`, `Firearms and Ammunition Sales`, `K-12 and Community College Facilities`, `Legislative Procedure Requirements`, `Marijuana Legalization`, `Medi-Cal Hospital Fee Program`, `Repeal of Death Penalty`, `State Prescription Drug Purchase Standards`, `Tax Extension for Education and Healthcare`, `Voter Approval of Revenue Bonds`, algorithm = "Hartigan-Wong", normalize_data = TRUE, elbow_method_mode = FALSE, centers=2)
  res <- model_df %>% broom::tidy(model, type="data")
  # Verify that on PC1 axis, 2 cluster means are separated more than the sum of each group's standard deviation.
  summarized <- res %>% group_by(cluster) %>% summarize(m=mean(PC1),sd=sd(PC1))
  expect_true(abs(summarized$m[[1]] - summarized$m[[2]]) > summarized$sd[[1]] + summarized$sd[[2]])
})
