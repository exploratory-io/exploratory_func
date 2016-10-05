
test_that("anomary_detection", {
  data <- readRDS("~/Downloads/binded.rds")
  renamed <- dplyr::rename(data, `with space` = date) %>%
    dplyr::filter(name == "cookpad")
  do_anomaly_detection(renamed, `with space`, Close, alpha = 0.00001)
})
