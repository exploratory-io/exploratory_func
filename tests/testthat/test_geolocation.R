context("test for geolocation functions")

# this is to check event time taking test
with_long_test = FALSE

test_that("ip to country", {
  if(with_long_test){
    loadNamespace("dplyr")
    test_df <- data.frame(ip=c("133.43.96.45", "133.68.18.180", "165.213.131.21", "139.230.35.135"))
    result <- (
      test_df
      %>%  dplyr::mutate(country = get_country(ip))
    )
    expect_equal(result[["country"]], c("Japan", "Japan", "Republic of Korea", "Australia"))
  }
})
