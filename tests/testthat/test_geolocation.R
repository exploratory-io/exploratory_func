context("test for geolocation functions")

# this is to check event time taking test
with_long_test = FALSE

test_that("countrycode", {
  # ISO to Correlates of War
  res <- countrycode(c('USA', 'DZA'), 'iso3c', 'cown')
  expect_equal(res, c(2, 615))
  # English to ISO
  res <- countrycode('Albania', 'country.name', 'iso3c')
  expect_equal(res, "ALB")
  # German to French
  res <- countrycode('Albanien', 'country.name.de', 'iso.name.fr')
  expect_equal(res, "Albanie (l')")
})

test_that("ip to country", {
  if(with_long_test){
    loadNamespace("dplyr")
    test_df <- data.frame(ip=c("133.43.96.45", "133.68.18.180", "165.213.131.21", "139.230.35.135"))
    result <- (
      test_df
      %>%  dplyr::mutate(country = ip_to_country(ip))
    )
    expect_equal(result[["country"]], c("Japan", "Japan", "Republic of Korea", "Australia"))
  } else {
    skip("skip get_country")
  }
})
