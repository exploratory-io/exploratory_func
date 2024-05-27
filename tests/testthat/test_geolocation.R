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

  res <- countrycode(c("estoniA","latVia","LithuanIa", "IrAn", "jaPan", "Canada"), origin = "country.name", destination = "region23")
  expect_equal(res, c("Eastern Europe", "Eastern Europe", "Eastern Europe", "Western Asia", "Eastern Asia", "Northern America"))

  res <- countrycode(c("estoniA","latVia","LithuanIa", "IrAn", "jaPan", "Canada"), origin = "country.name", destination = "region11")
  expect_equal(res, c("East Europe","East Europe","East Europe","West Asia","East Asia","North America"))


  # origin="flex" special mode. Input can be in either iso2c, iso3c or name.
  res <- exploratory::countrycode(c('Afghanistan', 'USA', 'NZ'), 'flex', 'iso2c')
  expect_equal(res, c('AF', 'US', 'NZ'))

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

test_that("geocode_us_state", {
  res <- exploratory::geocode_us_state(data.frame(state=c("CA", "NY")), "state")
  expect_equal(TRUE, all(!is.na(res$longitude)))
  expect_equal(TRUE, all(!is.na(res$latitude)))
})

test_that("geocode_us_county", {
  res <- exploratory::geocode_us_county(data.frame(code=c("01003", "13005")), "code")
  expect_equal(TRUE, all(!is.na(res$longitude)))
  expect_equal(TRUE, all(!is.na(res$latitude)))
})

test_that("geocode_world_country", {
  res <- exploratory::geocode_world_country(data.frame(code=c("JP", "GB")), "code")
  expect_equal(TRUE, all(!is.na(res$longitude)))
  expect_equal(TRUE, all(!is.na(res$latitude)))
})
