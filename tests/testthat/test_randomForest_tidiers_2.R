context("test tidiers for ranger randomForest")

test_that("test ranger with regression", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- as.integer(test_data$CARRIER == "AA")
  model_ret <- build_model(test_data,
                           model_func = rangerReg,
                           formula = IS_AA ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  stats_ret <- model_stats(model_ret)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
})

