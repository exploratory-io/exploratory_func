context("test exp_survival")

test_that("test exp_survival", {
  # log simulation data
  data <- tibble::tibble(weeks_on_service = c(18, 13, 1, 7, 1, 1, 2, 1, 1, 1),
                         is_churned = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, NA, FALSE, TRUE),
                         os = structure(c(1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L),
                                        .Label = c("Windows", "Mac"), class = "factor"),
                         country = structure(c(13L, 82L, 27L, 82L, 82L, 27L, 13L,
                                               29L, 1L, 82L), .Label = c("Japan", "Afghanistan", "Argentina",
                                                                         "Australia", "Austria", "Belgium", "Benin", "Bermuda", "Bolivia",
                                                                         "Bosnia and Herzegovina", "Brazil", "Bulgaria", "Canada",
                                                                         "Chile", "China", "Colombia", "Costa Rica", "Croatia", "Czech Republic",
                                                                         "Denmark", "Dominican Republic", "Ecuador", "Egypt", "El Salvador",
                                                                         "Ethiopia", "Finland", "France", "Georgia", "Germany", "Greece",
                                                                         "Hashemite Kingdom of Jordan", "Hong Kong", "Hungary", "Iceland",
                                                                         "India", "Indonesia", "Ireland", "Israel", "Italy", "Kenya",
                                                                         "Kosovo", "Latvia", "Malaysia", "Mali", "Malta", "Mexico",
                                                                         "Mongolia", "Nepal", "Netherlands", "New Zealand", "Nigeria",
                                                                         "Norway", "Oman", "Panama", "Peru", "Philippines", "Poland",
                                                                         "Portugal", "Republic of Korea", "Republic of Lithuania",
                                                                         "Russia", "Saudi Arabia", "Senegal", "Serbia", "Singapore",
                                                                         "Slovak Republic", "Slovenia", "Somalia", "South Africa",
                                                                         "Spain", "Sri Lanka", "Sweden", "Switzerland", "Taiwan",
                                                                         "Tajikistan", "Thailand", "Trinidad and Tobago", "Turkey",
                                                                         "Ukraine", "United Arab Emirates", "United Kingdom", "United States",
                                                                         "Uruguay", "Venezuela", "Vietnam", "Zambia"), class = "factor"),
                         grp = c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"))
  data <- data %>% mutate(start_date = as.Date("2018-09-15")+lubridate::weeks(weeks_on_service))
  data <- data %>% mutate(end_date = start_date+lubridate::weeks(weeks_on_service))
  data$end_date[[3]] <- NA #set NAs
  data$end_date[[5]] <- NA
  data <- data %>% rename(`weeks on service`=weeks_on_service, `is churned`=is_churned, `o s`=os, `start date`=start_date, `end date`=end_date)

  ret <- data %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1, type="survival_rate")
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  # Rows is the number of data except NAs.
  expect_equal(ret3$`Rows`, sum(!is.na(data$`is churned`), na.rm=TRUE))
  # Rows (TRUE) is the number of TRUEs.
  expect_equal(ret3$`Rows (TRUE)`,sum(data$`is churned`, na.rm=TRUE))

  # No cohort case
  ret <- data %>% exp_survival(`weeks on service`, `is churned`)
  ret1 <- ret %>% tidy_rowwise(model1, type="survival_rate")
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  # ret3 should return a data frame with "Rows" and "Rows (TRUE)" columns
  expect_equal(colnames(ret3), c("Rows","Rows (TRUE)"))
  # Rows is the number of data except NAs.
  expect_equal(ret3$`Rows`, sum(!is.na(data$`is churned`), na.rm=TRUE))
  # Rows (TRUE) is the number of TRUEs.
  expect_equal(ret3$`Rows (TRUE)`,sum(data$`is churned`, na.rm=TRUE))

  data2 <- data %>% mutate(`o s` = "Windows") # test single value cohort case
  ret <- data2 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1)
  expect_true(!is.null(ret1$Cohort))
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  expect_true(is.character(ret3$Note)) # There should be Note column with error.

  data3 <- data %>% mutate(`o s` = factor(`o s`)) # test cohort as factor
  ret <- data3 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1)
  expect_true(is.factor(ret1$Cohort)) # factor levels should be kept
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)

  data3 <- data %>% mutate(`o s` = `o s` == "Windows") # test cohort as logical 
  ret <- data3 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1)
  expect_true(is.factor(ret1$Cohort))
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)

  # test with start/end time instead of length of time. checking if it runs without error.
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill="today")
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill="2020-01-01")
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill=as.Date("2020-01-01"))
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)

  # Make sure group_by column is kept in the outout.
  data4 <- data %>% group_by(grp)
  ret <- data4 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy_rowwise(model1)
  ret2 <- ret %>% tidy_rowwise(model2)
  ret3 <- ret %>% glance_rowwise(model2)
  expect_equal(colnames(ret1), c("grp","Cohort","Time","Observations","Events","Censored","Survival Rate","Std Error","Conf Low","Conf High"))
  expect_true("grp" %in% colnames(ret2))
  expect_true("grp" %in% colnames(ret3))
})

test_that("exp_survival max_nrow caps the subjects the curve is fitted on", {
  set.seed(1)
  data <- data.frame(
    `weeks on service` = sample(1:50, 400, replace = TRUE),
    `is churned` = rep(c(TRUE, FALSE), 200),
    grp = rep(c("a", "b"), each = 200),
    check.names = FALSE
  )

  full <- data %>% exp_survival(`weeks on service`, `is churned`) %>% glance_rowwise(model2)
  capped <- data %>% exp_survival(`weeks on service`, `is churned`, max_nrow = 100) %>%
    glance_rowwise(model2)

  # glance's Rows is the number of observations the fit actually saw.
  expect_equal(full$Rows, 400)
  expect_equal(capped$Rows, 100)

  # NULL means "every row", which is what the Sample Data checkbox sends when it
  # is off -- it must be identical to not passing the argument at all.
  unlimited <- data %>% exp_survival(`weeks on service`, `is churned`, max_nrow = NULL) %>%
    glance_rowwise(model2)
  expect_equal(unlimited$Rows, 400)

  # A cap above the row count changes nothing.
  above <- data %>% exp_survival(`weeks on service`, `is churned`, max_nrow = 1000) %>%
    glance_rowwise(model2)
  expect_equal(above$Rows, 400)

  # The cap is per group, like every other analytics function's max_nrow.
  grouped <- data %>% dplyr::group_by(grp) %>%
    exp_survival(`weeks on service`, `is churned`, max_nrow = 50) %>%
    glance_rowwise(model2)
  expect_equal(nrow(grouped), 2)
  expect_equal(grouped$Rows, c(50, 50))

  # Same seed, same sample: a sampled run has to be reproducible.
  again <- data %>% exp_survival(`weeks on service`, `is churned`, max_nrow = 100) %>%
    tidy_rowwise(model1)
  expect_equal(again, data %>% exp_survival(`weeks on service`, `is churned`, max_nrow = 100) %>%
    tidy_rowwise(model1))
})
