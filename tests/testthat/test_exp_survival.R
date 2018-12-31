context("test exp_survival")

test_that("test exp_survival", {
  # log simulation data
  data <- structure(list(weeks_on_service = c(18, 13, 1, 7, 1, 1, 2, 1,
                                              1, 1), is_churned = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
                                                                    TRUE, FALSE, TRUE), os = structure(c(1L, 2L, 1L, 1L, 2L, 2L,
                                                                                                         2L, 2L, 2L, 1L), .Label = c("Windows", "Mac"), class = "factor"),
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
                                                                         "Uruguay", "Venezuela", "Vietnam", "Zambia"), class = "factor")),
                    row.names = c(NA,-10L), class = c("tbl_df", "tbl", "data.frame"), .Names = c("weeks_on_service","is_churned", "os", "country"))
  data <- data %>% mutate(start_date = as.Date("2018-09-15")+lubridate::weeks(weeks_on_service))
  data <- data %>% mutate(end_date = start_date+lubridate::weeks(weeks_on_service))
  data$end_date[[3]] <- NA #set NAs
  data$end_date[[5]] <- NA
  data <- data %>% rename(`weeks on service`=weeks_on_service, `is churned`=is_churned, `o s`=os, `start date`=start_date, `end date`=end_date)

  ret <- data %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)
  ret <- data %>% exp_survival(`weeks on service`, `is churned`)
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)

  browser()
  data2 <- data %>% mutate(`o s` = "Windows") # test single value cohort case
  ret <- data2 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy(model1)
  expect_true(!is.null(ret1$cohort))
  ret2 <- ret %>% tidy(model2)

  data3 <- data %>% mutate(`o s` = factor(`o s`)) # test cohort as factor
  ret <- data3 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy(model1)
  expect_true(is.factor(ret1$cohort)) # factor levels should be kept
  ret2 <- ret %>% tidy(model2)

  data3 <- data %>% mutate(`o s` = `o s` == "Windows") # test cohort as logical 
  ret <- data3 %>% exp_survival(`weeks on service`, `is churned`, cohort=`o s`)
  ret1 <- ret %>% tidy(model1)
  expect_true(is.factor(ret1$cohort))
  ret2 <- ret %>% tidy(model2)

  # test with start/end time instead of length of time. checking if it runs without error.
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, cohort=`o s`)
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill="today")
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill="2020-01-01")
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)
  ret <- data %>% exp_survival(NULL, `is churned`, start_time=`start date`, end_time=`end date`, end_time_fill=as.Date("2020-01-01"))
  ret1 <- ret %>% tidy(model1)
  ret2 <- ret %>% tidy(model2)

})
