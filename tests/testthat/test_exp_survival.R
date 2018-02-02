context("test exp_survival")

test_that("test exp_survival", {
  # log simulation data
  data <- structure(list(weeks_on_service = c(18, 13, 0, 7, 0, 0, 1, 0,
                                              0, 0), is_churned = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
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
  data <- data %>% rename(`weeks on service`=weeks_on_service, `is churned`=is_churned)
  ret <- data %>% exp_survival(`weeks on service`, `is churned`, cohort=os)
  ret <- data %>% exp_survival(`weeks on service`, `is churned`)


})
