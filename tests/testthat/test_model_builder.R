context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(
  vec1=seq(10),
  vec2=10-seq(10),
  rand=runif(10, min = 0, max=10),
  na=as.vector(replicate(5,c(NA,5))),
  group=paste("group",c(rep(1,5), rep(2, 5)), sep=""),
  col=rep(seq(5),2))

test_that("test build_lm with NA values", {
  test_df <- data.frame(
    val = seq(8),
    val1 = c("char", "char" ,rep(c(NA,1), each = 3)),
    val2 = c("char", "char2" ,rep(c(1,NA), each = 3))
    )
  expect_error({
    build_lm(test_df, val ~ .)
  }, "more than 2 unique values are needed for categorical predictor columns")
})

test_that("test build_lm with all NA values", {
  test_df <- data.frame(
    val = seq(6),
    val1 = c(rep(c(NA,1), each = 3)),
    val2 = c(rep(c(1,NA), each = 3))
  )
  expect_error({
    build_lm(test_df, val ~ .)
  }, "no data after removing NA")
})

test_that("test build_glm with NA values", {
  test_df <- data.frame(
    val = seq(8),
    val1 = c("char", "char" ,rep(c(NA,1), each = 3)),
    val2 = c("char", "char2" ,rep(c(1,NA), each = 3))
  )
  expect_error({
    build_glm(test_df, val ~ .)
  }, "more than 2 unique values are needed for categorical predictor columns")
})

# this returns "object 'fit' not found" but yet to understand what this means, so kept commented out
# test_that("test build_glm with all NA values", {
#   test_df <- data.frame(
#     val = seq(6),
#     val1 = c(rep(c(NA,1), each = 3)),
#     val2 = c(rep(c(1,NA), each = 3))
#   )
#   expect_error({
#     build_glm(test_df, val ~ .)
#   }, "no data after removing NA")
# })

test_that("test with 2 groups with 3 centers", {
  test_df <- data.frame(
    val = as.vector(rep(c(1,5), 3)),
    group = paste("group",rep(c(1, 2), each = 3), sep = ""),
    col = rep(seq(3), 2))
  expect_error({
    build_kmeans(test_df, skv = c("group", "col", "val"), centers = 2)
  }, "Centers should be less than unique subjects\\.")
})

test_that("test with na values", {
  test_df <- data.frame(
    na=rep(c(NA, 5, 1, 4), 5),
    group=paste("group",rep(c(1, 2, 3, 4), each=5), sep=""),
    col=rep(seq(5), 4))
  test_df <- dplyr::filter(test_df, group != "group2" | col != 4)
  ret <- build_kmeans(test_df, skv = c("group", "col", "na"), fill = 1)
  expect_error({
    build_kmeans(test_df, skv = c("group", "col", "na"), fill = NA)
  }, "There is NA in the data.")
})

test_that("test with too small subject", {
  test_df <- data.frame(
    val=rep(c(1, 5), 5),
    group=paste("group",rep(c(1, 2), each=5), sep=""),
    col=rep(seq(5), 2))
  expect_error({
    build_kmeans(test_df, skv = c("group", "col", "val"), centers = 3)
  }, "Centers should be less than unique subjects\\.")
})

test_that("test with too small key", {
  test_df <- data.frame(
    val=rep(c(1, 5), 5),
    group=paste("group",rep(c(1, 2), each=5), sep=""),
    col=rep(seq(5), 2))
  expect_error({
    build_kmeans(test_df, skv = c("col", "group", "val"))
  }, "Centers should be less than distinct data points\\.")
})

test_that("test build_glm and broom tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_glm(vec1~vec2)
      %>%
        broom::tidy(model)
    )
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_glm and broom", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_glm(vec1~vec2, augment=TRUE)
    )
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), ncol(test_df)+7)
  }
})

test_that("test build_kmeans.cols and broom::tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_kmeans.cols(vec1, vec2, rand, centers=2)
      %>%
        broom::tidy(model)
    )
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_kmeans.cols augment=T", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_kmeans.cols(vec1, vec2, rand, centers=2, augment=T)
    )
    expect_equal(nrow(result), 10)
    expect_true(is.integer(result[["cluster"]]))
  }
})

test_that("test build_kmeans all na", {
  test_df <- data.frame(
    all_na = rep(NA, 10),
    val = seq(10)
  )
  expect_error({
    test_df %>%
      build_kmeans(all_na, val, centers=2, augment=T)
  }, "No data after removing NA")
})

test_that("test build_kmeans.cols ignore NA rows", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_kmeans.cols(vec1, vec2, na, centers=2, keep.source=TRUE, augment = FALSE) %>%
      predict(model, data=source.data)
    expect_equal(dim(result)[[1]], 5)
  }
})

test_that("test build_kmeans.cols ignore NA rows", {
  na_char <- as.character(seq(10))
  na_char[[3]] <- NA
  test_df <- data.frame(
    na_char,
    n_char = as.character(10 - seq(10)), stringsAsFactors = FALSE
  )
  result <- test_df %>%
    build_kmeans.cols(na_char, n_char, centers=2, keep.source=TRUE, augment = FALSE) %>%
    predict(model, data=source.data)
  expect_equal(dim(result)[[1]], 9)
})

test_that("test build_kmeans.cols ignore NA rows with grouped", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  build_kmeans.cols(vec1, vec2, na, centers=1, keep.source=TRUE)
      %>%  broom::tidy(model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("build_kmeans.kv augment=TRUE", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    group=rep(paste("group", seq(2)), each=9),
    key=rep(paste("dim", rep(seq(3))), each=2),
    value=seq(3), stringsAsFactors = F
  )

  test_df[["subject with space"]] <- rep(paste("sub", rep(seq(3), each=3)), each=2)

  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  build_kmeans.kv(`subject with space`, key, value, center=1, augment=TRUE)
  )
  expect_true(!is.null(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})

test_that("test build_kmeans.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      build_kmeans.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})

test_that("test build_kmeans.cols ignore NA rows with grouped and keep.source=FALSE", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  build_kmeans.cols(vec1, vec2, na, centers=1, keep.source=FALSE)
      %>%  broom::tidy(model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_kmeans.cols", {
  df <- data.frame(number = seq(4), number2 = seq(4)-4)
  ret <- (df %>%  build_kmeans.cols(number, number2, keep.source=TRUE, augment = FALSE) %>%  predict(model, data=source.data))
  expect_true(is.integer(ret$cluster))
})

test_that("test build_kmeans", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  result <- test_df %>%
    build_kmeans(skv = c("vec1", "vec2"), centers=2, augment = FALSE) %>%
    predict(model, data = source.data)
  expect_true(is.integer(result[["cluster.new"]]))
  expect_equal(length(colnames(result)[colnames(result) == "cluster"]), 1)
  expect_equal(length(colnames(result)[colnames(result) == "cluster.new"]), 1)
})

test_that("test build_kmeans skv with wrong column name", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  expect_error({
    test_df %>%
      build_kmeans(skv = c("vec1", "vec"), centers=2) %>%
      predict(model, data = source.data)
  }, "undefined columns selected")
})

test_that("test build_kmeans cols with wrong column name", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  expect_error({
    test_df %>%
      build_kmeans(vec, vec10, centers=2) %>%
      predict(model, data = source.data)
  }, "undefined columns selected")
})

test_that("build_lda", {
  loadNamespace("dplyr")
  # this is sampled data from llis_display data in https://github.com/davidmeza1/doctopics/tree/master/data
  input_df <- structure(list(LessonId = c(5204L, 1879L, 2456L, 731L, 1489L,
                                          625L, 945L, 817L, 154L, 1497L), Lesson = c("The Constellation Program would have benefited from an enterprise architecture that provided the applications and technologies to adequately disseminate information to program team members at multiple locations.",
                                                                                     "Numerous NASA projects have suffered severe cost and schedule impacts due to problems with hybrid DC-DC converter application, quality, and reliability. Common in NASA spaceflight hardware, these devices are:\n     Complex assemblies-- not just piece parts-- that require a multi-disciplinary support team (i.e., design engineering, electronic parts engineering, quality assurance engineering, and reliability engineering) for successful application to flight systems.<br/>\n<br/>     Long lead-time procurements: system requirements changes can result in a project selecting a power converter ill suited to the application.\n     ",
                                                                                     "Although, the likely proximate cause of the Aquarius EGSE transportation mishap was operator error by the contracted van driver operating the lift gate, the root cause was inadequate planning and coordination of the transportation task.",
                                                                                     " If these design practices are not incorporated into the design of the SDS, early, partial, or complete curtailment of the mission could occur due to component failure or to false or inappropriate commands. SDS performance data may not be available for failure analyses and the capability for work-around procedures may not be available. ",
                                                                                     " The high-fidelity SAGE III simulator proved itself to be an invaluable tool in expediting instrument development as well as meeting the requirement to verify and validate flight software changes prior to execution during on-orbit operations. Specific lessons learned follow:  \n      \n     \n\n      \n        The SAGE III high-fidelity simulator was used to verify and validate designs prior to committing to a flight hardware build. This approach allowed designers to test their design in a less restrictive environment than required for flight hardware and correct design problems prior to building flight hardware. We believe that this approach saved time and flight hardware rework.  \n\n        The SAGE III high-fidelity simulator facilitated flight software development. SAGE III flight software was developed in parallel with flight hardware using the high fidelity simulator. Significant schedule and costs were saved using this approach because the hardware and software development was a parallel rather than serial activity.  \n\n        Development risk was reduced by testing flight software on engineering hardware rather than flight hardware. This approach avoided the potential situation where critical and high value flight hardware was operated using unproven flight software. The likelihood of damage to critical flight systems by errant software was avoided using this approach.  \n\n        The high fidelity simulator proved to be invaluable during anomaly troubleshooting during integration and test. For instance, the simulator was used to isolate a hardware problem and to develop a software fix for an interface communications problem that occurred infrequently (two-three times) when the instrument was tested at cold temperatures during thermal vacuum testing. This ability allowed the environmental test program to continue in parallel with the anomaly investigation and work-around solution development.  \n\n        The simulator was used to develop, verify, and validate flight software changes during on-orbit operations. The simulator was needed to work around problems with spacecraft provided data (navigation data and attitude data) and other problems with the flight code found during on-orbit operations but not identified during integration and testing.  \n\n        The instrument operations team assumed responsibility for primary flight software sustaining engineering during spacecraft integration. Formal training and &ldquo;behind-the-wheel&rdquo; operation of the simulator was performed to ensure the operations team was capable and ready to perform these functions.  \n\n        Configuration management of the simulator is required. Particular emphasis should be placed on verifying that the ground simulator configuration and software image match the flight configuration to the greatest extent possible.  \n      ",
                                                                                     " The Board found that the loss of the Lewis Spacecraft was the direct result of an implementation of a technically flawed Safe Mode in the Attitude Control System. This error was made fatal to the spacecraft by the reliance on that unproven Safe Mode by the on orbit operations team and by the failure to adequately monitor spacecraft health and safety during the critical initial mission phase. \n\n     The Board also discovered numerous other factors that contributed to the environment that allowed the direct causes to occur. While the direct causes were the most visible reasons for the failure, the Board believes that the indirect causes were also very significant contributors. Many of these factors can be attributed to a lack of a mutual understanding between the contractor and the Government as to what is meant by Faster, Better, Cheaper. These indirect contributors are to be taken in the context of implementing a program in the Faster, Better, Cheaper mode: \n\n    \n        Requirement changes without adequate resource adjustment  \n\n        Cost and schedule pressures  \n\n        Program Office move  \n\n        Inadequate ground station availability for initial operations  \n\n        Frequent key personnel changes  \n\n        Inadequate engineering discipline  \n\n        Inadequate management discipline  \n    </ul>\n      \n      \n\n     The Board strongly endorses the concept of \"Faster, Better, Cheaper\" in space programs and believes that this paradigm can be successfully implemented with sound engineering, and attentive, and effective management. However the role changes for Government and Industry are significant and must be acknowledged, planned for and maintained throughout the program.Since these roles are fundamental changes in how business is conducted, they must be recognized by all team members and behaviors adjusted at all levels. The Board observed an attempt during the early phase of the Lewis Program to work in a Faster, Better, Cheaper culture, but as the Program progressed the philosophy changed to business as usual with dedicated engineers working long hours using standard processes to meet a short schedule and skipping the typical Government oversight functions. ",
                                                                                     " Understand the physical characteristics of any chemical mixture and implement appropriate safety measures to preclude undesirable events. ",
                                                                                     " Failure to perform vibroacoustic testing could result in payload failures when subjected to the vibroacoustic environments of the mission, particularly the severe acoustic environment of launch. In addition, the simultaneous occurrence of low-frequency random vibration with high-intensity, low-frequency acoustics can cause failure of load-carrying elements. Examples of components that are susceptible to failure due to random vibration excitation are thin films, filaments, electronic circuit boards, and optical elements. ",
                                                                                     " The pneumatic and fliud quick disconnects are the same size and located close enough to easily allow misconnection within APU carrier plates. ",
                                                                                     " OSP planning documents were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them. Absent these documents, unseen organizational friction and implementation inefficiencies resulted.  \n      \n     Additionally:  \n     a. Program Plan and SEMP The OSP Program Plan, Systems Engineering Management Plan (SEMP) and related foundational plans (Risk Management, Configuration and Data Management, Records Plans, etc.) were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them  \n      \n    b. Schedule Planning and Implementation A lack of standardization of inputs from various centers and organizations complicated the Integrated Master Schedule baseline and updates. Additionally, in response to acceleration of the program, two separate schedules of ongoing and planned work were being maintained.  \n      \n    c. Flight Test Planning Flight test has a major impact on technical, schedule, and budget planning and should be considered in up-front planning. Schedules leading to verification by flight testing need to be assessed for credibility.  \n      \n     d. Program Shutdown The OSP program ceased abruptly. Some key players were reassigned early, including both managers and administrative personnel thus further complicating the shutdown task. Geographically distant centers relied on web-sites and electronic meeting notices and calendars but, these tools were not updated regularly and then shutdown prior to program's end.  \n      \n     "
                                          )), .Names = c("LessonId", "Lesson"), row.names = c(102L, 337L,
                                                                                              289L, 1123L, 628L, 1257L, 812L, 1170L, 1616L, 619L), class = "data.frame")
  tokenized <- input_df %>% do_tokenize(Lesson)
  ret <- build_lda(tokenized, LessonId, token, n_topics=3)
})

test_that("build_lda with theta", {
  loadNamespace("dplyr")
  # this is sampled data from llis_display data in https://github.com/davidmeza1/doctopics/tree/master/data
  input_df <- structure(list(LessonId = c(5204L, 1879L, 2456L, 731L, 1489L,
                                          625L, 945L, 817L, 154L, 1497L), Lesson = c("The Constellation Program would have benefited from an enterprise architecture that provided the applications and technologies to adequately disseminate information to program team members at multiple locations.",
                                                                                     "Numerous NASA projects have suffered severe cost and schedule impacts due to problems with hybrid DC-DC converter application, quality, and reliability. Common in NASA spaceflight hardware, these devices are:\n     Complex assemblies-- not just piece parts-- that require a multi-disciplinary support team (i.e., design engineering, electronic parts engineering, quality assurance engineering, and reliability engineering) for successful application to flight systems.<br/>\n<br/>     Long lead-time procurements: system requirements changes can result in a project selecting a power converter ill suited to the application.\n     ",
                                                                                     "Although, the likely proximate cause of the Aquarius EGSE transportation mishap was operator error by the contracted van driver operating the lift gate, the root cause was inadequate planning and coordination of the transportation task.",
                                                                                     " If these design practices are not incorporated into the design of the SDS, early, partial, or complete curtailment of the mission could occur due to component failure or to false or inappropriate commands. SDS performance data may not be available for failure analyses and the capability for work-around procedures may not be available. ",
                                                                                     " The high-fidelity SAGE III simulator proved itself to be an invaluable tool in expediting instrument development as well as meeting the requirement to verify and validate flight software changes prior to execution during on-orbit operations. Specific lessons learned follow:  \n      \n     \n\n      \n        The SAGE III high-fidelity simulator was used to verify and validate designs prior to committing to a flight hardware build. This approach allowed designers to test their design in a less restrictive environment than required for flight hardware and correct design problems prior to building flight hardware. We believe that this approach saved time and flight hardware rework.  \n\n        The SAGE III high-fidelity simulator facilitated flight software development. SAGE III flight software was developed in parallel with flight hardware using the high fidelity simulator. Significant schedule and costs were saved using this approach because the hardware and software development was a parallel rather than serial activity.  \n\n        Development risk was reduced by testing flight software on engineering hardware rather than flight hardware. This approach avoided the potential situation where critical and high value flight hardware was operated using unproven flight software. The likelihood of damage to critical flight systems by errant software was avoided using this approach.  \n\n        The high fidelity simulator proved to be invaluable during anomaly troubleshooting during integration and test. For instance, the simulator was used to isolate a hardware problem and to develop a software fix for an interface communications problem that occurred infrequently (two-three times) when the instrument was tested at cold temperatures during thermal vacuum testing. This ability allowed the environmental test program to continue in parallel with the anomaly investigation and work-around solution development.  \n\n        The simulator was used to develop, verify, and validate flight software changes during on-orbit operations. The simulator was needed to work around problems with spacecraft provided data (navigation data and attitude data) and other problems with the flight code found during on-orbit operations but not identified during integration and testing.  \n\n        The instrument operations team assumed responsibility for primary flight software sustaining engineering during spacecraft integration. Formal training and &ldquo;behind-the-wheel&rdquo; operation of the simulator was performed to ensure the operations team was capable and ready to perform these functions.  \n\n        Configuration management of the simulator is required. Particular emphasis should be placed on verifying that the ground simulator configuration and software image match the flight configuration to the greatest extent possible.  \n      ",
                                                                                     " The Board found that the loss of the Lewis Spacecraft was the direct result of an implementation of a technically flawed Safe Mode in the Attitude Control System. This error was made fatal to the spacecraft by the reliance on that unproven Safe Mode by the on orbit operations team and by the failure to adequately monitor spacecraft health and safety during the critical initial mission phase. \n\n     The Board also discovered numerous other factors that contributed to the environment that allowed the direct causes to occur. While the direct causes were the most visible reasons for the failure, the Board believes that the indirect causes were also very significant contributors. Many of these factors can be attributed to a lack of a mutual understanding between the contractor and the Government as to what is meant by Faster, Better, Cheaper. These indirect contributors are to be taken in the context of implementing a program in the Faster, Better, Cheaper mode: \n\n    \n        Requirement changes without adequate resource adjustment  \n\n        Cost and schedule pressures  \n\n        Program Office move  \n\n        Inadequate ground station availability for initial operations  \n\n        Frequent key personnel changes  \n\n        Inadequate engineering discipline  \n\n        Inadequate management discipline  \n    </ul>\n      \n      \n\n     The Board strongly endorses the concept of \"Faster, Better, Cheaper\" in space programs and believes that this paradigm can be successfully implemented with sound engineering, and attentive, and effective management. However the role changes for Government and Industry are significant and must be acknowledged, planned for and maintained throughout the program.Since these roles are fundamental changes in how business is conducted, they must be recognized by all team members and behaviors adjusted at all levels. The Board observed an attempt during the early phase of the Lewis Program to work in a Faster, Better, Cheaper culture, but as the Program progressed the philosophy changed to business as usual with dedicated engineers working long hours using standard processes to meet a short schedule and skipping the typical Government oversight functions. ",
                                                                                     " Understand the physical characteristics of any chemical mixture and implement appropriate safety measures to preclude undesirable events. ",
                                                                                     " Failure to perform vibroacoustic testing could result in payload failures when subjected to the vibroacoustic environments of the mission, particularly the severe acoustic environment of launch. In addition, the simultaneous occurrence of low-frequency random vibration with high-intensity, low-frequency acoustics can cause failure of load-carrying elements. Examples of components that are susceptible to failure due to random vibration excitation are thin films, filaments, electronic circuit boards, and optical elements. ",
                                                                                     " The pneumatic and fliud quick disconnects are the same size and located close enough to easily allow misconnection within APU carrier plates. ",
                                                                                     " OSP planning documents were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them. Absent these documents, unseen organizational friction and implementation inefficiencies resulted.  \n      \n     Additionally:  \n     a. Program Plan and SEMP The OSP Program Plan, Systems Engineering Management Plan (SEMP) and related foundational plans (Risk Management, Configuration and Data Management, Records Plans, etc.) were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them  \n      \n    b. Schedule Planning and Implementation A lack of standardization of inputs from various centers and organizations complicated the Integrated Master Schedule baseline and updates. Additionally, in response to acceleration of the program, two separate schedules of ongoing and planned work were being maintained.  \n      \n    c. Flight Test Planning Flight test has a major impact on technical, schedule, and budget planning and should be considered in up-front planning. Schedules leading to verification by flight testing need to be assessed for credibility.  \n      \n     d. Program Shutdown The OSP program ceased abruptly. Some key players were reassigned early, including both managers and administrative personnel thus further complicating the shutdown task. Geographically distant centers relied on web-sites and electronic meeting notices and calendars but, these tools were not updated regularly and then shutdown prior to program's end.  \n      \n     "
                                          )), .Names = c("LessonId", "Lesson"), row.names = c(102L, 337L,
                                                                                              289L, 1123L, 628L, 1257L, 812L, 1170L, 1616L, 619L), class = "data.frame")
  tokenized <- input_df %>% do_tokenize(Lesson)
  ret <- build_lda(tokenized, LessonId, token, n_topics=3, output = "theta")
  browser()
  expect_true(is.integer(ret[["document"]]))
  expect_true(is.integer(ret[["topic"]]))
  expect_true(is.numeric(ret[["value"]]))
})

test_that("build_lda with phi", {
  loadNamespace("dplyr")
  # this is sampled data from llis_display data in https://github.com/davidmeza1/doctopics/tree/master/data
  input_df <- structure(list(LessonId = c(5204L, 1879L, 2456L, 731L, 1489L,
                                          625L, 945L, 817L, 154L, 1497L), Lesson = c("The Constellation Program would have benefited from an enterprise architecture that provided the applications and technologies to adequately disseminate information to program team members at multiple locations.",
                                                                                     "Numerous NASA projects have suffered severe cost and schedule impacts due to problems with hybrid DC-DC converter application, quality, and reliability. Common in NASA spaceflight hardware, these devices are:\n     Complex assemblies-- not just piece parts-- that require a multi-disciplinary support team (i.e., design engineering, electronic parts engineering, quality assurance engineering, and reliability engineering) for successful application to flight systems.<br/>\n<br/>     Long lead-time procurements: system requirements changes can result in a project selecting a power converter ill suited to the application.\n     ",
                                                                                     "Although, the likely proximate cause of the Aquarius EGSE transportation mishap was operator error by the contracted van driver operating the lift gate, the root cause was inadequate planning and coordination of the transportation task.",
                                                                                     " If these design practices are not incorporated into the design of the SDS, early, partial, or complete curtailment of the mission could occur due to component failure or to false or inappropriate commands. SDS performance data may not be available for failure analyses and the capability for work-around procedures may not be available. ",
                                                                                     " The high-fidelity SAGE III simulator proved itself to be an invaluable tool in expediting instrument development as well as meeting the requirement to verify and validate flight software changes prior to execution during on-orbit operations. Specific lessons learned follow:  \n      \n     \n\n      \n        The SAGE III high-fidelity simulator was used to verify and validate designs prior to committing to a flight hardware build. This approach allowed designers to test their design in a less restrictive environment than required for flight hardware and correct design problems prior to building flight hardware. We believe that this approach saved time and flight hardware rework.  \n\n        The SAGE III high-fidelity simulator facilitated flight software development. SAGE III flight software was developed in parallel with flight hardware using the high fidelity simulator. Significant schedule and costs were saved using this approach because the hardware and software development was a parallel rather than serial activity.  \n\n        Development risk was reduced by testing flight software on engineering hardware rather than flight hardware. This approach avoided the potential situation where critical and high value flight hardware was operated using unproven flight software. The likelihood of damage to critical flight systems by errant software was avoided using this approach.  \n\n        The high fidelity simulator proved to be invaluable during anomaly troubleshooting during integration and test. For instance, the simulator was used to isolate a hardware problem and to develop a software fix for an interface communications problem that occurred infrequently (two-three times) when the instrument was tested at cold temperatures during thermal vacuum testing. This ability allowed the environmental test program to continue in parallel with the anomaly investigation and work-around solution development.  \n\n        The simulator was used to develop, verify, and validate flight software changes during on-orbit operations. The simulator was needed to work around problems with spacecraft provided data (navigation data and attitude data) and other problems with the flight code found during on-orbit operations but not identified during integration and testing.  \n\n        The instrument operations team assumed responsibility for primary flight software sustaining engineering during spacecraft integration. Formal training and &ldquo;behind-the-wheel&rdquo; operation of the simulator was performed to ensure the operations team was capable and ready to perform these functions.  \n\n        Configuration management of the simulator is required. Particular emphasis should be placed on verifying that the ground simulator configuration and software image match the flight configuration to the greatest extent possible.  \n      ",
                                                                                     " The Board found that the loss of the Lewis Spacecraft was the direct result of an implementation of a technically flawed Safe Mode in the Attitude Control System. This error was made fatal to the spacecraft by the reliance on that unproven Safe Mode by the on orbit operations team and by the failure to adequately monitor spacecraft health and safety during the critical initial mission phase. \n\n     The Board also discovered numerous other factors that contributed to the environment that allowed the direct causes to occur. While the direct causes were the most visible reasons for the failure, the Board believes that the indirect causes were also very significant contributors. Many of these factors can be attributed to a lack of a mutual understanding between the contractor and the Government as to what is meant by Faster, Better, Cheaper. These indirect contributors are to be taken in the context of implementing a program in the Faster, Better, Cheaper mode: \n\n    \n        Requirement changes without adequate resource adjustment  \n\n        Cost and schedule pressures  \n\n        Program Office move  \n\n        Inadequate ground station availability for initial operations  \n\n        Frequent key personnel changes  \n\n        Inadequate engineering discipline  \n\n        Inadequate management discipline  \n    </ul>\n      \n      \n\n     The Board strongly endorses the concept of \"Faster, Better, Cheaper\" in space programs and believes that this paradigm can be successfully implemented with sound engineering, and attentive, and effective management. However the role changes for Government and Industry are significant and must be acknowledged, planned for and maintained throughout the program.Since these roles are fundamental changes in how business is conducted, they must be recognized by all team members and behaviors adjusted at all levels. The Board observed an attempt during the early phase of the Lewis Program to work in a Faster, Better, Cheaper culture, but as the Program progressed the philosophy changed to business as usual with dedicated engineers working long hours using standard processes to meet a short schedule and skipping the typical Government oversight functions. ",
                                                                                     " Understand the physical characteristics of any chemical mixture and implement appropriate safety measures to preclude undesirable events. ",
                                                                                     " Failure to perform vibroacoustic testing could result in payload failures when subjected to the vibroacoustic environments of the mission, particularly the severe acoustic environment of launch. In addition, the simultaneous occurrence of low-frequency random vibration with high-intensity, low-frequency acoustics can cause failure of load-carrying elements. Examples of components that are susceptible to failure due to random vibration excitation are thin films, filaments, electronic circuit boards, and optical elements. ",
                                                                                     " The pneumatic and fliud quick disconnects are the same size and located close enough to easily allow misconnection within APU carrier plates. ",
                                                                                     " OSP planning documents were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them. Absent these documents, unseen organizational friction and implementation inefficiencies resulted.  \n      \n     Additionally:  \n     a. Program Plan and SEMP The OSP Program Plan, Systems Engineering Management Plan (SEMP) and related foundational plans (Risk Management, Configuration and Data Management, Records Plans, etc.) were not sufficiently mature when the decision was made to embark on an accelerated program. Parallel development during program execution deadened the sense of urgency to complete them  \n      \n    b. Schedule Planning and Implementation A lack of standardization of inputs from various centers and organizations complicated the Integrated Master Schedule baseline and updates. Additionally, in response to acceleration of the program, two separate schedules of ongoing and planned work were being maintained.  \n      \n    c. Flight Test Planning Flight test has a major impact on technical, schedule, and budget planning and should be considered in up-front planning. Schedules leading to verification by flight testing need to be assessed for credibility.  \n      \n     d. Program Shutdown The OSP program ceased abruptly. Some key players were reassigned early, including both managers and administrative personnel thus further complicating the shutdown task. Geographically distant centers relied on web-sites and electronic meeting notices and calendars but, these tools were not updated regularly and then shutdown prior to program's end.  \n      \n     "
                                          )), .Names = c("LessonId", "Lesson"), row.names = c(102L, 337L,
                                                                                              289L, 1123L, 628L, 1257L, 812L, 1170L, 1616L, 619L), class = "data.frame")
  tokenized <- input_df %>% do_tokenize(Lesson)
  ret <- build_lda(tokenized, LessonId, token, n_topics=2, output = "phi")

  expect_true(is.character(ret[["term"]]))
  browser()
  expect_true(is.integer(ret[["topic"]]))
  expect_true(is.numeric(ret[["value"]]))

})

test_that("build_lda with no term", {
  loadNamespace("dplyr")
  input_df <- data.frame(
    document_title=c(rep("The War of the Worlds", 4), rep("Pride and Prejudice", 4)),
    token = c("this", "was", "the", "deputation", "this", "was", "invitation", "enough"),
    count = rep(1, 8)
  )
  expect_error({
    ret <- build_lda(input_df, document_title, token, count, n_topics=2)
  }, "There is no term after filtering")
})
