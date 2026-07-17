# how to run this test:
# devtools::test(filter="factanal")
context("test factor analysis function, exp_factanal")

test_that("exp_factanal with default orthogonal varimax rotation", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  check_output <- function(model_df) {
    res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
    expect_equal(colnames(res),
                 c("Factors", "Variance Explained (Ratio)", "Variance Explained", "Chi-Square", "P Value", "DF", "Rows", "Method", "Rotation", "RMSR", "RMSEA", "TLI", "BIC"))
    res <- model_df %>% tidy_rowwise(model, type="variances")
    expect_equal(colnames(res),
                 c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion", "Factor", "% Variance", "Cummulated % Variance"))
    res <- model_df %>% tidy_rowwise(model, type="loadings")
    # Make sure that factor levels set on variable column is sorted by top factor.
    # The first level should be a variable whose top factor is factor 1.
    factor_1_top_var <- levels(res$variable)[1]
    expect_equal(as.character((res %>% dplyr::filter(variable==!!factor_1_top_var & factor %nin% c("Communality","Uniqueness")) %>% dplyr::arrange(desc(abs(value))))$factor[1]), "Factor 1")
    expect_equal(colnames(res),
                 c("variable", "factor", "value"))
    expect_equal(levels(res$factor), c("Factor 1", "Factor 2", "Factor 3", "Communality", "Uniqueness")) # Verify that order of factor levels are in order.
    res <- model_df %>% tidy_rowwise(model, type="correlation")
    # For orthogonal rotations, correlation should return empty dataframe.
    expect_equal(nrow(res), 0)
    res <- model_df %>% tidy_rowwise(model, type="biplot")
    # Factor 3 can be various column names like "MR3" here.
    expect_true(all(c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_col", ".factor_1", ".factor_2", ".variable", ".factor_2_variable") %in% colnames(res)))
    res <- model_df %>% tidy_rowwise(model, type="screeplot")
    expect_equal(colnames(res),
                 c("factor", "eigenvalue"))
    res <- model_df %>% tidy_rowwise(model, type="data")
    expect_equal(colnames(res),
                 c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","Factor 1","Factor 2","Factor 3"))
    # New report tidy types (issue #37018).
    res <- model_df %>% tidy_rowwise(model, type="suitability")
    expect_equal(colnames(res), c("Metric", "Value", "Judgement", "Description", "status"))
    expect_equal(res$Metric, c("KMO", "Bartlett's Test of Sphericity", "Rows Used", "Variables Used"))
    res <- model_df %>% tidy_rowwise(model, type="factor_count")
    expect_equal(colnames(res), c("Method", "Recommended Number of Factors", "Description"))
    expect_equal(res$Method, c("Kaiser Criterion", "Parallel Analysis", "Scree Plot"))
    expect_equal(res$`Recommended Number of Factors`[3], "Check the chart")
    res <- model_df %>% tidy_rowwise(model, type="parallel_screeplot")
    expect_equal(colnames(res), c("Factor", "Eigenvalue", "Random Data Eigenvalue"))
    res <- model_df %>% tidy_rowwise(model, type="loadings_wide")
    expect_equal(colnames(res), c("variable", "Factor 1", "Factor 2", "Factor 3", "Judgement", "judgement_status", "primary_factor", "secondary_factors", "direction"))
    # Every status token must be one of the known set (guards against a typo drifting from the client tooltip keys).
    expect_true(all(res$judgement_status %in% c("strong", "moderate", "near_crossload", "crossload", "ambiguous_crossload", "low_loading")))
    res <- model_df %>% tidy_rowwise(model, type="communalities")
    expect_equal(colnames(res), c("variable", "Communality", "Uniqueness", "Judgement", "judgement_status"))
    res <- model_df %>% tidy_rowwise(model, type="communalities_long")
    expect_equal(colnames(res), c("variable", "Component", "Ratio"))
    expect_equal(levels(res$Component), c("Communality", "Uniqueness"))
  }

  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="minres") 
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="ml")
  check_output(model_df)
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="pa") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="ols")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="wls")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="gls")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="minchi")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=3, fm="minrank")
  check_output(model_df)
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="alpha") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
})

test_that("exp_factanal with oblique Promax rotation", {
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/iq0yb0iifbv7vkc/airline_2013_10_tricky_v3_50k.csv?dl=1", delim = NULL, quote = "\"" , col_names = TRUE , na = c('') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
    readr::type_convert() %>%
    exploratory::clean_data_frame()

  check_output <- function(model_df) {
    res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
    expect_equal(colnames(res),
                 c("Factors", "Variance Explained (Ratio)", "Variance Explained", "Chi-Square", "P Value", "DF", "Rows", "Method", "Rotation", "RMSR", "RMSEA", "TLI", "BIC"))
    res <- model_df %>% tidy_rowwise(model, type="variances")
    expect_equal(colnames(res),
                 c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion", "Factor", "% Variance", "Cummulated % Variance"))
    res <- model_df %>% tidy_rowwise(model, type="loadings")
    # Make sure that factor levels set on variable column is sorted by top factor.
    # The first level should be a variable whose top factor is factor 1.
    factor_1_top_var <- levels(res$variable)[1]
    expect_equal(as.character((res %>% dplyr::filter(variable==!!factor_1_top_var & factor %nin% c("Communality","Uniqueness")) %>% dplyr::arrange(desc(abs(value))))$factor[1]), "Factor 1")
    expect_equal(colnames(res),
                 c("variable", "factor", "value"))
    expect_equal(levels(res$factor), c("Factor 1", "Factor 2", "Communality", "Uniqueness")) # Verify that order of factor levels are in order.
    res <- model_df %>% tidy_rowwise(model, type="correlation")
    # For orthogonal rotations, correlation should return empty dataframe.
    expect_equal(colnames(res),
                 c("factor1", "factor2", "correlation"))
    expect_true(all(stringr::str_detect(res$factor1, "^Factor ")))
    expect_true(all(stringr::str_detect(res$factor2, "^Factor ")))
    expect_true(all(levels(res$factor1) == stringr::str_c("Factor ", 1:2)))
    res <- model_df %>% tidy_rowwise(model, type="biplot")
    # Factor 3 can be various column names like "MR3" here.
    expect_equal(colnames(res),
      c("YE AR","MON TH"    ,"DAY OF MONTH"      ,"FL DATE"           ,"CAR RIER",
        "TAIL NUM"          ,"FL NUM"            ,"ORI GIN"           ,"ORIGIN CITY NAME"  ,"ORIGIN STATE ABR",
        "DE ST"             ,"DEST CITY NAME"    ,"DEST STATE ABR"    ,"DEP TIME"          ,"DEP DELAY"       ,
        "ARR TIME"          ,"ARR DELAY"         ,"CAN CELLED"        ,"CANCELLATION CODE" ,"AIR TIME"        ,
        "DIS TANCE"         ,"WEATHER DELAY"     ,"delay ed"          ,"is UA"             ,"is delayed"      ,
        "end time"          ,"is UA or AA"       ,".factor_1"         ,".factor_2"         ,
        ".variable"         ,".factor_2_variable"))
    res <- model_df %>% tidy_rowwise(model, type="screeplot")
    expect_equal(colnames(res),
                 c("factor", "eigenvalue"))
    res <- model_df %>% tidy_rowwise(model, type="data")
    expect_equal(colnames(res),
      c("YE AR"            ,"MON TH"           ,"DAY OF MONTH"     ,"FL DATE"          ,"CAR RIER"         ,"TAIL NUM"      ,  
        "FL NUM"           ,"ORI GIN"          ,"ORIGIN CITY NAME" ,"ORIGIN STATE ABR" ,"DE ST"            ,"DEST CITY NAME",  
        "DEST STATE ABR"   ,"DEP TIME"         ,"DEP DELAY"        ,"ARR TIME"         ,"ARR DELAY"        ,"CAN CELLED"    ,  
        "CANCELLATION CODE","AIR TIME"         ,"DIS TANCE"        ,"WEATHER DELAY"    ,"delay ed"         ,"is UA"         ,  
        "is delayed"       ,"end time"         ,"is UA or AA"      ,"Factor 1"         ,"Factor 2"))
  }

  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "minres", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)
  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "ml", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="pa") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "ols", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)
  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "wls", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)
  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "gls", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)
  model_df <- df %>% exp_factanal(`DAY OF MONTH`, `FL NUM`, `DEP DELAY`, `ARR DELAY`, `AIR TIME`, `DIS TANCE`, nfactors = 2, fm = "minchi", scores = "regression", rotate = "Promax", max_nrow = 50000)
  check_output(model_df)

  # Skipping for now since this gives "Error in `solve.default(U)`: system is computationally singular: reciprocal condition number = 1.32945e-43".
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, nfactors=2, fm="minrank", rotate="promax")
  # check_output(model_df)

  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="alpha") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
})

test_that("exp_factanal with strange column name and all-NA column", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10))) %>%
    mutate(na_col = NA)
  model_df <- exp_factanal(df, `Cy l`, mpg, hp, na_col)
  res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  expect_equal(colnames(res),
               c("Factors", "Variance Explained (Ratio)", "Variance Explained", "Chi-Square", "P Value", "DF", "Rows", "Method", "Rotation", "RMSR", "RMSEA", "TLI", "BIC"))
  res <- model_df %>% tidy_rowwise(model, type="variances")
  expect_equal(colnames(res),
               c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion", "Factor", "% Variance", "Cummulated % Variance"))
  res <- model_df %>% tidy_rowwise(model, type="loadings")
  expect_equal(colnames(res),
               c("variable", "factor", "value"))
  res <- model_df %>% tidy_rowwise(model, type="biplot")
  expect_equal(colnames(res),
               c("mpg", "Cy l", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_col", ".factor_1", ".factor_2", ".variable", ".factor_2_variable"))
  res <- model_df %>% tidy_rowwise(model, type="screeplot")
  expect_equal(colnames(res),
               c("factor", "eigenvalue"))
  res <- model_df %>% tidy_rowwise(model, type="data")
  expect_equal(colnames(res),
               c("mpg","Cy l","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","Factor 1","Factor 2"))
  # New tidy types must also survive a strange column name + all-NA column (issue #37018).
  res <- model_df %>% tidy_rowwise(model, type="suitability")
  expect_equal(colnames(res), c("Metric", "Value", "Judgement", "Description", "status"))
  res <- model_df %>% tidy_rowwise(model, type="loadings_wide")
  expect_true(all(c("variable", "Judgement", "judgement_status") %in% colnames(res)))
  res <- model_df %>% tidy_rowwise(model, type="communalities")
  expect_equal(colnames(res), c("variable", "Communality", "Uniqueness", "Judgement", "judgement_status"))
})

test_that("factor analysis report judgment helpers (issue #37018)", {
  cfg <- factanal_report_config()

  # KMO thresholds + NA handling. Labels are English-canonical; status is the language-neutral token.
  expect_equal(judge_kmo(0.85)$status, "great")
  expect_equal(judge_kmo(0.75)$status, "good")
  expect_equal(judge_kmo(0.65)$status, "min")
  expect_equal(judge_kmo(0.55)$status, "poor")
  expect_equal(judge_kmo(0.40)$status, "below")
  expect_equal(judge_kmo(NA_real_)$status, "na")
  expect_equal(judge_kmo(0.85)$label, "Very Suitable")

  # Bartlett p-value.
  expect_equal(judge_bartlett(0.001)$status, "suitable")
  expect_equal(judge_bartlett(0.20)$status, "caution")
  expect_equal(judge_bartlett(NA_real_)$status, "na")

  # Communality thresholds. A communality > 1 (Heywood case) is flagged before "too high".
  expect_equal(judge_communality(1.05)$status, "improper")
  expect_equal(judge_communality(1.0)$status, "too_high")
  expect_equal(judge_communality(0.97)$status, "too_high")
  expect_equal(judge_communality(0.70)$status, "good")
  expect_equal(judge_communality(0.50)$status, "moderate")
  expect_equal(judge_communality(0.30)$status, "weak")
  expect_equal(judge_communality(NA_real_)$status, "na")

  # Loading judgment: strong, negative direction, hard-to-interpret, cross-loading.
  strong <- judge_loading(c(`Factor 1` = 0.75, `Factor 2` = 0.10))
  expect_equal(strong$status, "strong")
  expect_equal(strong$primary_factor, "Factor 1")
  expect_equal(strong$direction, "positive")
  expect_equal(strong$label, "Strongly related to Factor 1")

  strong_neg <- judge_loading(c(`Factor 1` = 0.10, `Factor 2` = -0.75))
  expect_equal(strong_neg$status, "strong")
  expect_equal(strong_neg$direction, "negative")
  expect_equal(strong_neg$label, "Strongly related to Factor 2 (negative)")

  low <- judge_loading(c(`Factor 1` = 0.20, `Factor 2` = 0.15))
  expect_equal(low$status, "low_loading")
  expect_equal(low$label, "Hard to interpret")

  ambiguous <- judge_loading(c(`Factor 1` = 0.55, `Factor 2` = 0.50))
  expect_equal(ambiguous$status, "ambiguous_crossload")

  crossload <- judge_loading(c(`Factor 1` = 0.70, `Factor 2` = 0.45))
  expect_equal(crossload$status, "crossload")
  expect_equal(crossload$primary_factor, "Factor 1")
  expect_equal(crossload$secondary_factors, "Factor 2")

  moderate <- judge_loading(c(`Factor 1` = 0.45, `Factor 2` = 0.05))
  expect_equal(moderate$status, "moderate")
  one_factor <- judge_loading(c(`Factor 1` = 0.75))
  expect_equal(one_factor$status, "strong")
  expect_equal(one_factor$secondary_factors, "")
  expect_equal(judge_loading(c(`Factor 1` = NA_real_, `Factor 2` = NA_real_))$status, "na")
  old_fa <- structure(list(), class = "fa_exploratory")
  expect_equal(tidy.fa_exploratory(old_fa, type = "suitability")$Value,
               c("N/A", "N/A", "N/A", "N/A"))

  # Communality bar (#37018): a Heywood case (communality > 1) leaves communality UNCAPPED so the
  # numeric label shows the actual value (e.g. 105); the chart's 0-100 value-axis range clips the
  # bar at 100. Uniqueness is clamped to 0 (never negative). Variable names stay clean (no marker).
  fake_fa <- list(communality = c(A = 0.70, B = 1.05, C = 0.30))
  class(fake_fa) <- "fa_exploratory"
  clong <- tidy.fa_exploratory(fake_fa, type = "communalities_long")
  expect_equal(colnames(clong), c("variable", "Component", "Ratio"))
  bwide <- tidyr::pivot_wider(clong, names_from = Component, values_from = Ratio)
  # Ratios are on a 0-100 percentage scale. Heywood variable (B): uncapped communality (105) so the
  # label shows the actual value; uniqueness clamped to 0.
  expect_equal(bwide$Communality[as.character(bwide$variable) == "B"], 105)
  expect_equal(bwide$Uniqueness[as.character(bwide$variable) == "B"], 0)
  # Normal variable unchanged.
  expect_equal(bwide$Communality[as.character(bwide$variable) == "A"], 70)
  # No warning marker appended to any variable name.
  expect_false(any(grepl("⚠", as.character(bwide$variable))))
  # Component is Communality-first (stack/color/legend order).
  expect_equal(levels(clong$Component), c("Communality", "Uniqueness"))
  # Variables ordered by communality DESCENDING: the highest (Heywood B) is the first level.
  expect_equal(levels(clong$variable)[1], "B")

  # Parallel analysis returns a recommended count and per-factor threshold table, deterministically.
  set.seed(1)
  pa <- compute_parallel_analysis(mtcars[, c("mpg","cyl","disp","hp","drat","wt","qsec")], n_iter = 20)
  expect_true(is.numeric(pa$recommended_n))
  expect_equal(colnames(pa$table), c("factor_number", "actual_eigenvalue", "random_eigenvalue_threshold"))
  set.seed(99)
  before <- .Random.seed
  compute_parallel_analysis(mtcars[, 1:3], n_iter = 2)
  expect_equal(.Random.seed, before)
  set.seed(99)
  before <- .Random.seed
  expect_error(compute_parallel_analysis(mtcars[, 1:3], n_iter = 0), "positive integer")
  expect_equal(.Random.seed, before)
})
