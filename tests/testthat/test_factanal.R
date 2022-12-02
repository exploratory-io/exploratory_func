# how to run this test:
# devtools::test(filter="factanal")
context("test factor analysis function, exp_factanal")

test_that("exp_factanal with default orthogonal varimax rotation", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  check_output <- function(model_df) {
    res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
    expect_equal(colnames(res),
                 c("Number of Factors", "Explained Variance (%)", "Explained Variance", "Chi-Square", "P Value", "Degree of Freedom", "Number of Rows"))
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
                 c("Number of Factors", "Explained Variance (%)", "Explained Variance", "Chi-Square", "P Value", "Degree of Freedom", "Number of Rows"))
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
               c("Number of Factors", "Explained Variance (%)", "Explained Variance", "Chi-Square", "P Value", "Degree of Freedom", "Number of Rows"))
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
})
