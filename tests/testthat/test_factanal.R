# how to run this test:
# devtools::test(filter="factanal")
context("test factor analysis function, exp_factanal")

# Shared by the tests below (issue tam#30432). The factor score coefficients ARE the weights that
# turn the standardized variables into the factor scores that type="data" reports, so reconstructing
# the scores from them is the real correctness check -- and it simultaneously pins the factor column
# ORDER: if x$weights' columns were in a different order than the "Factor N" IDs every other tidy
# type emits, the reconstruction would come out scrambled rather than merely mislabeled.
check_score_coefficients <- function(model_df) {
  coefs <- model_df %>% tidy_rowwise(model, type="score_coefficients")
  dat <- model_df %>% tidy_rowwise(model, type="data")
  factor_cols <- setdiff(colnames(coefs), "variable")
  expect_gt(length(factor_cols), 0)
  expect_true(all(factor_cols %in% colnames(dat)))
  expect_true(all(coefs$variable %in% colnames(dat)))
  reconstructed <- scale(as.matrix(dat[, coefs$variable])) %*% as.matrix(coefs[, factor_cols])
  expect_equal(unname(reconstructed), unname(as.matrix(dat[, factor_cols])), tolerance = 1e-6)
}

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
    expect_equal(res$Metric, c("KMO", "Bartlett's Test of Sphericity (P Value)", "Rows Used", "Variables Used")) # #37340
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
    # Factor score coefficients (issue tam#30432).
    res <- model_df %>% tidy_rowwise(model, type="score_coefficients")
    expect_equal(colnames(res), c("variable", "Factor 1", "Factor 2", "Factor 3"))
    expect_setequal(res$variable, c("cyl", "mpg", "hp"))
    check_score_coefficients(model_df)
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
    # Factor score coefficients under an oblique rotation, where psych reorders the factors
    # (e.g. MR1, MR3, MR2), and with column names containing spaces (issue tam#30432).
    res <- model_df %>% tidy_rowwise(model, type="score_coefficients")
    expect_equal(colnames(res), c("variable", "Factor 1", "Factor 2"))
    expect_setequal(res$variable, c("DAY OF MONTH", "FL NUM", "DEP DELAY", "ARR DELAY", "AIR TIME", "DIS TANCE"))
    check_score_coefficients(model_df)
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
  # The all-NA column is dropped before fitting, so it must not appear as a coefficient row either.
  res <- model_df %>% tidy_rowwise(model, type="score_coefficients")
  expect_equal(colnames(res), c("variable", "Factor 1", "Factor 2"))
  expect_setequal(res$variable, c("Cy l", "mpg", "hp"))
  check_score_coefficients(model_df)
})

test_that("exp_factanal with nfactors=1 (issue #30798)", {
  # Biplot plots Factor 1 vs Factor 2, so it is the only tidy type that structurally cannot
  # support a single factor. It must degrade gracefully to an empty result with the same column
  # shape the client expects, instead of erroring on a missing "Factor 2" column. Every other
  # tidy type must keep working normally with nfactors=1 -- exercise every type used elsewhere in
  # this test file to guard against a similar nfactors>=2 assumption lurking anywhere else.
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_factanal(df, cyl, mpg, hp, drat, max_nrow=30, nfactors=1, fm="minres")

  res <- model_df %>% tidy_rowwise(model, type="biplot")
  expect_equal(nrow(res), 0)
  expect_true(all(c(".factor_1", ".factor_2", ".factor_2_variable") %in% colnames(res)))
  expect_true(is.numeric(res$.factor_1))
  expect_true(is.numeric(res$.factor_2))
  expect_true(is.numeric(res$.factor_2_variable))

  # "variances" needs its own value-level check: psych::fa()'s Vaccounted matrix omits
  # "Cumulative Var" / "Proportion Explained" / "Cumulative Proportion" when nfactors=1 (they are
  # trivial for a single factor), and factanal.R backfills them. For one factor: cumulative
  # variance up through "the only factor" equals that factor's own proportion, and that one
  # factor explains 100% of whatever is explained.
  variances_res <- model_df %>% tidy_rowwise(model, type="variances")
  expect_equal(nrow(variances_res), 1)
  expect_equal(variances_res$`Cumulative Var`, variances_res$`Proportion Var`)
  expect_equal(variances_res$`Proportion Explained`, 1)
  expect_equal(variances_res$`Cumulative Proportion`, 1)
  expect_equal(variances_res$`Cummulated % Variance`, variances_res$`% Variance`)

  # Every other tidy/glance type must run cleanly with nfactors=1 (no other nfactors>=2
  # assumption should exist elsewhere in this file besides biplot/variances).
  expect_no_error(model_df %>% glance_rowwise(model, pretty.name=TRUE))
  expect_no_error(model_df %>% tidy_rowwise(model, type="loadings"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="correlation"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="screeplot"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="data"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="suitability"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="factor_count"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="parallel_screeplot"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="loadings_wide"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="communalities"))
  expect_no_error(model_df %>% tidy_rowwise(model, type="communalities_long"))
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
  expect_equal(colnames(pa$table), c("factor_number", "actual_eigenvalue", "random_eigenvalue_threshold", "retained"))
  # method defaults to "factor_model" (issue tam#37332).
  expect_equal(pa$method, "factor_model")
  expect_equal(pa$factor_extraction_method, "minres")
  expect_equal(pa$quantile_prob, 0.95)
  expect_equal(pa$table$retained, seq_len(nrow(pa$table)) <= pa$recommended_n)
  set.seed(99)
  before <- .Random.seed
  compute_parallel_analysis(mtcars[, 1:3], n_iter = 2)
  expect_equal(.Random.seed, before)
  set.seed(99)
  before <- .Random.seed
  expect_error(compute_parallel_analysis(mtcars[, 1:3], n_iter = 0), "positive integer")
  expect_equal(.Random.seed, before)
})

test_that("parallel analysis method: factor_model vs smc (issue #37332)", {
  df <- mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")]

  # compute_parallel_factor_eigenvalues: both methods return one eigenvalue per variable, and the
  # two methods must disagree on a real correlation matrix (smc is a strictly reduced-diagonal
  # matrix vs a one-factor communality estimate -- they are not expected to coincide).
  cor_mat <- stats::cor(df)
  eig_factor_model <- compute_parallel_factor_eigenvalues(cor_mat, method = "factor_model", fm = "minres", n_obs = nrow(df))
  eig_smc <- compute_parallel_factor_eigenvalues(cor_mat, method = "smc")
  expect_equal(length(eig_factor_model), ncol(df))
  expect_equal(length(eig_smc), ncol(df))
  expect_false(isTRUE(all.equal(eig_factor_model, eig_smc)))
  # method must be one of the two allowed values.
  expect_error(compute_parallel_factor_eigenvalues(cor_mat, method = "bogus"))
  # A non-square matrix is rejected before any estimation is attempted.
  expect_error(compute_parallel_factor_eigenvalues(cor_mat[, 1:3], method = "smc"), "square matrix")
  expect_error(compute_parallel_factor_eigenvalues(matrix(c(1, NA, NA, 1), 2, 2), method = "smc"), "non-finite")

  # compute_parallel_recommended_n: counts only the LEADING run of TRUE, not a simple sum.
  expect_equal(compute_parallel_recommended_n(c(2, 2, 0.5, 2), c(1, 1, 1, 1)), 2)
  expect_equal(compute_parallel_recommended_n(c(2, 2, 2, 2), c(1, 1, 1, 1)), 4)
  expect_equal(compute_parallel_recommended_n(c(0.5, 2, 2, 2), c(1, 1, 1, 1)), 0)

  # compute_parallel_analysis dispatches on method and applies it to BOTH the actual data and the
  # random-data null distribution -- confirmed by the two methods producing different results on
  # the same data/seed.
  set.seed(7)
  pa_factor_model <- compute_parallel_analysis(df, n_iter = 15, method = "factor_model", fm = "minres")
  set.seed(7)
  pa_smc <- compute_parallel_analysis(df, n_iter = 15, method = "smc")
  expect_equal(pa_factor_model$method, "factor_model")
  expect_equal(pa_smc$method, "smc")
  expect_false(isTRUE(all.equal(pa_factor_model$table$actual_eigenvalue, pa_smc$table$actual_eigenvalue)))
  expect_false(isTRUE(all.equal(pa_factor_model$table$random_eigenvalue_threshold, pa_smc$table$random_eigenvalue_threshold)))

  # exp_factanal(): parallel_method defaults to factor_model, is forwarded to compute_parallel_analysis,
  # is stored on the fit even when parallel itself is unavailable, and match.arg rejects bogus values.
  model_df <- exp_factanal(df, mpg, cyl, disp, hp, drat, wt, qsec, nfactors = 2, fm = "minres", parallel_n_iter = 10)
  fit <- model_df$model[[1]]
  expect_equal(fit$parallel_method, "factor_model")
  expect_equal(fit$parallel_factor_extraction_method, "minres")
  expect_equal(fit$parallel$method, "factor_model")

  smc_model_df <- exp_factanal(df, mpg, cyl, disp, hp, drat, wt, qsec, nfactors = 2, fm = "minres",
                               parallel_n_iter = 10, parallel_method = "smc")
  smc_fit <- smc_model_df$model[[1]]
  expect_equal(smc_fit$parallel_method, "smc")
  expect_equal(smc_fit$parallel$method, "smc")

  expect_error(exp_factanal(df, mpg, cyl, disp, hp, drat, wt, qsec, nfactors = 2, parallel_method = "bogus"))

  # parallel_screeplot: the actual-data curve must be the SAME eigenvalues the parallel analysis
  # itself used (method-aware), not a fresh plain correlation-matrix eigen() -- so it must equal
  # fit$parallel$table$actual_eigenvalue and must NOT equal the plain screeplot's eigenvalues, since
  # factor-model/SMC eigenvalues differ from PCA-style correlation-matrix eigenvalues.
  screeplot_res <- model_df %>% tidy_rowwise(model, type = "screeplot")
  parallel_screeplot_res <- model_df %>% tidy_rowwise(model, type = "parallel_screeplot")
  expect_equal(parallel_screeplot_res$Eigenvalue, fit$parallel$table$actual_eigenvalue)
  expect_false(isTRUE(all.equal(parallel_screeplot_res$Eigenvalue, screeplot_res$eigenvalue)))
  # The normal (non-parallel) scree plot is untouched by the method selection (issue #37332 section 10).
  expect_equal(screeplot_res$eigenvalue, eigen(fit$correlation, symmetric = TRUE, only.values = TRUE)$values)

  # factor_count description names the method actually used.
  fc_factor_model <- model_df %>% tidy_rowwise(model, type = "factor_count")
  expect_equal(fc_factor_model$Description[[2]], "Number of factors whose factor-model eigenvalue exceeds the random-data threshold.")
  fc_smc <- smc_model_df %>% tidy_rowwise(model, type = "factor_count")
  expect_equal(fc_smc$Description[[2]], "Number of factors whose SMC-based factor eigenvalue exceeds the random-data threshold.")

  # analysis_method table carries the new row. Read BY ITEM NAME: #37340 moved the two data counts
  # to the top of the table, so the row's position is no longer 4.
  am_value <- function(tbl, item) tbl$Value[[which(tbl$Item == item)]]
  am_factor_model <- model_df %>% tidy_rowwise(model, type = "analysis_method")
  expect_true("Parallel Analysis Method" %in% am_factor_model$Item)
  expect_equal(am_value(am_factor_model, "Parallel Analysis Method"), "Factor Model")
  am_smc <- smc_model_df %>% tidy_rowwise(model, type = "analysis_method")
  expect_equal(am_value(am_smc, "Parallel Analysis Method"), "Diagonal SMC")

  # factanal_parallel_method_label: NULL degrades to the factor_model default; unknown -> Not Available.
  expect_equal(factanal_parallel_method_label(NULL), "Factor Model")
  expect_equal(factanal_parallel_method_label("factor_model"), "Factor Model")
  expect_equal(factanal_parallel_method_label("smc"), "Diagonal SMC")
  expect_equal(factanal_parallel_method_label("something_else"), "Not Available")

  # A model saved before issue #37332 has no parallel_method field at all; the report must still
  # show a sensible default rather than erroring or showing NA/blank.
  legacy_fit <- fit
  legacy_fit$parallel_method <- NULL
  legacy_am <- tidy(legacy_fit, type = "analysis_method")
  # #37340 moved the two data counts to the top, so read the row BY NAME rather than by position.
  expect_equal(legacy_am$Value[[which(legacy_am$Item == "Parallel Analysis Method")]], "Factor Model")
})

test_that("report part 3: variances_judged, suitability P value format, analysis_method order (issue tam#37340)", {
  model_df <- mtcars %>%
    exp_factanal(mpg, cyl, disp, hp, drat, wt, qsec, nfactors = 2, fm = "minres",
                 rotate = "varimax", cor_type = "pearson", parallel_n_iter = 5)
  fit <- model_df$model[[1]]

  # --- variances_judged: one row per correlation-matrix eigenvalue (per VARIABLE, like PCA's PCn),
  # so every candidate factor is judged -- not just the extracted ones.
  judged <- tidy(fit, type = "variances_judged")
  expect_equal(colnames(judged),
               c("Factor", "Eigenvalue", "% Variance", "Cummulated % Variance",
                 "Parallel Analysis", "Kaiser Criterion", "Adoption",
                 "parallel_status", "kaiser_status", "selected_status"))
  n_var <- length(fit$communality)
  expect_equal(nrow(judged), n_var)
  expect_equal(judged$Factor, as.character(seq_len(n_var)))
  # Eigenvalues come off eigen(x$correlation) -- the SAME basis as screeplot / parallel_screeplot,
  # descending, and the ratios are eigenvalue shares summing to 100%.
  expect_equal(judged$Eigenvalue, eigen(fit$correlation, symmetric = TRUE, only.values = TRUE)$values)
  expect_equal(sum(judged$`% Variance`), 100)
  expect_equal(judged$`Cummulated % Variance`, cumsum(judged$`% Variance`))
  expect_equal(judged$`Cummulated % Variance`[[n_var]], 100)
  # Kaiser mirrors the factor_count branch's kaiser_n (eigenvalue > 1) and is ALWAYS judged for
  # factor analysis (always a correlation matrix) -- never "na" the way covariance-scaled PCA is.
  expect_equal(sum(judged$kaiser_status == "adopted"), sum(judged$Eigenvalue > 1))
  expect_false(any(judged$kaiser_status == "na"))
  # Selected = the factors this analysis actually extracted (nfactors), first rows only.
  expect_equal(judged$selected_status, ifelse(seq_len(n_var) <= 2, "adopted", "not_adopted"))
  expect_equal(judged$Adoption, ifelse(seq_len(n_var) <= 2, "Adopted", "Not Adopted"))
  # Labels stay English-canonical; the client translates them. "Adopted" (not PCA's "Adopt") so the
  # English report reads as a judgment against "Not Adopted".
  expect_true(all(judged$`Parallel Analysis` %in% c("Adopted", "Not Adopted", "Not Available")))
  expect_true(all(judged$`Kaiser Criterion` %in% c("Adopted", "Not Adopted")))
  expect_true(all(judged$parallel_status %in% c("adopted", "not_adopted", "na")))
  # Parallel analysis unavailable (old saved model) degrades to Not Available / na, same shape.
  no_par <- fit
  no_par$parallel <- NULL
  judged_no_par <- tidy(no_par, type = "variances_judged")
  expect_equal(unique(judged_no_par$`Parallel Analysis`), "Not Available")
  expect_equal(unique(judged_no_par$parallel_status), "na")
  expect_equal(nrow(judged_no_par), n_var)

  # --- analysis_method: counts first, then the method rows (#37340).
  method_tbl <- tidy(fit, type = "analysis_method")
  expect_equal(method_tbl$Item, c("Number of Variables", "Row Count", "Correlation",
                                  "Factor Extraction Method", "Rotation", "Parallel Analysis Method"))
  expect_equal(method_tbl$Value[[1]], as.character(n_var))
  expect_equal(method_tbl$Value[[2]], as.character(nrow(mtcars)))
  expect_equal(method_tbl$Value[[3]], "Pearson Correlation")

  # --- suitability: Metric names the P value, the Value cell holds the value ONLY, and the
  # small-p threshold is 0.0001 (was 0.001).
  suit <- tidy(fit, type = "suitability")
  expect_equal(suit$Metric[[2]], "Bartlett's Test of Sphericity (P Value)")
  expect_false(grepl("p", suit$Value[[2]], fixed = TRUE)) # no "p < " / "p = " prefix
  expect_true(grepl("^(< 0\\.0001|[0-9]+\\.[0-9]{4}|N/A)$", suit$Value[[2]]))
  fake <- fit
  fake$bartlett <- list(p.value = 1e-9)
  expect_equal(tidy(fake, type = "suitability")$Value[[2]], "< 0.0001")
  fake$bartlett <- list(p.value = 0.00005)
  expect_equal(tidy(fake, type = "suitability")$Value[[2]], "< 0.0001")
  # Just ABOVE the threshold: 4 fixed decimals, never scientific notation ("2e-04").
  fake$bartlett <- list(p.value = 0.0002)
  expect_equal(tidy(fake, type = "suitability")$Value[[2]], "0.0002")
  fake$bartlett <- list(p.value = 0.03)
  expect_equal(tidy(fake, type = "suitability")$Value[[2]], "0.0300")
  fake$bartlett <- NULL
  expect_equal(tidy(fake, type = "suitability")$Value[[2]], "N/A")
})

test_that("report part 3: an over-parameterized fit legitimately has no fit test (issue tam#37340)", {
  # 4 factors on 7 variables drives the model's degrees of freedom NEGATIVE, so psych::fa returns
  # no chi-square P value, no RMSEA and no BIC. The report must present that as "not available"
  # (and must NOT claim a hypothesis-test verdict) -- these blanks are the true output of an
  # over-parameterized fit, not a serialization bug, so glance is intentionally left as-is.
  model_df <- mtcars %>%
    exp_factanal(mpg, cyl, disp, hp, drat, wt, qsec, nfactors = 4, fm = "minres",
                 rotate = "varimax", cor_type = "pearson", parallel_n_iter = 3)
  g <- glance(model_df$model[[1]])
  expect_true(g$DF <= 0)
  expect_true(is.na(g$`P Value`))
})
