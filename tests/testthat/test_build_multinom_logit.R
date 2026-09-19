context("test build_multinom_logit")

# Canonical column-escaping stress-test name (tam workflow rule 7).
COMPLEX_NAME <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"

# NPS-like 3-class outcome whose drivers are deliberately ASYMMETRIC: `x1` pushes
# toward Promoter and away from Detractor, `plan == Premium` only toward Promoter.
make_multinom_test_df <- function(n = 600, seed = 1) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  plan <- sample(c("Basic", "Standard", "Premium"), n, replace = TRUE)
  eta_promoter <- 0.8 * x1 + (plan == "Premium") * 1
  eta_detractor <- -0.8 * x1
  p <- cbind(1, exp(eta_promoter), exp(eta_detractor))
  p <- p / rowSums(p)
  nps <- apply(p, 1, function(pr) sample(c("Passive", "Promoter", "Detractor"), 1, prob = pr))
  df <- data.frame(nps = nps, x1 = x1, plan = plan, w = stats::runif(n, 0.5, 2), stringsAsFactors = FALSE)
  df[[COMPLEX_NAME]] <- df$nps
  df$`部署 名!#` <- df$plan
  df
}

test_that("default reference category is the most frequent category", {
  df <- make_multinom_test_df()
  most_frequent <- names(sort(table(df$nps), decreasing = TRUE))[[1]]
  model_df <- df %>% build_multinom_logit(nps, x1, plan)
  expect_equal(model_df$model[[1]]$reference_category, most_frequent)
  coef_df <- model_df %>% tidy_rowwise(model)
  expect_setequal(unique(as.character(coef_df$category)), setdiff(unique(df$nps), most_frequent))
})

test_that("reference category ties resolve to the first level", {
  expect_equal(resolve_multinom_reference_category(factor(c("b", "a", "a", "b", "c"))), "a")
  expect_equal(resolve_multinom_reference_category(factor(c("b", "a")), ""), "a")
})

test_that("explicit reference category is honored and coefficients match nnet::multinom directly", {
  df <- make_multinom_test_df()
  model_df <- df %>% build_multinom_logit(nps, x1, plan, reference_category = "Passive")
  coef_df <- model_df %>% tidy_rowwise(model)

  # Independent reference: fit nnet::multinom ourselves with the same reference
  # and predictor coding, and compare estimates and Wald standard errors.
  ref_df <- df
  ref_df$nps <- stats::relevel(factor(ref_df$nps), ref = "Passive")
  ref_df$plan <- forcats::fct_infreq(ref_df$plan)
  ref_fit <- nnet::multinom(nps ~ x1 + plan, data = ref_df, trace = FALSE, Hess = TRUE, maxit = 200)
  ref_coef <- stats::coef(ref_fit)
  ref_se <- summary(ref_fit)$standard.errors

  promoter_x1 <- coef_df %>% dplyr::filter(category == "Promoter", term == "x1")
  expect_equal(promoter_x1$estimate, unname(ref_coef["Promoter", "x1"]), tolerance = 1e-4)
  expect_equal(promoter_x1$std.error, unname(ref_se["Promoter", "x1"]), tolerance = 1e-4)
  expect_equal(promoter_x1$odds.ratio, exp(promoter_x1$estimate))
  # Asymmetric drivers are recovered with the right signs.
  expect_gt(promoter_x1$estimate, 0)
  detractor_x1 <- coef_df %>% dplyr::filter(category == "Detractor", term == "x1")
  expect_lt(detractor_x1$estimate, 0)
  # Categorical predictor terms are prettified and carry their base level.
  plan_terms <- coef_df %>% dplyr::filter(startsWith(term, "plan: "))
  expect_true(nrow(plan_terms) == 4)
  expect_true(all(!is.na(plan_terms$base.level)))
})

test_that("unknown reference category lists the valid categories", {
  df <- make_multinom_test_df()
  expect_error(df %>% build_multinom_logit(nps, x1, reference_category = "Nope"),
               "Choose one of: Detractor, Passive, Promoter")
})

test_that("numeric, 2-level and >20-level targets are rejected", {
  df <- make_multinom_test_df()
  expect_error(df %>% build_multinom_logit(x1, plan), "must be a categorical column")
  df2 <- df %>% dplyr::filter(nps != "Detractor")
  expect_error(df2 %>% build_multinom_logit(nps, x1), "3 or more categories")
  df3 <- df
  df3$many <- paste0("c", seq_len(nrow(df3)) %% 21)
  expect_error(df3 %>% build_multinom_logit(many, x1), "at most 20 categories")
})

test_that("complex column names work as target and predictor, with weight, NA and test split", {
  df <- make_multinom_test_df()
  df[[COMPLEX_NAME]][c(2, 5)] <- NA
  df$x1[7] <- NA
  model_df <- df %>% build_multinom_logit(!!rlang::sym(COMPLEX_NAME), x1, `部署 名!#`,
                                         weight = w, reference_category = "Detractor", test_rate = 0.3)
  model <- model_df$model[[1]]
  expect_equal(model$reference_category, "Detractor")

  coef_df <- model_df %>% tidy_rowwise(model, pretty.name = TRUE)
  expect_true(all(c("Category", "Term", "Odds Ratio", "P Value", "Base Level", "Reference Category") %in% colnames(coef_df)))
  expect_true(any(startsWith(coef_df$Term, "部署 名!#: ")))
  # Terms are shown as plain column names, never backtick-quoted.
  numeric_df <- df
  numeric_df$`満足度 (1-5)` <- round(stats::runif(nrow(df), 1, 5))
  numeric_terms <- (numeric_df %>% build_multinom_logit(nps, `満足度 (1-5)`, x1) %>% tidy_rowwise(model))$term
  expect_true("満足度 (1-5)" %in% numeric_terms)
  expect_false(any(grepl("`", numeric_terms, fixed = TRUE)))

  eval_df <- model_df %>% evaluate_multinom_logit(data = "training_and_test")
  expect_equal(eval_df$`Data Type`, c("Training", "Test"))
  expect_equal(sum(eval_df$Rows), nrow(df) - 3)
  expect_true(all(eval_df$`Accuracy Rate` > 0.4))

  aug <- model_df %>% augment_rowwise(model)
  expect_true(all(paste0("predicted_probability_", c("Detractor", "Passive", "Promoter")) %in% colnames(aug)))
  expect_false("(weights)" %in% colnames(aug))
  prob_sums <- rowSums(aug[, paste0("predicted_probability_", c("Detractor", "Passive", "Promoter"))])
  expect_equal(unname(prob_sums), rep(1, nrow(aug)), tolerance = 1e-8)
})

test_that("glance reports fit statistics and convergence", {
  df <- make_multinom_test_df()
  g <- df %>% build_multinom_logit(nps, x1, plan) %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(g$`Number of Categories`, 3)
  expect_true(g$Converged)
  expect_true(g$`Null Deviance` > g$`Residual Deviance`)
  expect_true(g$`McFadden R-Squared` > 0 && g$`McFadden R-Squared` < 1)
})

test_that("vif, importance, partial dependence and report helpers return data", {
  df <- make_multinom_test_df()
  model_df <- df %>% build_multinom_logit(nps, x1, `部署 名!#`)
  vif_df <- model_df %>% tidy_rowwise(model, type = "vif")
  expect_setequal(vif_df$term, c("x1", "部署 名!#"))
  imp_df <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_setequal(imp_df$variable, c("x1", "部署 名!#"))
  expect_true(all(!is.na(imp_df$p.value)))
  pd_df <- model_df %>% tidy_rowwise(model, type = "partial_dependence")
  expect_setequal(unique(pd_df$y_name), c("Detractor", "Passive", "Promoter"))
  expect_setequal(unique(pd_df$x_name), c("x1", "部署 名!#"))

  info <- multinom_logit_report_basic_info(model_df)
  expect_equal(info$`Reference Category`, model_df$model[[1]]$reference_category)
  expect_equal(info$Categories, 3)
  probs <- multinom_logit_report_multiclass_probabilities(model_df)
  expect_equal(nrow(probs), 3 * nrow(df))
})

test_that("importance p-values match exact predictor terms", {
  set.seed(22)
  n <- 800
  x <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  eta <- 2.2 * x2
  p <- cbind(exp(eta), 1, 1)
  p <- p / rowSums(p)
  y <- apply(p, 1, function(pr) sample(c("A", "B", "C"), 1, prob = pr))
  df <- data.frame(y = y, x = x, x2 = x2)

  model_df <- df %>% build_multinom_logit(y, x, x2, reference_category = "C")
  coef_df <- model_df %>% tidy_rowwise(model, conf.int = FALSE, exponentiate = FALSE)
  imp_df <- model_df %>% tidy_rowwise(model, type = "importance")

  expected_x_p <- min(coef_df$p.value[coef_df$term == "x"])
  actual_x_p <- imp_df$p.value[imp_df$variable == "x"]
  expect_equal(actual_x_p, expected_x_p)
  expect_gt(actual_x_p, 0.1)
  expect_lt(imp_df$p.value[imp_df$variable == "x2"], 1e-10)
})

test_that("ordered test splits reject target categories absent from training", {
  df <- data.frame(
    y = c(rep("A", 30), rep("B", 30), rep("C", 30), rep("D", 10)),
    x = seq_len(100)
  )

  expect_error(
    df %>% build_multinom_logit(
      y, x,
      reference_category = "A",
      test_rate = 0.1,
      test_split_type = "ordered"
    ),
    "[Tt]arget categories.*training data"
  )
})

test_that("perfectly collinear predictors surface a collinearity message instead of failing", {
  df <- make_multinom_test_df()
  df$x2 <- df$x1 * 2
  model_df <- df %>% build_multinom_logit(nps, x1, x2)
  expect_true(inherits(model_df$model[[1]]$vif, "error"))
  expect_match(conditionMessage(model_df$model[[1]]$vif), "perfect collinearity")
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "vif")), 0)
})

test_that("Repeat By fits one model per group against the same reference", {
  df <- make_multinom_test_df()
  df$grp <- rep(c("A", "B"), length.out = nrow(df))
  model_df <- df %>% dplyr::group_by(grp) %>% build_multinom_logit(nps, x1, plan, group_cols = "grp")
  expect_equal(nrow(model_df), 2)
  refs <- vapply(model_df$model, function(m) m$reference_category, character(1))
  expect_equal(length(unique(refs)), 1)
  coef_df <- model_df %>% tidy_rowwise(model)
  expect_setequal(unique(coef_df$grp), c("A", "B"))
})

test_that("importance p-values are found for names R backtick-quotes because of non-ASCII punctuation", {
  # A Japanese comma makes the name non-syntactic, so R quotes the model term
  # (`...、...`) even though the name has no ASCII symbol.
  set.seed(7)
  n <- 600
  df <- data.frame(
    y = sample(c("A", "B", "C"), n, replace = TRUE),
    check.names = FALSE
  )
  df[["同じような商品であれば、価格が安い方を選ぶ"]] <- sample(1:5, n, TRUE)
  df[["満足度"]] <- sample(1:5, n, TRUE)
  df[["地域、区分"]] <- sample(c("東", "西", "南"), n, TRUE)
  model_df <- df %>% build_multinom_logit(y, `同じような商品であれば、価格が安い方を選ぶ`, `満足度`, `地域、区分`)
  coef_df <- model_df %>% tidy_rowwise(model, conf.int = FALSE, exponentiate = FALSE)
  imp_df <- model_df %>% tidy_rowwise(model, type = "importance")
  for (var in c("同じような商品であれば、価格が安い方を選ぶ", "満足度", "地域、区分")) {
    expected <- min(coef_df$p.value[coef_df$term == var | startsWith(coef_df$term, paste0(var, ": "))])
    expect_equal(imp_df$p.value[imp_df$variable == var], expected, info = var)
  }
})

test_that("importance p-values are found for logical predictors, whose term ends in TRUE", {
  set.seed(11)
  n <- 800
  flag <- stats::runif(n) < 0.5
  x <- stats::rnorm(n)
  p <- cbind(1, exp(1.2 * flag), exp(0.3 * x))
  p <- p / rowSums(p)
  df <- data.frame(y = apply(p, 1, function(pr) sample(c("A", "B", "C"), 1, prob = pr)), x = x)
  df[["アプリ利用"]] <- flag
  model_df <- df %>% build_multinom_logit(y, `アプリ利用`, x, reference_category = "A")
  coef_df <- model_df %>% tidy_rowwise(model, conf.int = FALSE, exponentiate = FALSE)
  imp_df <- model_df %>% tidy_rowwise(model, type = "importance")
  expected <- min(coef_df$p.value[coef_df$term == "アプリ利用TRUE"])
  expect_false(is.na(expected))
  expect_equal(imp_df$p.value[imp_df$variable == "アプリ利用"], expected)
})
