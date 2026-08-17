context("test correspondence analysis category order preservation (#37847)")

# Regression coverage for #37847: crosstab pivot/bar chart headers and the
# dimension/category report tables were re-sorting categories alphabetically
# instead of honoring the original Factor level order (or, for a Logical
# column, TRUE before FALSE).

test_that("ca_get_category_levels: factor keeps its own level order", {
  x <- factor(c("Standard", "Free", "Professional", "Enterprise"),
              levels = c("Free", "Standard", "Professional", "Enterprise"))
  expect_equal(ca_get_category_levels(x), c("Free", "Standard", "Professional", "Enterprise"))
})

test_that("ca_get_category_levels: factor drops declared-but-unobserved levels", {
  x <- factor(c("Standard", "Free"),
              levels = c("Free", "Standard", "Professional", "Enterprise"))
  expect_equal(ca_get_category_levels(x), c("Free", "Standard"))
})

test_that("ca_get_category_levels: logical orders TRUE before FALSE regardless of first appearance", {
  # FALSE appears first in the raw data -- the returned order must still be
  # TRUE, FALSE (not first-appearance, not alphabetical -- "FALSE" < "TRUE").
  x <- c(FALSE, FALSE, TRUE, FALSE)
  expect_equal(ca_get_category_levels(x), c("TRUE", "FALSE"))
})

test_that("ca_get_category_levels: logical with only one observed value keeps that single value", {
  expect_equal(ca_get_category_levels(c(FALSE, FALSE)), "FALSE")
  expect_equal(ca_get_category_levels(c(TRUE, TRUE)), "TRUE")
})

test_that("ca_get_category_levels: character/other keeps first-appearance order, not alphabetical", {
  x <- c("Zebra", "Apple", "Zebra", "Mango")
  expect_equal(ca_get_category_levels(x), c("Zebra", "Apple", "Mango"))
})

test_that("ca_get_category_levels: NA values are dropped from the level order", {
  x <- c("b", NA, "a")
  expect_equal(ca_get_category_levels(x), c("b", "a"))
})

# Deliberately non-alphabetical level order so an alphabetical-sort bug is
# always visible: alphabetical would be Enterprise, Free, Professional, Standard.
plan_levels <- c("Free", "Standard", "Professional", "Enterprise")

make_order_data <- function(n = 600, seed = 7) {
  set.seed(seed)
  data.frame(
    plan = factor(sample(plan_levels, n, replace = TRUE, prob = c(.35, .3, .2, .15)),
                  levels = plan_levels),
    churn = sample(c(FALSE, TRUE), n, replace = TRUE, prob = c(.4, .6)),
    resptime = factor(sample(c("1 hour or less", "2-3 days", "4+ days"), n, replace = TRUE),
                       levels = c("1 hour or less", "2-3 days", "4+ days")),
    stringsAsFactors = FALSE
  )
}

test_that("MCA (3+ variables): dimension_matrix and category_details keep the source factor's level order", {
  df <- make_order_data()
  m <- df %>% exp_mca(plan, churn, resptime, ncp = 5)
  model <- m$model[[1]]
  expect_equal(model$analysis_type, "MCA")

  dm_plan <- (m %>% tidy_rowwise(model, type = "dimension_matrix") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(dm_plan, plan_levels)

  cd_plan <- (m %>% tidy_rowwise(model, type = "category_details") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(cd_plan, plan_levels)

  # Logical variable: TRUE before FALSE in both report tables too.
  dm_churn <- (m %>% tidy_rowwise(model, type = "dimension_matrix") %>%
    dplyr::filter(variable == "churn", dimension == 1))$category
  expect_equal(dm_churn, c("TRUE", "FALSE"))
})

test_that("MCA: missing categories retain their prefixed category ids", {
  df <- data.frame(
    plan = factor(c("Free", NA, "Standard", "Professional", "Enterprise"),
                  levels = plan_levels),
    churn = c(FALSE, TRUE, FALSE, TRUE, FALSE),
    resptime = factor(c("1 hour or less", "2-3 days", NA, "4+ days", "2-3 days"),
                      levels = c("1 hour or less", "2-3 days", "4+ days")),
    stringsAsFactors = FALSE
  )
  model <- (df %>% exp_mca(plan, churn, resptime, ncp = 2))$model[[1]]
  lookup <- model$category_lookup

  expect_true(all(grepl("^V[0-9]+:", lookup$category_id)))
  expect_equal(lookup$variable[lookup$category == "NA"], c("plan", "resptime"))
})

test_that("CA (2 variables): dimension_matrix and category_details keep the source factor's level order", {
  df <- make_order_data()
  m <- df %>% exp_mca(plan, resptime, ncp = 5)
  model <- m$model[[1]]
  expect_equal(model$analysis_type, "CA")

  dm_plan <- (m %>% tidy_rowwise(model, type = "dimension_matrix") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(dm_plan, plan_levels)

  cd_plan <- (m %>% tidy_rowwise(model, type = "category_details") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(cd_plan, plan_levels)
})

test_that("residual_cells: row_category/column_category are factors ordered like the source columns", {
  # tam's pivot/bar chart headers group by this column via dplyr::group_by()+
  # arrange(), which sorts a character column alphabetically but respects a
  # factor's level order -- so the value returned to tam MUST be a factor
  # (not a character) with the correct level order, not merely have its raw
  # rows arranged in the correct order.
  df <- make_order_data()
  m <- df %>% exp_mca(plan, churn, resptime, ncp = 5)
  model <- m$model[[1]]

  rc <- m %>% tidy_rowwise(model, type = "residual_cells")
  expect_true(is.factor(rc$row_category))
  expect_true(is.factor(rc$column_category))

  plan_vs_churn <- rc %>% dplyr::filter(pair_id == "plan × churn")
  expect_equal(levels(droplevels(plan_vs_churn$row_category)), plan_levels)
  expect_equal(levels(droplevels(plan_vs_churn$column_category)), c("TRUE", "FALSE"))
})

test_that("residual_cells: character (non-factor) categorical column keeps first-appearance order", {
  set.seed(11)
  n <- 200
  # "Zebra" deliberately appears before "Apple" in raw row order.
  region <- sample(c("Zebra", "Apple", "Mango"), n, replace = TRUE, prob = c(.5, .3, .2))
  outcome <- sample(c("Yes", "No"), n, replace = TRUE)
  df <- data.frame(region = region, outcome = outcome, stringsAsFactors = FALSE)

  m <- df %>% exp_mca(region, outcome, ncp = 5)
  model <- m$model[[1]]
  rc <- m %>% tidy_rowwise(model, type = "residual_cells")
  expect_equal(levels(droplevels(rc$row_category)), c("Zebra", "Apple", "Mango"))
})

test_that("exp_mca_aggregated (wide) and exp_mca_aggregated_long already honor the source order (no regression)", {
  wide_df <- data.frame(
    plan = factor(plan_levels, levels = plan_levels),
    A = c(10, 20, 5, 8),
    B = c(4, 15, 12, 6)
  )
  m_wide <- wide_df %>% exp_mca_aggregated(plan, A, B, column_variable_name = "Segment", ncp = 2)
  dm_wide <- (m_wide %>% tidy_rowwise(model, type = "dimension_matrix") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(dm_wide, plan_levels)

  long_df <- expand.grid(plan = plan_levels, seg = c("A", "B"), stringsAsFactors = FALSE)
  long_df$plan <- factor(long_df$plan, levels = plan_levels)
  long_df$count <- c(10, 20, 5, 8, 4, 15, 12, 6)
  m_long <- long_df %>% exp_mca_aggregated_long(plan, seg, count, ncp = 2)
  dm_long <- (m_long %>% tidy_rowwise(model, type = "dimension_matrix") %>%
    dplyr::filter(variable == "plan", dimension == 1))$category
  expect_equal(dm_long, plan_levels)
})
