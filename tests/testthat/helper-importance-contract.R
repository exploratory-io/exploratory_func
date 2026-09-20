# Shared fixture + assertion for the "importance p-value comes from the variable's own
# coefficients" contract (tam#37033).
#
# A model that shows a Variable Importance chart colored by significance has to map each
# model TERM back to the variable it came from. The term text depends on the predictor's
# TYPE and on whether its NAME is syntactic, and two shapes shipped broken one after the
# other in build_multinom_logit: a name holding a Japanese comma (R backtick-quotes any
# non-syntactic name, and an ASCII-only quoting guess missed it) and a logical predictor
# (its only term is "<var>TRUE" and it has no xlevels entry). Both surfaced to the user as
# 判定不可 / "P Value unavailable" on a variable whose coefficients had perfectly good
# P values.
#
# Every predictor shape lives here, in one fixture, so a model is tested against all of
# them at once rather than against the one shape whose bug was reported.

IMPORTANCE_CONTRACT_SHAPES <- list(
  list(name = "満足度", type = "numeric"),
  list(name = "サポート満足度 (1-5)", type = "numeric"),           # ASCII space + parens -> quoted
  list(name = "商品を購入する前に、価格を比較する", type = "numeric"), # 、 -> quoted, no ASCII symbol
  list(name = "x", type = "numeric"),                             # prefix of "x2" below
  list(name = "x2", type = "numeric"),
  list(name = "プラン", type = "character"),
  list(name = "地域、区分", type = "character"),                   # quoted factor
  list(name = "満足度ランク", type = "ordered"),                   # ordered factor -> unordered in the fit
  list(name = "モバイルアプリ利用", type = "logical"),              # term is "<var>TRUE"
  list(name = "解約、意向あり", type = "logical")                   # quoted AND TRUE-suffixed
)

#' Predictor names of the contract fixture.
#' @return character vector
importance_contract_predictors <- function() {
  vapply(IMPORTANCE_CONTRACT_SHAPES, function(shape) shape$name, character(1))
}

#' Build a data frame holding one column per predictor shape, plus targets.
#'
#' Every predictor really influences the target, so no coefficient is estimated at exactly
#' zero and a missing P value can only mean the lookup failed. Columns: the shapes, plus
#' `y_category` (3 ordered-or-not categories) and `y_numeric`.
#'
#' @param n number of rows
#' @param seed random seed
#' @return data frame
importance_contract_df <- function(n = 900, seed = 37033) {
  set.seed(seed)
  df <- data.frame(.row = seq_len(n))
  for (shape in IMPORTANCE_CONTRACT_SHAPES) {
    df[[shape$name]] <- switch(shape$type,
      numeric = stats::rnorm(n),
      character = sample(c("A", "B", "C"), n, replace = TRUE),
      ordered = factor(sample(c("低", "中", "高"), n, replace = TRUE), levels = c("低", "中", "高"), ordered = TRUE),
      logical = stats::runif(n) < 0.5
    )
  }
  score <- df[["満足度"]] + df[["x"]] - df[["x2"]] + df[["サポート満足度 (1-5)"]] +
    df[["商品を購入する前に、価格を比較する"]] +
    (df[["プラン"]] == "A") + (df[["地域、区分"]] == "B") +
    as.numeric(df[["満足度ランク"]] == "高") +
    df[["モバイルアプリ利用"]] + df[["解約、意向あり"]]
  df$.row <- NULL
  df$y_numeric <- score + stats::rnorm(n)
  # The category MUST be noisy, not a deterministic function of the predictors: a
  # separable target makes every standard error NA, every coefficient p-value NA, and
  # the contract below vacuous (it passed a sabotage run that way while it was written).
  noisy <- score + stats::rnorm(n, sd = 2.5)
  cuts <- stats::quantile(noisy, c(1 / 3, 2 / 3))
  df$y_category <- factor(
    ifelse(noisy < cuts[[1]], "低", ifelse(noisy < cuts[[2]], "中", "高")),
    levels = c("低", "中", "高")
  )
  df
}

#' Assert every variable's importance P value is the smallest P value among its OWN terms.
#'
#' Accepts each term shape a predictor can produce: the bare name (numeric), "<var>: <level>"
#' or "<var><level>" (categorical, prettified or raw) and "<var>TRUE" (logical).
#'
#' @param model_df a model data frame (the result of a build_* call)
#' @param label name of the model function, for failure messages
#' @param fixture the data frame the model was fitted on (for each predictor's type and levels)
#' @return invisible(NULL)
expect_importance_matches_coefficients <- function(model_df, label, fixture) {
  coef_df <- model_df %>% tidy_rowwise(model, conf.int = FALSE, exponentiate = FALSE)
  imp_df <- model_df %>% tidy_rowwise(model, type = "importance")
  predictors <- importance_contract_predictors()

  expect_setequal(imp_df$variable, predictors)
  # Fail-closed: if the fit produced no usable p-values at all (a separable or degenerate
  # fixture), every per-variable comparison below would be skipped and this test would
  # assert nothing.
  expect_true(sum(!is.na(coef_df$p.value)) >= length(predictors),
              info = paste(label, "-- fixture produced no coefficient p-values; the contract would be vacuous"))
  # EXACT term strings per variable, derived from the fixture's own shapes -- never a
  # prefix match: "満足度" is a prefix of "満足度ランク", so a loose match would hand a
  # variable its neighbour's coefficients and the contract would compare the wrong rows.
  shape_of <- function(var) {
    Filter(function(shape) identical(shape$name, var), IMPORTANCE_CONTRACT_SHAPES)[[1]]$type
  }
  levels_of <- function(var) {
    values <- fixture[[var]]
    if (is.factor(values)) levels(values) else sort(unique(as.character(values)))
  }
  terms_of <- function(var) {
    type <- shape_of(var)
    expected <- switch(type,
      numeric = var,
      ordered = ,
      character = {
        non_reference <- levels_of(var)[-1]
        c(paste0(var, ": ", non_reference), paste0(var, non_reference))
      },
      logical = c(paste0(var, "TRUE"), paste0(var, ": TRUE"))
    )
    bare <- gsub("`", "", as.character(coef_df$term), fixed = TRUE)
    bare %in% expected
  }
  for (var in predictors) {
    own <- unname(coef_df$p.value[terms_of(var)])
    own <- own[!is.na(own)]
    if (length(own) == 0) {
      # The model could not estimate this term's standard error at all; the chart is then
      # right to say "unavailable". Nothing to compare.
      next
    }
    expect_equal(unname(imp_df$p.value[imp_df$variable == var]), unname(min(own)),
                 info = paste0(label, " / ", var,
                               " -- importance p-value does not come from this variable's own coefficients"))
  }
  expect_false(any(is.na(imp_df$p.value[imp_df$variable %in% predictors]) &
                     vapply(predictors, function(v) any(!is.na(coef_df$p.value[terms_of(v)])), logical(1))),
               info = paste(label, "-- a variable whose coefficients have p-values was reported as unavailable"))
  invisible(NULL)
}
