context("importance p-value contract across models")

# A model that attaches P values to its Variable Importance table has to map model TERMS
# back to VARIABLE names. That lookup broke twice in build_multinom_logit (tam#37033) and
# the failure is invisible -- the chart just says 判定不可 / "P Value unavailable" for a
# variable whose coefficients are fine.
#
# The population is DISCOVERED from the source, not listed by hand: every R file using the
# `p.value = purrr::map_dbl(variable, ...)` idiom must appear in MODELS_UNDER_CONTRACT with
# a closure that fits it on the shared shape fixture. A NEW analytics model that copies this
# idiom from build_polr / build_multinom_logit -- the normal way a new type is written --
# fails this test until it is registered, and then has to pass every predictor shape.

# file name -> how to fit that model on importance_contract_df()
MODELS_UNDER_CONTRACT <- list(
  "build_multinom_logit.R" = function(df) {
    df %>% build_multinom_logit(y_category, !!!rlang::syms(importance_contract_predictors()),
                                reference_category = "中")
  },
  "build_polr.R" = function(df) {
    df %>% build_polr(y_category, !!!rlang::syms(importance_contract_predictors()))
  }
)

#' R files whose tidy() builds an importance table's p.value by mapping terms to variables.
#' @return character vector of file names
discover_importance_p_value_files <- function() {
  r_dir <- normalizePath(file.path("..", "..", "R"), mustWork = FALSE)
  if (!dir.exists(r_dir)) {
    r_dir <- normalizePath(file.path(rprojroot::find_package_root_file(), "R"), mustWork = FALSE)
  }
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  keep <- vapply(files, function(f) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    grepl("p.value = purrr::map_dbl(variable", src, fixed = TRUE)
  }, logical(1))
  basename(files[keep])
}

test_that("every model that maps terms to variables for importance is under contract", {
  discovered <- discover_importance_p_value_files()
  expect_true(length(discovered) > 0,
              info = "the discovery regex found no model files -- the idiom moved, update it")
  unregistered <- setdiff(discovered, names(MODELS_UNDER_CONTRACT))
  expect_equal(unregistered, character(0),
               info = paste("these files build an importance p-value by matching model terms to variable",
                            "names but are not in MODELS_UNDER_CONTRACT. Add a fit closure so every",
                            "predictor shape is checked:", paste(unregistered, collapse = ", ")))
  stale <- setdiff(names(MODELS_UNDER_CONTRACT), discovered)
  expect_equal(stale, character(0),
               info = paste("registered but no longer using the idiom -- drop:", paste(stale, collapse = ", ")))
})

for (model_file in names(MODELS_UNDER_CONTRACT)) {
  local({
    file_name <- model_file
    fit <- MODELS_UNDER_CONTRACT[[model_file]]
    test_that(paste(file_name, "gives every predictor shape an importance p-value from its own coefficients"), {
      df <- importance_contract_df()
      model_df <- fit(df)
      expect_importance_matches_coefficients(model_df, file_name, df)
    })
  })
}
