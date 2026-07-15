# how to run this test:
# devtools::test(filter="rpart_tree_nodes")
context("test rpart tree_nodes tidier")

if (!exists("flight")) {
  flight <- exploratory::read_delim_file("https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  set.seed(1)
  flight <- flight %>% slice_sample(n=5000)
}

# Canonical stress column name (CLAUDE.md rule 7).
stress_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"

expected_cols <- c("node_id","parent_id","depth","is_leaf","edge_label","predicted",
                   "n","pct","class_json","cond_column","cond_operator","cond_value","mean_value",
                   "sd_value","rmse_value","dist_json")

parse_classes <- function(json) {
  # returns list of data.frames per row (NA -> NULL)
  lapply(json, function(j) if (is.na(j)) NULL else jsonlite::fromJSON(j))
}

test_that("tree_nodes binary classification (logical target) - shape, sums, TRUE-first order", {
  model_df <- flight %>% exp_rpart(`delay ed`, `DEP DELAY`, `ARR DELAY`)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_true(is.data.frame(nodes))
  expect_equal(colnames(nodes), expected_cols)
  expect_true(nrow(nodes) >= 1)

  # root
  root <- nodes[nodes$node_id == 1, ]
  expect_equal(nrow(root), 1)
  expect_true(is.na(root$parent_id))
  expect_equal(root$depth, 0L)

  # every non-root has a parent present in the table
  non_root <- nodes[nodes$node_id != 1, ]
  expect_true(all(non_root$parent_id %in% nodes$node_id))

  # The LEAVES partition the data (a pruned tree has leaves at varying depths, so
  # a single depth level does NOT sum to root n -- only the full leaf set does).
  root_n <- root$n
  expect_equal(sum(nodes$n[nodes$is_leaf]), root_n)
  # every internal node's n equals the sum of its two children's n
  for (i in which(!nodes$is_leaf)) {
    kids <- nodes$n[nodes$parent_id %in% nodes$node_id[i]]
    if (length(kids) == 2) expect_equal(sum(kids), nodes$n[i])
  }

  # class_json: per-node classes present, TRUE listed FIRST for a logical target
  cls <- parse_classes(nodes$class_json)
  root_cls <- cls[[which(nodes$node_id == 1)]]
  expect_equal(root_cls$label[1], "TRUE")               # requirement 3
  expect_setequal(root_cls$label, c("TRUE","FALSE"))
  # class counts sum to node n
  expect_equal(sum(root_cls$n), root_n)

  # leaves flagged
  expect_true(any(nodes$is_leaf))
})

test_that("tree_nodes multiclass classification - one class entry per ylevel", {
  flight3 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY","TX"))
  model_df <- flight3 %>% exp_rpart(`ORIGIN STATE ABR`, `DEP DELAY`, `ARR DELAY`)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)
  cls <- parse_classes(nodes$class_json)
  root_cls <- cls[[which(nodes$node_id == 1)]]
  expect_setequal(root_cls$label, c("CA","NY","TX"))
  expect_equal(sum(root_cls$n), nodes[nodes$node_id==1, ]$n)
  expect_true(all(is.na(nodes$mean_value)))              # not regression
  # classification leaves the regression-only columns NA (requirement 7)
  expect_true(all(is.na(nodes$sd_value)))
  expect_true(all(is.na(nodes$rmse_value)))
  expect_true(all(is.na(nodes$dist_json)))
})

test_that("tree_nodes regression - predicted mean, class_json NA, sd/rmse/dist (requirement 7)", {
  model_df <- flight %>% exp_rpart(`ARR DELAY`, `DEP DELAY`)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)
  expect_true(all(is.na(nodes$class_json)))
  expect_true(all(!is.na(nodes$mean_value)))
  expect_true(all(!is.na(nodes$predicted)))

  # SD / RMSE come from frame$dev (anova within-node SSE). Rebuild the model's
  # frame to check the closed forms exactly.
  rp <- model_df$model[[1]]
  fr <- rp$frame
  fr_ids <- as.integer(rownames(fr))
  ord <- match(nodes$node_id, fr_ids)
  expect_equal(nodes$rmse_value, sqrt(fr$dev[ord] / fr$n[ord]), tolerance = 1e-8)
  sd_expected <- ifelse(fr$n[ord] > 1, sqrt(fr$dev[ord] / (fr$n[ord] - 1)), NA_real_)
  expect_equal(nodes$sd_value, sd_expected, tolerance = 1e-8)

  # dist_json: shared bins on every node; per-node counts sum to n; an internal
  # node's counts equal the element-wise sum of its two children's.
  parse_dist <- function(j) jsonlite::fromJSON(j)
  root <- parse_dist(nodes$dist_json[nodes$node_id == 1])
  expect_equal(length(root$breaks), 21)                  # ~20 equal-width bins
  expect_equal(sum(root$counts), nodes$n[nodes$node_id == 1])
  # breaks identical across all nodes
  all_breaks <- lapply(nodes$dist_json, function(j) parse_dist(j)$breaks)
  for (b in all_breaks) expect_equal(b, root$breaks)
  # children-sum invariant on the root's two children (if the tree split)
  kids <- nodes[!is.na(nodes$parent_id) & nodes$parent_id == 1, ]
  if (nrow(kids) == 2) {
    c1 <- parse_dist(kids$dist_json[1])$counts
    c2 <- parse_dist(kids$dist_json[2])$counts
    expect_equal(c1 + c2, root$counts)
  }
})

test_that("tree_nodes regression - integer/discrete target uses one bin per integer", {
  set.seed(7)
  n <- 400
  df <- tibble::tibble(x1 = rnorm(n), x2 = rnorm(n))
  # discrete integer target, small range (<= 30) so it should get per-integer bins
  df$level <- as.integer(pmin(5L, pmax(1L, round(3 + df$x1))))
  model_df <- df %>% exp_rpart(level, x1, x2)
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")

  root <- jsonlite::fromJSON(nodes$dist_json[nodes$node_id == 1])
  # one bin per integer: breaks are consecutive half-integers (width 1, at n +/- 0.5)
  expect_true(all(abs(diff(root$breaks) - 1) < 1e-9))
  expect_true(all(abs((root$breaks %% 1) - 0.5) < 1e-9))
  rng <- range(df$level)
  expect_equal(length(root$counts), as.integer(diff(rng)) + 1L)
  expect_equal(sum(root$counts), nodes$n[nodes$node_id == 1])
})

test_that("tree_nodes multibyte/symbol column with categorical + numeric split", {
  set.seed(42)
  n <- 400
  df <- tibble::tibble(
    !!stress_name := sample(c("A,B","C","D","E"), n, replace=TRUE),  # comma inside a level
    num_col = rnorm(n),
    target = factor(sample(c("yes","no"), n, replace=TRUE))
  )
  # make the split meaningful: bias target by the stress column
  df$target <- factor(ifelse(df[[stress_name]] %in% c("A,B","C"), "yes", "no"))
  model_df <- df %>% exp_rpart(target, !!rlang::sym(stress_name), num_col)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)

  # a categorical split on the stress column must surface the ORIGINAL name in cond_column + edge_label
  cat_rows <- nodes[!is.na(nodes$cond_operator) & nodes$cond_operator == "in", ]
  expect_true(nrow(cat_rows) >= 1)
  expect_true(any(cat_rows$cond_column == stress_name))
  expect_true(any(grepl(stress_name, nodes$edge_label, fixed=TRUE)))

  # cond_value for `in` is a JSON array string (robust to commas inside a level)
  jv <- cat_rows$cond_value[cat_rows$cond_column == stress_name][1]
  parsed <- jsonlite::fromJSON(jv)
  expect_true(is.character(parsed))
  expect_true(all(parsed %in% c("A,B","C","D","E")))
})

test_that("tree_nodes single-node tree (no split) - one leaf row, no error", {
  set.seed(7)
  n <- 60
  df <- tibble::tibble(
    x = rnorm(n),                                   # varying predictor (avoids "only one unique value")
    target = factor(sample(c("yes","no"), n, replace=TRUE))
  )
  # cp = 0.9 makes any split fail the complexity threshold -> root-only tree.
  model_df <- df %>% exp_rpart(target, x, cp = 0.9)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)
  expect_equal(nrow(nodes), 1)
  expect_true(nodes$is_leaf[1])
  expect_true(is.na(nodes$parent_id[1]))
  expect_true(is.na(nodes$edge_label[1]) || nodes$edge_label[1] == "")
})

test_that("tree_nodes regression with a (near-)constant target - no crash, SD/RMSE ~ 0 (requirement 7)", {
  set.seed(11)
  n <- 80
  df <- tibble::tibble(
    x = rnorm(n),
    target = 5 + rnorm(n) * 1e-9                       # essentially constant
  )
  model_df <- df %>% exp_rpart(target, x, cp = 0.9)     # root-only regression tree
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)
  expect_equal(nrow(nodes), 1)
  expect_true(!is.na(nodes$mean_value[1]))
  expect_lt(nodes$rmse_value[1], 1e-6)                  # zero variance -> ~0
  # dist_json still valid: single-bin (or degenerate) histogram summing to n
  d <- jsonlite::fromJSON(nodes$dist_json[1])
  expect_equal(sum(d$counts), nodes$n[1])
})
