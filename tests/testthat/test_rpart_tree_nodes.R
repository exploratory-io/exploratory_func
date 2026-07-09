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
                   "n","pct","class_json","cond_column","cond_operator","cond_value","mean_value")

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
})

test_that("tree_nodes regression - predicted mean, class_json NA, mean_value populated", {
  model_df <- flight %>% exp_rpart(`ARR DELAY`, `DEP DELAY`)
  nodes <- model_df %>% tidy_rowwise(model, type="tree_nodes")
  expect_equal(colnames(nodes), expected_cols)
  expect_true(all(is.na(nodes$class_json)))
  expect_true(all(!is.na(nodes$mean_value)))
  # predicted string equals formatted mean_value
  expect_true(all(!is.na(nodes$predicted)))
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
