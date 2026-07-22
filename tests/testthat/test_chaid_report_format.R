context('chaid report formatting helpers (tam #37177)')

test_that('chaid_parse_interval recognizes the three bin label shapes', {
  expect_equal(chaid_parse_interval('<= 3')$upper, '3')
  expect_equal(chaid_parse_interval('<= 3')$lower_value, -Inf)
  expect_equal(chaid_parse_interval('> 23')$lower, '23')
  expect_equal(chaid_parse_interval('> 23')$upper_value, Inf)
  i <- chaid_parse_interval('(3, 6.7]')
  expect_equal(i$lower, '3')
  expect_equal(i$upper, '6.7')
  expect_null(chaid_parse_interval('Missing'))
  expect_null(chaid_parse_interval('研究開発'))
  expect_null(chaid_parse_interval('<= abc'))
})

test_that('chaid_collapse_intervals collapses contiguous runs only', {
  # Spec examples (#37177).
  expect_equal(chaid_collapse_intervals(c('(3, 5]', '(5, 6.7]', '(6.7, 8]')), '(3, 8]')
  expect_equal(
    chaid_collapse_intervals(c('(8, 10]', '(10, 13]', '(13, 17]', '(17, 23]', '> 23')),
    '> 8')
  expect_equal(
    chaid_collapse_intervals(c('<= 26', '(26, 29]', '(29, 31]', '(31, 34]')),
    '<= 34')
  expect_equal(
    chaid_collapse_intervals(c('(38, 41]', '(41, 45]', '(45, 50]', '> 50')),
    '> 38')
})

test_that('a gap keeps the pieces enumerated', {
  # User decision (#37177): never collapse across a gap.
  expect_equal(chaid_collapse_intervals(c('(34, 36]', '(41, 45]')),
               c('(34, 36]', '(41, 45]'))
  expect_equal(chaid_collapse_intervals(c('(3, 5]', '(5, 6.7]', '(10, 13]')),
               c('(3, 6.7]', '(10, 13]'))
})

test_that('non-interval members pass through and act as barriers', {
  expect_equal(chaid_collapse_intervals(c('(3, 5]', '(5, 8]', 'Missing')),
               c('(3, 8]', 'Missing'))
  expect_equal(chaid_collapse_intervals(c('研究開発', '人事')), c('研究開発', '人事'))
  expect_equal(chaid_collapse_intervals(character()), character())
  expect_equal(chaid_collapse_intervals('<= 3'), '<= 3')
})

test_that('chaid_readable_condition rewrites the spec examples', {
  expect_equal(
    chaid_readable_condition('Root & 勤続年数 in {(3, 5] + (5, 6.7] + (6.7, 8]}'),
    '3 < 勤続年数 <= 8')
  expect_equal(
    chaid_readable_condition(
      'Root & 勤続年数 in {(8, 10] + (10, 13] + (13, 17] + (17, 23] + > 23} & 部署 in {研究開発 + 人事}'),
    '勤続年数 > 8 & 部署 in (研究開発 + 人事)')
})

test_that('chaid_readable_condition handles the root and edge shapes', {
  expect_equal(chaid_readable_condition('Root'), 'All')
  expect_equal(chaid_readable_condition(''), 'All')
  expect_true(is.na(chaid_readable_condition(NA_character_)))
  expect_equal(chaid_readable_condition('Root & 年齢 in {<= 26}'), '年齢 <= 26')
  expect_equal(chaid_readable_condition('Root & 年齢 in {> 50}'), '年齢 > 50')
  expect_equal(chaid_readable_condition(c('Root', 'Root & 年齢 in {<= 26}')),
               c('All', '年齢 <= 26'))
})

test_that('a single non-interval member reads as an equality (tam #37177)', {
  expect_equal(chaid_readable_condition('Root & 部署 in {営業}'), '部署 = 営業')
  expect_equal(chaid_readable_condition('Root & 残業 in {TRUE}'), '残業 = TRUE')
  expect_equal(chaid_readable_condition('Root & 残業 in {FALSE}'), '残業 = FALSE')
  expect_equal(
    chaid_readable_condition('Root & 残業 in {TRUE} & 給料 in {<= 2695.8}'),
    '残業 = TRUE & 給料 <= 2695.8')
  # A contiguous bin run that collapses to ONE interval keeps the inequality
  # form -- the equality branch is only for non-interval members.
  expect_equal(
    chaid_readable_condition('Root & 給料 in {<= 2317.6 + (2317.6, 2695.8]}'),
    '給料 <= 2695.8')
  # Multi-member groups keep the in (...) form.
  expect_equal(
    chaid_readable_condition('Root & 職種 in {ラボ技術者 + 営業担当}'),
    '職種 in (ラボ技術者 + 営業担当)')
})

test_that('a category value containing " & " does not break condition splitting', {
  expect_equal(
    chaid_readable_condition('Root & 部署 in {R & D + 人事} & 年齢 in {<= 26}'),
    '部署 in (R & D + 人事) & 年齢 <= 26')
  expect_equal(
    chaid_readable_condition('Root & 部署 in {R & D}'),
    '部署 = R & D')
})

test_that('chaid_order_group_parts sorts alphabetically without levels', {
  expect_equal(chaid_order_group_parts(c('b', 'a', 'c')), c('a', 'b', 'c'))
  expect_equal(chaid_order_group_parts('a'), 'a')
})

test_that('chaid_order_group_parts honors declared level order', {
  levels <- c('既婚', '離婚', '独身')
  expect_equal(chaid_order_group_parts(c('離婚', '既婚'), levels), c('既婚', '離婚'))
  expect_equal(chaid_order_group_parts(c('既婚', '離婚'), levels), c('既婚', '離婚'))
  # Unknown members keep their relative order, after the declared ones.
  expect_equal(chaid_order_group_parts(c('不明', '離婚'), levels), c('離婚', '不明'))
})

test_that('chaid_normalize_group_label orders then collapses', {
  expect_equal(
    chaid_normalize_group_label('(5, 6.7] + (3, 5]', levels = c('(3, 5]', '(5, 6.7]')),
    '(3, 6.7]')
  expect_equal(chaid_normalize_group_label('離婚 + 既婚'), '既婚 + 離婚')
  expect_equal(
    chaid_normalize_group_label('離婚 + 既婚', levels = c('既婚', '離婚', '独身')),
    '既婚 + 離婚')
  expect_equal(
    chaid_normalize_group_label('(3, 5] + (5, 6.7]', collapse = FALSE,
                                levels = c('(3, 5]', '(5, 6.7]')),
    '(3, 5] + (5, 6.7]')
})

test_that('chaid_keep_final_merges keeps the last row of each merge chain', {
  merges <- data.frame(
    node_id = c(1, 1, 1, 1, 6, 6, 2),
    variable = c('t', 't', 't', 't', 'a', 'a', 'm'),
    original_categories = c(
      '(3, 5] | (5, 6.7]',
      '(3, 5] | (5, 6.7] | (6.7, 8]',
      '(8, 10] | (10, 13]',
      '(8, 10] | (10, 13] | (13, 17]',
      '<= 26 | (26, 29]',
      '(34, 36] | (36, 38]',
      '離婚 | 既婚'),
    stringsAsFactors = FALSE
  )
  kept <- chaid_keep_final_merges(merges)
  expect_equal(kept$original_categories, c(
    '(3, 5] | (5, 6.7] | (6.7, 8]',
    '(8, 10] | (10, 13] | (13, 17]',
    '<= 26 | (26, 29]',
    '(34, 36] | (36, 38]',
    '離婚 | 既婚'))
})

test_that('chaid_keep_final_merges is a no-op on empty input', {
  empty <- data.frame(node_id = integer(), variable = character(),
                      original_categories = character(), stringsAsFactors = FALSE)
  expect_equal(nrow(chaid_keep_final_merges(empty)), 0)
})

test_that('chaid_group_level_order prefers the user-declared factor levels', {
  model <- list(
    original_factor_levels = list(部署 = c('営業', '研究開発', '人事')),
    predictor_info = list(
      部署 = list(ordered = FALSE, levels = c('研究開発', '人事', '営業')),
      婚姻ステータス = list(ordered = FALSE, levels = c('離婚', '既婚', '独身')),
      年齢 = list(ordered = TRUE, levels = c('<= 26', '(26, 29]', '> 29'))
    )
  )
  # Declared factor order wins over the post-cleanup appearance order.
  expect_equal(chaid_group_level_order(model, '部署'), c('営業', '研究開発', '人事'))
  # A character predictor has no meaningful order -> alphabetical.
  expect_null(chaid_group_level_order(model, '婚姻ステータス'))
  # A binned numeric / ordinal keeps its bin order.
  expect_equal(chaid_group_level_order(model, '年齢'), c('<= 26', '(26, 29]', '> 29'))
  expect_null(chaid_group_level_order(model, '存在しない列'))
  expect_equal(
    chaid_normalize_group_label('人事 + 営業', chaid_group_level_order(model, '部署')),
    '営業 + 人事')
})
