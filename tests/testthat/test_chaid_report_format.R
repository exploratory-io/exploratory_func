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

test_that('chaid_format_interval keeps the raw "> N" shape (machine format, tam #37691)', {
  # chaid_format_interval()'s output also becomes `cond_value` in build_chaid.R
  # (the interactive tree's Show Detail drill-down), which the tam-side
  # DTreeGenerator.parseNumericBinLabel parses as exactly "<=" / ">" / "(a, b]".
  # The report-display "N <" flip is a SEPARATE, later step -- see
  # chaid_display_symbol_after_number() below -- never folded in here.
  expect_equal(chaid_format_interval(chaid_parse_interval('> 8')), '> 8')
  expect_equal(chaid_format_interval(chaid_parse_interval('> 23')), '> 23')
  expect_equal(chaid_format_interval(chaid_parse_interval('<= 23')), '<= 23')
  expect_equal(chaid_format_interval(chaid_parse_interval('(3, 8]')), '(3, 8]')
})

test_that('chaid_display_symbol_after_number rewrites > N to N < for report display (tam #37691)', {
  expect_equal(chaid_display_symbol_after_number('> 8'), '8 <')
  expect_equal(chaid_display_symbol_after_number('> 23'), '23 <')
  # <= and the bounded (a, b] shape are unaffected.
  expect_equal(chaid_display_symbol_after_number('<= 23'), '<= 23')
  expect_equal(chaid_display_symbol_after_number('(3, 8]'), '(3, 8]')
  # Spec example: multiple "+"-joined tokens, only the unbounded one flips.
  expect_equal(chaid_display_symbol_after_number('<= 3 + (3, 8] + > 8'), '<= 3 + (3, 8] + 8 <')
  # Non-interval / passthrough values are untouched.
  expect_equal(chaid_display_symbol_after_number('Missing'), 'Missing')
  expect_equal(chaid_display_symbol_after_number('All'), 'All')
  expect_true(is.na(chaid_display_symbol_after_number(NA_character_)))
})

test_that('chaid_collapse_intervals collapses contiguous runs only', {
  # Spec examples (#37177). Stays on the raw "> N" shape -- see the
  # chaid_format_interval test above for why the display flip is a later,
  # separate step (chaid_display_symbol_after_number), not baked in here.
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

# tam #38107: cleanup_df(..., map_name = FALSE) replaces commas with periods in
# column names before chaid_fit() ever runs (mmpf::marginalPrediction does not
# handle commas well) -- model$nodes$split_variable / rule /
# category_merge_map$variable are therefore built entirely in this CLEANED
# name space. model$terms_mapping (clean -> original) is only computed by
# exp_chaid() after chaid_fit() returns, so report functions must resolve it
# back at display time via chaid_map_display_name()/chaid_map_display_names_in_text().

test_that('chaid_map_display_name maps a clean name back to its original, and is a no-op otherwise', {
  tm <- c('部署.1' = '部署,1', '年齢' = '年齢')
  expect_equal(chaid_map_display_name('部署.1', tm), '部署,1')
  # A name that IS its own mapping (no comma) is unchanged.
  expect_equal(chaid_map_display_name('年齢', tm), '年齢')
  # A name absent from terms_mapping passes through untouched.
  expect_equal(chaid_map_display_name('存在しない列', tm), '存在しない列')
  # NULL/empty terms_mapping is always a no-op (e.g. a hand-built model in a test).
  expect_equal(chaid_map_display_name('部署.1', NULL), '部署.1')
  expect_equal(chaid_map_display_name('部署.1', character(0)), '部署.1')
  # NA passes through.
  expect_true(is.na(chaid_map_display_name(NA_character_, tm)))
  # Vectorized.
  expect_equal(chaid_map_display_name(c('部署.1', '年齢', NA), tm), c('部署,1', '年齢', NA))
})

test_that('chaid_map_display_names_in_text rewrites every embedded clean name in a composite rule string', {
  tm <- c('部署.1' = '部署,1', '年齢' = '年齢')
  # Single condition.
  expect_equal(
    chaid_map_display_names_in_text('部署.1 in {営業 + 人事}', tm),
    '部署,1 in {営業 + 人事}')
  # Multi-condition rule ("&"-joined), one variable renamed, one not.
  expect_equal(
    chaid_map_display_names_in_text('Root & 部署.1 in {営業} & 年齢 in {<= 26}', tm),
    'Root & 部署,1 in {営業} & 年齢 in {<= 26}')
  # A longer clean name that is a PREFIX of a shorter one is not partially
  # substituted before the shorter one gets its turn (longest-first ordering).
  tm2 <- c('a' = 'a-original', 'ab' = 'ab-original')
  expect_equal(chaid_map_display_names_in_text('ab in {x}', tm2), 'ab-original in {x}')
  expect_equal(chaid_map_display_names_in_text('a in {x}', tm2), 'a-original in {x}')
  # NA passes through; NULL/empty terms_mapping is a no-op.
  expect_true(is.na(chaid_map_display_names_in_text(NA_character_, tm)))
  expect_equal(chaid_map_display_names_in_text('部署.1 in {x}', NULL), '部署.1 in {x}')
})

test_that('chaid_group_level_order resolves a CLEAN variable name through terms_mapping before looking up original_factor_levels', {
  # Without this, a comma-renamed factor predictor's declared level order is
  # invisible to chaid_group_level_order (original_factor_levels is keyed by
  # the ORIGINAL name, captured before cleanup) and silently falls through to
  # predictor_info's clean-keyed ordered/levels, or further to alphabetical.
  model <- list(
    terms_mapping = c('部署.1' = '部署,1'),
    original_factor_levels = list('部署,1' = c('営業', '研究開発', '人事')),
    predictor_info = list('部署.1' = list(ordered = FALSE, levels = c('研究開発', '人事', '営業')))
  )
  # Callers always pass the CLEAN name (matching predictor_info's key space,
  # exactly as chaid_numeric_intervals()/chaid_category_merge_table() do).
  expect_equal(chaid_group_level_order(model, '部署.1'), c('営業', '研究開発', '人事'))
})
