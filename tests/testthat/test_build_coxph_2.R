context("Cox regression test part 2")
# This test case detected the following 2 error cases that are fixed in the following commits.
# a86c5507 - Error in `approxfun(res$time, res0[[i]])`: need at least two non-NA values to interpolate
# 0603e434 - Error in `data.table::rbindlist(curve_dfs_list)`: Item 11 has 8 columns, inconsistent with item 1 which has 9 columns. To fill missing columns use fill=TRUE.
test_that("Verify Cox regression issue fixed by a86c5507 (conf.int NA handling) and 0603e434 (error fron rbindlist)", {
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/6tmbwv0hbcxda0f/airline_2013_10_tricky_v4_ja_damemoji.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
    readr::type_convert() %>%
    exploratory::clean_data_frame() %>%
    sample_n(5000, seed = 1)
  df2 <- df %>% mutate(`終了 時表` = as.Date(`終了 時表`)) %>% mutate(`終了 時表` = impute_na(`終了 時表`, type="value", val=max(`フライト 日表`, `終了 時表`, na.rm=TRUE)))
  df3 <- df2 %>% filter(`UA または AA表`)
  model_df <- df3 %>% build_coxph.fast(NULL, `遅れ た表`, `航空 会社表`, `距 離表`, `航空 会社名表`, `年 度表`, `天候による 遅れ表`, `キャンセル コード表`, `キャン セル表`, `出発 空港表`, `出発 都市名表`, `出発 州表`, `到着 空港表`, `到着 都市名表`, `到着 州表`, `出発 時刻表`, `出発 遅れ表`, predictor_funs = list(`航空 会社表`="none", `距 離表`="none", `航空 会社名表`="none", `年 度表`="none", `天候による 遅れ表`="none", `キャンセル コード表`="none", `キャン セル表`="none", `出発 空港表`="none", `出発 都市名表`="none", `出発 州表`="none", `到着 空港表`="none", `到着 都市名表`="none", `到着 州表`="none", `出発 時刻表`="none", `出発 遅れ表`="none"), start_time = `フライト 日表`, end_time = `終了 時表`, time_unit = "auto", test_split_type = "random", test_rate = 0.3)
  expect_true(!is.null(model_df$model))
})
