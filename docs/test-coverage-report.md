# Exploratory Package Test Coverage Report

- Generated on: `2026-03-11 11:09:20 PDT`
- Package: `exploratory`
- Scope: all named functions discovered in `R/*.R` via `covr` instrumentation
- Method: function-level expression coverage from `covr::environment_coverage()` with tests executed through `testthat::test_file(..., stop_on_failure = FALSE, stop_on_warning = FALSE)`

## Executive Summary

| Metric | Value |
| --- | ---: |
| Overall package coverage | 70.024% |
| Total functions | 865 |
| Covered functions (>0%) | 641 |
| Uncovered functions (0%) | 224 |
| Partially covered functions | 285 |
| Fully covered functions | 356 |
| Exported functions | 511 |
| Exported covered functions | 349 |
| Exported uncovered functions | 162 |
| Uncovered exported functions used by Desktop production code/config | 142 |
| Uncovered exported functions referenced only by Desktop tests | 1 |
| Uncovered exported functions with no Desktop hit in strict scan | 19 |

## Coverage Bands

| Function coverage band | Functions |
| --- | ---: |
| 0% | 224 |
| 0.1-25% | 5 |
| 25.1-50% | 6 |
| 50.1-75% | 63 |
| 75.1-99.9% | 211 |
| 100% | 356 |

## Highest-Risk Files

| File | Functions | Covered | Uncovered | Partial | Full | Avg. function coverage |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | 112 | 49 | 63 | 25 | 24 | 38.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | 16 | 0 | 16 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | 15 | 0 | 15 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | 15 | 0 | 15 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | 14 | 0 | 14 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | 193 | 180 | 13 | 42 | 138 | 89.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | 12 | 0 | 12 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | 12 | 0 | 12 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | 8 | 1 | 7 | 0 | 1 | 12.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | 6 | 0 | 6 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | 5 | 0 | 5 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | 51 | 46 | 5 | 24 | 22 | 82.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | 4 | 0 | 4 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | 4 | 0 | 4 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | 30 | 26 | 4 | 18 | 8 | 72.0% |

## Desktop Usage of Uncovered Exported Functions

- Desktop scan root: `~/Work/gitrepo/tam`
- Strict search scope: `src/js`, `public/lib/uidefinitions`, `public/lib/plugins`, `public/lib/models`, and `test`
- “Production” means the function name appears in first-party Desktop source/config, typically translator code, plugin definitions, or UI definitions.
- “Test only” means the name appeared only in Desktop tests.
- `toJSON` is manually classified as `No hit` because the strict search only found `jsonlite::toJSON(...)` or helper-name matches, not the exported `exploratory::toJSON` API.

| Desktop usage class | Functions | Notes |
| --- | ---: | --- |
| Production | 142 | Referenced from Desktop source/config such as `src/js/translator/RTranslator.js` and plugin/UI definition files. |
| Test only | 1 | Present only in Desktop tests. |
| No hit | 19 | No first-party Desktop reference found in the strict scan. |

### Test-Only References

| Function | Desktop evidence |
| --- | --- |
| `refreshGoogleTokenForSheet` | `src/js/models/Plugin.test.js` |

### No-Hit Functions

| Function | Note |
| --- | --- |
| `getAzureContainer` | No first-party Desktop reference found in the strict scan. |
| `getAzureEndPoint` | No first-party Desktop reference found in the strict scan. |
| `model_confint` | No first-party Desktop reference found in the strict scan. |
| `getSalesforceToken` | No first-party Desktop reference found in the strict scan. |
| `refreshGoogleTokenForAnalytics` | No first-party Desktop reference found in the strict scan. |
| `refreshSalesforceToken` | No first-party Desktop reference found in the strict scan. |
| `do_kl_dist.kv_` | No first-party Desktop reference found in the strict scan. |
| `do_princomp` | No first-party Desktop reference found in the strict scan. |
| `downloadDataFromGoogleCloudStorage` | No first-party Desktop reference found in the strict scan. |
| `extractDataFromGoogleBigQueryToCloudStorage` | No first-party Desktop reference found in the strict scan. |
| `getListOfColumns` | No first-party Desktop reference found in the strict scan. |
| `getTokenInfo` | No first-party Desktop reference found in the strict scan. |
| `toJSON` | Matches in `tam` were `jsonlite::toJSON(...)` and helper names, not the exported `exploratory::toJSON` API. |
| `do_chisq.test_` | No first-party Desktop reference found in the strict scan. |
| `any_error` | No first-party Desktop reference found in the strict scan. |
| `get_num_errors` | No first-party Desktop reference found in the strict scan. |
| `calculate_cohens_f_squared` | No first-party Desktop reference found in the strict scan. |
| `pivot_` | No first-party Desktop reference found in the strict scan. |
| `to_matrix` | No first-party Desktop reference found in the strict scan. |

## Uncovered Exported Functions

| File | Function | Coverage | Desktop usage in `tam` | Desktop evidence |
| --- | --- | ---: | --- | --- |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `clearS3CacheFile` | 0% | Production | `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getCSVFileFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getCSVFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelFileFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelSheetsFromS3ExcelFile` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getParquetFileFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getParquetFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getS3Folders` | 0% | Production | `src/js/actions/DataFileListDialogActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `guessFileEncodingForS3File` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetCSVFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetExcelFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetParquetFilesFromS3` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getCSVFileFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getCSVFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelFileFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelSheetsFromAzureExcelFile` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getParquetFileFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getParquetFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `listAzureContainers` | 0% | Production | `src/js/translator/RTranslator.js`; `src/js/util/RemoteConnectionChecker.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `listItemsInAzure` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetCSVFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetExcelFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetParquetFilesFromAzure` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | `do_market_impact` | 0% | Production | `public/lib/uidefinitions/ColumnMenu.json`; `public/lib/uidefinitions/ColumnMenu_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `execute_tidyquant` | 0% | Production | `public/lib/plugins/tidyquant/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `get_riem_measures` | 0% | Production | `public/lib/plugins/riem_measures/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `riem_stations_exp` | 0% | Production | `public/lib/plugins/riem_measures/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_retention_cohort.R` | `do_cohort` | 0% | Production | `public/lib/uidefinitions/do_cohort.json`; `src/js/components/analysis/AnalysisUtil.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_intercom_data.R` | `get_intercom_data` | 0% | Production | `public/lib/plugins/intercom/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | `get_mailchimp_data` | 0% | Production | `public/lib/plugins/mailchimp/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_stripe_data.R` | `get_stripe_data` | 0% | Production | `public/lib/plugins/stripe/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalytics` | 0% | Production | `public/lib/plugins/googleanalytics/extension.json`; `public/lib/plugins/googleanalytics/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalyticsSegmentList` | 0% | Production | `public/lib/plugins/googleanalytics/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalyticsTimeZoneInfo` | 0% | Production | `src/js/actions/PluginActions.js`; `src/js/components/config/DataConnectionDialog.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleProfile` | 0% | Production | `public/lib/plugins/googleanalytics/extension.json`; `public/lib/plugins/googleanalytics/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `clearGoogleCloudStorageCacheFile` | 0% | Production | `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getCSVFileFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getCSVFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelFileFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelSheetsFromGoogleCloudStorageExcelFile` | 0% | Production | `src/js/actions/DataFrameActions.js`; `src/js/actions/ExcelSheetsListDialogActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getParquetFileFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getParquetFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `guessFileEncodingForGoogleCloudStorageFile` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `listItemsInGoogleCloudStorageBucket` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetCSVFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetExcelFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetParquetFilesFromGoogleCloudStorage` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `clearGoogleDriveCacheFile` | 0% | Production | `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getCSVFileFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js`; `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getCSVFilesFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js`; `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelFileFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js`; `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelFilesFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js`; `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelSheetsFromGoogleDriveExcelFile` | 0% | Production | `src/js/actions/DataFrameActions.js`; `src/js/actions/ExcelSheetsListDialogActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getGoogleDriveFolderDetails` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `guessFileEncodingForGoogleDriveFile` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `listItemsInGoogleDrive` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `searchAndGetCSVFilesFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `searchAndGetExcelFilesFromGoogleDrive` | 0% | Production | `src/js/components/dataframe/UploadMainForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheet` | 0% | Production | `public/lib/plugins/googlesheet/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheetList` | 0% | Production | `src/js/components/widget/WriteBackStepDialog.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheetWorkSheetList` | 0% | Production | `src/js/actions/DataFrameActions.js`; `src/js/components/dataframe/PluginMixin.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getTeamDrives` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/actions/PluginActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `updateGoogleSheet` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `uploadDataToGoogleSheets` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `uploadGoogleSheet` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_trends.R` | `getGoogleTrends` | 0% | Production | `public/lib/plugins/googletrends/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/importer.R` | `nest` | 0% | Production | `public/lib/uidefinitions/nest.json`; `src/js/components/transform/CommandUIFactory.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForAnalytics` | 0% | Production | `public/lib/plugins/googleanalytics/extension.json`; `public/lib/plugins/googleanalytics/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForBigQuery` | 0% | Production | `public/lib/plugins/googlebigquery/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForCloudStorage` | 0% | Production | `public/lib/plugins/googlecloudstorage/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForDrive` | 0% | Production | `public/lib/plugins/googledrive/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForSheet` | 0% | Production | `public/lib/plugins/googlesheet/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getOAuthToken` | 0% | Production | `src/js/actions/DataFrameActions.js`; `src/js/actions/PluginActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getTwitterToken` | 0% | Production | `public/lib/plugins/twitter/extension.json`; `public/lib/plugins/twitter_timeline/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForAnalysis` | 0% | Production | `public/lib/plugins/googleanalytics/extension.json`; `public/lib/plugins/googleanalytics/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForBigQuery` | 0% | Production | `public/lib/plugins/googlebigquery/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshTwitterToken` | 0% | Production | `public/lib/plugins/twitter/extension.json`; `public/lib/plugins/twitter_timeline/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/presto.R` | `queryPresto` | 0% | Production | `public/lib/plugins/presto/extension.json`; `public/lib/plugins/treasuredata/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `loginToSalesforce` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceDataFromTable` | 0% | Production | `public/lib/plugins/salesforce/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceDataWithQuery` | 0% | Production | `public/lib/plugins/salesforce_sql/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceMetadata` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceTableDetails` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clearDBConnection` | 0% | Production | `src/js/components/dataframe/PluginDataSourceForm.react.js`; `src/js/components/dataframe/SQLEditor.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clear_cache_file` | 0% | Production | `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `convertFromJSON` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `createTempEnvironment` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `deleteGoogleBigQueryTable` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `executeGenericQuery` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `executeGoogleBigQuery` | 0% | Production | `public/lib/plugins/googlebigquery/extension.json`; `public/lib/plugins/googlebigquery_sa/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getAmazonAthenaConnection` | 0% | Production | `src/js/translator/RTranslator.js`; `src/js/util/RemoteConnectionChecker.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDBConnection` | 0% | Production | `public/lib/plugins/snowflake/extension.json`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDataFromGoogleBigQueryTable` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDataFromGoogleBigQueryTableViaCloudStorage` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryDataSets` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryProjects` | 0% | Production | `public/lib/plugins/googlebigquery/extension.json`; `public/lib/plugins/googlebigquery_sa/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryTable` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryTables` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfTables` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfTablesWithODBC` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getMongoCollectionNames` | 0% | Production | `src/js/components/dataframe/PluginDataSourceForm.react.js`; `src/js/io/DatabaseIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getMongoCollectionNumberOfRows` | 0% | Production | `src/js/components/dataframe/PluginDataSourceForm.react.js`; `src/js/components/dataframe/SQLEditor.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getObjectFromRdata` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getObjectListFromRdata` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getTwitter` | 0% | Production | `public/lib/plugins/twitter/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_excel_sheets` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `guess_csv_file_encoding` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `handleLabelledColumns` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `listGoogleCloudStorageBuckets` | 0% | Production | `src/js/actions/DataFileListDialogActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `load_fred` | 0% | Production | `public/lib/plugins/fredr/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryAmazonAthena` | 0% | Production | `public/lib/plugins/athena/extension.json`; `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryMongoDB` | 0% | Production | `public/lib/plugins/mongodb/extension.json`; `public/lib/plugins/mongodb/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryMySQL` | 0% | Production | `public/lib/plugins/aurora/extension.json`; `public/lib/plugins/aurora/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryNeo4j` | 0% | Production | `public/lib/plugins/neo4j/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryODBC` | 0% | Production | `public/lib/plugins/access/extension.json`; `public/lib/plugins/dbiodbc/extension.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryPostgres` | 0% | Production | `public/lib/plugins/postgres/extension.json`; `public/lib/plugins/postgres/scripts/plugin.r` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_excel_file_multi_sheets` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_log_file` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_parquet_files` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_raw_lines` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `saveGoogleBigQueryResultAs` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadExcelFileMultiSheets` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadParquetFiles` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setTokenInfo` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/actions/ProjectActions.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `submitGoogleBigQueryJob` | 0% | Production | `src/js/actions/GoogleBigQueryActions.js`; `src/js/io/RIO.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `toDataFrame` | 0% | Production | `src/js/translator/RTranslator.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_add_cluster_column` | 0% | Production | `src/js/components/analysis/templates/exp_textanal.json`; `src/js/components/analysis/templates/exp_textanal_pre_tokenized.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_text_cluster` | 0% | Production | `src/js/components/analysis/AnalysisUtil.js`; `src/js/components/analysis/templates/exp_text_cluster.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/toplevel_util.R` | `row_as_header` | 0% | Production | `public/lib/uidefinitions/DataFrameMenu.json`; `public/lib/uidefinitions/DataFrameMenu_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/twitter_timeline.R` | `getTwitterTimeline` | 0% | Production | `public/lib/plugins/twitter_timeline/extension.json`; `src/js/components/dataframe/GoogleAnalyticsForm.react.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `false_pct` | 0% | Production | `public/lib/uidefinitions/do_anomaly_detection.json`; `public/lib/uidefinitions/do_anomaly_detection_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `false_ratio` | 0% | Production | `src/js/components/viz/VizControlUtil.js`; `src/js/query/Column.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_week_of_month` | 0% | Production | `src/js/components/transform/RDirectory.json`; `src/js/components/transform/RDirectory_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `reorder_cols` | 0% | Production | `public/lib/uidefinitions/ColumnMenu.json`; `public/lib/uidefinitions/ColumnMenu_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sample_frac` | 0% | Production | `public/lib/uidefinitions/DataFrameMenu.json`; `public/lib/uidefinitions/DataFrameMenu_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `str_normalize` | 0% | Production | `public/lib/uidefinitions/MultiColumnMenu.json`; `public/lib/uidefinitions/mutate_multiple.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `true_pct` | 0% | Production | `public/lib/uidefinitions/do_anomaly_detection.json`; `public/lib/uidefinitions/do_anomaly_detection_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `true_ratio` | 0% | Production | `src/js/components/viz/VizControlUtil.js`; `src/js/query/Column.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/value_check.R` | `is_empty` | 0% | Production | `src/js/components/transform/RDirectory.json`; `src/js/components/transform/RDirectory_ja.json` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForSheet` | 0% | Test only | `src/js/models/Plugin.test.js` |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getAzureContainer` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getAzureEndPoint` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_confint` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getSalesforceToken` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForAnalytics` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshSalesforceToken` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_kl_dist.kv_` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/princomp.R` | `do_princomp` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `downloadDataFromGoogleCloudStorage` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `extractDataFromGoogleBigQueryToCloudStorage` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfColumns` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getTokenInfo` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `toJSON` | 0% | No hit | Matches in `tam` were `jsonlite::toJSON(...)` and helper names, not the exported `exploratory::toJSON` API. |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `do_chisq.test_` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | `any_error` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | `get_num_errors` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_f_squared` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `pivot_` | 0% | No hit | — |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `to_matrix` | 0% | No hit | — |

## Lowest-Coverage Exported Functions (Partial)

| File | Function | Coverage |
| --- | --- | ---: |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction_coxph` | 9.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | `do_apriori_` | 12.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setConnectionPoolMode` | 28.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_delim_file` | 48.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `build_coxph` | 50.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_excel_file` | 50.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_word` | 59.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_ttest_aggregated` | 61.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `full_join` | 62.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `inner_join` | 62.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `right_join` | 62.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_inside` | 64.3% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `extract_from_date` | 64.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `do_survfit` | 64.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `exp_cut` | 64.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_anova` | 65.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `parse_character` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_extract_url` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_emoji` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_url` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `case_when` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `slice_sample` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `union` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `union_all` | 66.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `fml_xgboost` | 67.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `fml_lightgbm` | 68.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `recode` | 70.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestReg` | 72.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | `exp_arima` | 73.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `scrape_html_table` | 73.3% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadDelimFiles` | 73.3% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `xgboost_reg` | 73.9% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestBinary` | 75.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestMulti` | 75.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `get_sentiment` | 75.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `is_stopword` | 75.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_model.R` | `build_model_` | 75.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadExcelFiles` | 76.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `col_name` | 77.8% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction2` | 78.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `do_tokenize_icu` | 78.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `lightgbm_binary` | 78.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `evaluate_glm_training_and_test` | 79.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/corresp.R` | `exp_mca` | 79.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `bind_rows` | 79.3% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mutate_group` | 79.3% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `parse_logical` | 80.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `add_prediction` | 80.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `recode_factor` | 80.8% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `do_prophet_` | 81.7% |

## File-by-File Coverage

| File | Functions | Covered | Uncovered | Partial | Full | Avg. function coverage |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | 112 | 49 | 63 | 25 | 24 | 38.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | 16 | 0 | 16 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | 15 | 0 | 15 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | 15 | 0 | 15 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | 14 | 0 | 14 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | 193 | 180 | 13 | 42 | 138 | 89.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | 12 | 0 | 12 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | 12 | 0 | 12 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | 8 | 1 | 7 | 0 | 1 | 12.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | 6 | 0 | 6 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | 5 | 0 | 5 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | 51 | 46 | 5 | 24 | 22 | 82.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | 30 | 26 | 4 | 18 | 8 | 72.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | 4 | 0 | 4 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | 4 | 0 | 4 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | 3 | 0 | 3 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | 16 | 13 | 3 | 8 | 5 | 75.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | 3 | 0 | 3 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | 33 | 31 | 2 | 17 | 14 | 84.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/princomp.R` | 2 | 0 | 2 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | 68 | 66 | 2 | 39 | 27 | 86.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | 25 | 24 | 1 | 15 | 9 | 83.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | 6 | 5 | 1 | 3 | 2 | 80.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_retention_cohort.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/geolocation.R` | 2 | 1 | 1 | 0 | 1 | 50.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_intercom_data.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_stripe_data.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_trends.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/importer.R` | 2 | 1 | 1 | 0 | 1 | 50.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth_wrapper.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | 6 | 5 | 1 | 2 | 3 | 80.4% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/presto.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/toplevel_util.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/twitter_timeline.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/value_check.R` | 1 | 0 | 1 | 0 | 0 | 0.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/anomaly_detection.R` | 2 | 2 | 0 | 1 | 1 | 96.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | 4 | 4 | 0 | 3 | 1 | 83.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | 4 | 4 | 0 | 3 | 1 | 71.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | 10 | 10 | 0 | 8 | 2 | 85.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_glm.R` | 3 | 3 | 0 | 3 | 0 | 82.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | 32 | 32 | 0 | 13 | 19 | 95.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_model.R` | 2 | 2 | 0 | 1 | 1 | 87.7% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_multinom.R` | 2 | 2 | 0 | 1 | 1 | 78.8% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/cluster.R` | 1 | 1 | 0 | 1 | 0 | 95.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/corresp.R` | 2 | 2 | 0 | 1 | 1 | 89.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/detect_outlier.R` | 1 | 1 | 0 | 1 | 0 | 85.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_svd.R` | 4 | 4 | 0 | 3 | 1 | 92.8% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/factanal.R` | 4 | 4 | 0 | 2 | 2 | 92.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/gam_tidiers.R` | 1 | 1 | 0 | 0 | 1 | 100.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | 8 | 8 | 0 | 5 | 3 | 82.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/kmeans.R` | 2 | 2 | 0 | 1 | 1 | 97.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/loess_tidiers.R` | 1 | 1 | 0 | 0 | 1 | 100.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_builder.R` | 4 | 4 | 0 | 3 | 1 | 94.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | 10 | 10 | 0 | 2 | 8 | 99.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/mongo_js_eval.R` | 1 | 1 | 0 | 0 | 1 | 100.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/na_util.R` | 4 | 4 | 0 | 2 | 2 | 94.2% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pair_count.R` | 2 | 2 | 0 | 0 | 2 | 100.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | 8 | 8 | 0 | 2 | 6 | 99.0% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prcomp.R` | 2 | 2 | 0 | 2 | 0 | 91.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | 7 | 7 | 0 | 5 | 2 | 86.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | 9 | 9 | 0 | 5 | 4 | 96.1% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | 36 | 36 | 0 | 15 | 21 | 90.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival.R` | 4 | 4 | 0 | 1 | 3 | 97.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | 11 | 11 | 0 | 6 | 5 | 92.5% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/ts_cluster.R` | 2 | 2 | 0 | 2 | 0 | 92.6% |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | 10 | 10 | 0 | 0 | 10 | 100.0% |

## Execution Notes

- Test files discovered: `91`
- Test files started in wrapper: `92`
- Wrapper-level test file errors: `1`
- The coverage run continued past individual test failures and warnings so coverage could be collected for the whole package.
- Coverage percentages in this report are function-level expression coverage, not branch coverage.
- The Desktop-usage scan is a static reference search, not a runtime trace.
- The Desktop-usage scan intentionally focused on first-party Desktop wiring (`src/js`, plugin/model definitions, and UI definitions) and excluded bundled library/vendor copies to reduce false positives.

### Wrapper Errors

- `2026-03-11 18:07:19 ERROR test_toplevel_util.R test_toplevel_util.R:17:62: unexpected end of file`

## Full Function Inventory

| File | Function | Exported | Coverage | Status |
| --- | --- | --- | ---: | --- |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `CMean` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `best_matches` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `best_matches_from_zoo` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `calculate_distances` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `calculate_distances_from_zoo` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `check_inputs` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `create_market_vectors` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `dw` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `inference` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `lagp` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `mape_no_zeros` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/MarketMatching.R` | `stopif` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/anomaly_detection.R` | `do_anomaly_detection` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/anomaly_detection.R` | `do_anomaly_detection_` | Yes | 92.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | `create_ts_seq` | No | 84.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | `exp_arima` | Yes | 73.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | `glance.ARIMA_exploratory` | No | 76.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arima.R` | `glance_with_ts_metric` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | `do_apriori` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | `do_apriori_` | Yes | 12.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | `do_apriori_internal` | No | 89.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/arules.R` | `get_arules_graph_data` | Yes | 84.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `clearS3CacheFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `downloadDataFileFromS3` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getCSVFileFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getCSVFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelFileFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getExcelSheetsFromS3ExcelFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getParquetFileFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getParquetFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `getS3Folders` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `guessFileEncodingForS3File` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetCSVFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetExcelFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/aws_s3.R` | `searchAndGetParquetFilesFromS3` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `downloadDataFileFromAzure` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getAzureContainer` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getAzureEndPoint` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getCSVFileFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getCSVFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelFileFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getExcelSheetsFromAzureExcelFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getParquetFileFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `getParquetFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `listAzureContainers` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `listItemsInAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetCSVFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetExcelFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/azure.R` | `searchAndGetParquetFilesFromAzure` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `add_prediction` | Yes | 80.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `add_prediction2` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `assign_cluster` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `augment_kmeans` | Yes | 92.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `augment_rowwise` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `augment_rowwise_data` | No | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `calc_conf_mat` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `cluster_info` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `do_survfit` | Yes | 64.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `evaluation` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `glance_rowwise` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `kmeans_info` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_anova` | Yes | 65.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_coef` | Yes | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_confint` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_info` | Yes | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `model_stats` | Yes | 94.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `non_single_value_colnames` | No | 71.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction` | Yes | 89.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction2` | Yes | 78.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction_binary` | Yes | 84.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction_coxph` | Yes | 9.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction_survfit` | Yes | 96.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `prediction_training_and_test` | Yes | 94.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/broom_wrapper.R` | `tidy_rowwise` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `augment.coxph_exploratory` | No | 94.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `build_coxph` | Yes | 50.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `build_coxph.fast` | Yes | 90.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `calc_efron_log_likelihood` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `calc_permutation_importance_coxph` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `get_time_unit_days` | No | 70.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `glance.coxph_exploratory` | No | 95.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `partial_dependence.coxph_exploratory` | No | 79.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `survival_pdp_sort_categorical` | No | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_coxph.R` | `tidy.coxph_exploratory` | No | 76.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_glm.R` | `augment.glm_exploratory_0` | No | 69.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_glm.R` | `build_glm` | Yes | 85.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_glm.R` | `build_lr` | Yes | 92.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `augment.lgb.Booster` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `augment.lightgbm_binary` | No | 90.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `augment.lightgbm_exp` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `augment.lightgbm_multi` | No | 40.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `augment.lightgbm_reg` | No | 96.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `calc_permutation_importance_lightgbm_binary` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `calc_permutation_importance_lightgbm_multiclass` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `calc_permutation_importance_lightgbm_regression` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `exp_lightgbm` | Yes | 88.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `expand_lightgbm_metric_aliases` | No | 86.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `extract_actual.lightgbm_exp` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `extract_predicted.lightgbm_reg` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `extract_predicted_binary_labels.lightgbm_exp` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `extract_predicted_multiclass_labels.lightgbm_multi` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `fml_lightgbm` | Yes | 68.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `get_prediction_type.lightgbm_exp` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `glance.lgb.Booster` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `glance.lightgbm_exp` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `glance.lightgbm_exp.regression` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `has_special_json_chars` | No | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `importance_lightgbm` | No | 77.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `lightgbm_binary` | Yes | 78.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `lightgbm_build_evaluation_log` | No | 51.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `lightgbm_multi` | Yes | 93.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `lightgbm_reg` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `partial_dependence.lightgbm` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `predict_lightgbm` | No | 78.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `sanitize_lightgbm_colnames` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `tidy.lgb.Booster` | No | 64.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lightgbm.R` | `tidy.lightgbm_exp` | No | 64.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `augment.glm_exploratory` | No | 89.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `augment.lm_exploratory` | No | 91.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `augment.lm_exploratory_0` | No | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `build_lm` | Yes | 87.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `build_lm.fast` | Yes | 92.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_average_marginal_effects` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_glm_test_metrics` | Yes | 92.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_permutation_importance_binomial` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_permutation_importance_gaussian` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_permutation_importance_linear` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_permutation_importance_poisson` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `calc_vif` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `evaluate_glm_training_and_test` | Yes | 79.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `evaluate_lm_training_and_test` | Yes | 95.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `find_data.glm_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `get_var_min_pvalue` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `glance.glm_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `glance.lm_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `lm_partial_dependence` | Yes | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `map_terms_to_orig` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `partial_dependence.lm_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `preprocess_regression_data_after_sample` | Yes | 89.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `preprocess_regression_data_before_sample` | Yes | 94.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `remove_outliers_for_regression_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `tidy.glm_exploratory` | No | 78.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `tidy.lm_exploratory` | No | 85.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `tidy.lm_exploratory_0` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `var_to_possible_terms_coxph` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `var_to_possible_terms_lm` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `vif` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `vif_to_dataframe` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_lm.R` | `xlevels_to_base_level_table` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_model.R` | `build_model` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_model.R` | `build_model_` | Yes | 75.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_multinom.R` | `augment.multinom` | No | 57.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_multinom.R` | `build_multinom` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `augment.xgb.Booster` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `augment.xgboost_binary` | No | 93.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `augment.xgboost_exp` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `augment.xgboost_multi` | No | 92.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `augment.xgboost_reg` | No | 91.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `calc_permutation_importance_xgboost_binary` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `calc_permutation_importance_xgboost_multiclass` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `calc_permutation_importance_xgboost_regression` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `cleanup_df_for_test` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `exp_xgboost` | Yes | 88.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_actual` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_actual.xgboost_exp` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted.xgboost_reg` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted_binary_labels` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted_binary_labels.xgboost_exp` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted_multiclass_labels` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `extract_predicted_multiclass_labels.xgboost_multi` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `fml_xgboost` | Yes | 67.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `get_prediction_type` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `get_prediction_type.xgboost_exp` | No | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `glance.xgb.Booster` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `glance.xgboost_exp` | No | 50.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `glance.xgboost_exp.regression` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `importance_xgboost` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `partial_dependence.xgboost` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `predict_xgboost` | No | 94.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `prettify_xgboost_evaluation_log` | No | 53.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `tidy.xgb.Booster` | No | 73.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `tidy.xgboost_exp` | No | 78.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `xgboost_binary` | Yes | 86.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `xgboost_multi` | Yes | 92.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/build_xgboost.R` | `xgboost_reg` | Yes | 73.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | `do_market_impact` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | `do_market_impact_` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | `glance.bsts` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/causal_impact.R` | `tidy.bsts` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/cluster.R` | `cluster` | Yes | 95.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/corresp.R` | `exp_mca` | Yes | 79.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/corresp.R` | `tidy.mca_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `execute_tidyquant` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `get_riem_measures` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/data_source_extension.R` | `riem_stations_exp` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/detect_outlier.R` | `detect_outlier` | Yes | 85.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `calc_beta_prior` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `do_bayes_ab` | Yes | 92.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `exp_bayes_ab` | Yes | 91.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `exp_bayes_ab_aggregated` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `glance.bayesTest` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_bayes_ab.R` | `tidy.bayesTest` | No | 97.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_retention_cohort.R` | `do_cohort` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_svd.R` | `do_svd` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_svd.R` | `do_svd.cols` | Yes | 91.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_svd.R` | `do_svd.kv` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/do_svd.R` | `do_svd.kv_` | Yes | 93.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/factanal.R` | `exp_factanal` | Yes | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/factanal.R` | `glance.fa_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/factanal.R` | `preprocess_factanal_data_before_sample` | No | 84.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/factanal.R` | `tidy.fa_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/gam_tidiers.R` | `glance.gam_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/geolocation.R` | `countrycode` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/geolocation.R` | `maxmind_closure` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_intercom_data.R` | `get_intercom_data` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | `access_api` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | `export_activity` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | `export_members` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_mailchimp_data.R` | `get_mailchimp_data` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/get_stripe_data.R` | `get_stripe_data` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalytics` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalyticsSegmentList` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalyticsTimeZoneInfo` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleAnalyticsV4Property` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `getGoogleProfile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_analytics.R` | `parse_webproperty_list` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `clearGoogleCloudStorageCacheFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `downloadDataFileFromGoogleCloudStorage` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `gcs_list_objects_fixed` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getCSVFileFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getCSVFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelFileFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getExcelSheetsFromGoogleCloudStorageExcelFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getParquetFileFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `getParquetFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `guessFileEncodingForGoogleCloudStorageFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `listItemsInGoogleCloudStorageBucket` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetCSVFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetExcelFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_cloud_storage.R` | `searchAndGetParquetFilesFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `clearGoogleDriveCacheFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `downloadDataFileFromGoogleDrive` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getCSVFileFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getCSVFilesFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelFileFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelFilesFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getExcelSheetsFromGoogleDriveExcelFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `getGoogleDriveFolderDetails` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `guessFileEncodingForGoogleDriveFile` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `listItemsInGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `searchAndGetCSVFilesFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_drive.R` | `searchAndGetExcelFilesFromGoogleDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheet` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheetList` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getGoogleSheetWorkSheetList` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `getTeamDrives` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `normalizeDataForGoogleSheetsExport` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `updateGoogleSheet` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `uploadDataToGoogleSheets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_sheets.R` | `uploadGoogleSheet` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/google_trends.R` | `getGoogleTrends` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/importer.R` | `anonymize` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/importer.R` | `nest` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `anti_join` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `cross_join` | Yes | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `full_join` | Yes | 62.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `inner_join` | Yes | 62.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `insensitive_join` | No | 89.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `left_join` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `right_join` | Yes | 62.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/join.R` | `semi_join` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/kmeans.R` | `exp_kmeans` | Yes | 94.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/kmeans.R` | `iterate_kmeans` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/loess_tidiers.R` | `glance.loess` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_builder.R` | `build_kmeans` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_builder.R` | `build_kmeans.cols` | Yes | 92.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_builder.R` | `build_kmeans.kv` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_builder.R` | `build_kmeans.kv_` | Yes | 97.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `do_roc` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `do_roc_` | Yes | 93.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `do_survival_roc_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_binary` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_binary_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_binary_training_and_test` | Yes | 96.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_multi` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_multi_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_regression` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/model_eval.R` | `evaluate_regression_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/mongo_js_eval.R` | `jsToMongoJson` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/na_util.R` | `fill_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/na_util.R` | `fill_between_v` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/na_util.R` | `fill_ts_na` | Yes | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/na_util.R` | `impute_na` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForAnalytics` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForBigQuery` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForDrive` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getGoogleTokenForSheet` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getOAuthToken` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getSalesforceToken` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `getTwitterToken` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForAnalysis` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForAnalytics` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForBigQuery` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForCloudStorage` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForDrive` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshGoogleTokenForSheet` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshSalesforceToken` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth.R` | `refreshTwitterToken` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/oauth_wrapper.R` | `initialize` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pair_count.R` | `pair_count` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pair_count.R` | `pair_count_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_cosine_sim.kv` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_dist` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_dist.cols` | Yes | 96.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_dist.kv` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_dist.kv_` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/pairwise.R` | `do_kl_dist.kv_` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `calc_firm_from_pd` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `calc_partial_binning_data` | No | 95.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `capture_df_column_classes` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `count_occurrences` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `handle_partial_dependence` | No | 95.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `importance_firm` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `sd_with_weight` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/partial_dependence.R` | `shrink_partial_dependence_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prcomp.R` | `do_prcomp` | Yes | 85.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prcomp.R` | `tidy.prcomp_exploratory` | No | 96.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/presto.R` | `queryPresto` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/princomp.R` | `do_princomp` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/princomp.R` | `tidy.princomp_exploratory` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `add_country_holidays` | No | 72.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `do_prophet` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `do_prophet_` | Yes | 81.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `glance.prophet_exploratory` | No | 80.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `is_na_rm_func` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `tidy.prophet_exploratory` | No | 80.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/prophet.R` | `trim_future` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `align_predictor_factor_levels` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.randomForest.classification` | No | 22.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.randomForest.formula` | No | 71.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.randomForest.regression` | No | 21.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.randomForest.unsupervised` | No | 94.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.ranger` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.ranger.classification` | No | 95.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.ranger.regression` | No | 96.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.rpart` | No | 60.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.rpart.classification` | No | 73.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `augment.rpart.regression` | No | 96.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `calc_feature_imp` | Yes | 93.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `calc_permutation_importance_rpart_binary` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `calc_permutation_importance_rpart_multiclass` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `calc_permutation_importance_rpart_regression` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `cleanup_df` | No | 88.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `cleanup_df_per_group` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `evaluate_binary_classification` | No | 95.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `evaluate_classification` | Yes | 96.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `exp_balance` | Yes | 92.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `exp_rpart` | Yes | 92.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `extract_importance_history_from_boruta` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `extract_important_variables_from_boruta` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_actual_class_rpart` | No | 77.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_binary_predicted_probability_rpart` | No | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_class_levels_rpart` | No | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_classification_type` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_multiclass_predicted_probability_rpart` | No | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_multiclass_predicted_value_from_probability_rpart` | No | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_predicted_class_rpart` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `get_predicted_probability_rpart` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.Boruta_exploratory` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.randomForest.classification` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.randomForest.formula` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.randomForest.regression` | No | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.randomForest.unsupervised` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.ranger` | No | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.ranger.classification` | No | 96.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.ranger.regression` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `glance.rpart` | No | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `importance_ranger` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `partial_dependence.ranger` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `partial_dependence.rpart` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `predict_value_from_prob` | No | 86.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestBinary` | Yes | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestMulti` | Yes | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `randomForestReg` | Yes | 72.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `ranger.find_na` | No | 80.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `ranger.find_na_index` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `ranger.set_multi_predicted_values` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rangerBinary` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rangerCore` | No | 90.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rangerMulti` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rangerReg` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rename_groups` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rf_evaluation` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rf_evaluation_by_class` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rf_evaluation_training_and_test` | Yes | 94.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rf_importance` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `rf_partial_dependence` | Yes | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.Boruta_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.randomForest.classification` | No | 51.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.randomForest.formula` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.randomForest.regression` | No | 64.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.randomForest.unsupervised` | No | 77.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.ranger` | No | 89.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `tidy.rpart` | No | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/randomForest_tidiers.R` | `ubSMOTE2` | No | 97.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `loginToSalesforce` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceDataFromTable` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceDataWithQuery` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceMetadata` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/salesforce.R` | `querySalesforceTableDetails` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cmdscale` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cmdscale_` | Yes | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cor` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cor.cols` | Yes | 96.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cor.kv` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cor.kv_` | Yes | 91.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `do_cor_internal` | No | 96.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `normalize` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/stats_wrapper.R` | `tidy.cor_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `do_ngram` | Yes | 96.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `do_tfidf` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `do_tokenize` | Yes | 86.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `do_tokenize_icu` | Yes | 78.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `get_emoji_regex` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `get_sentiment` | Yes | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `get_stopwords` | Yes | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `is_alphabet` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `is_digit` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `is_stopword` | Yes | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `parse_character` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `parse_logical` | Yes | 80.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `parse_number` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `stem_word` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_detect` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_extract_after` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_extract_before` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_extract_inside` | Yes | 92.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_extract_url` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_logical` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_after` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_all` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_before` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_emoji` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_inside` | Yes | 92.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_range` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_url` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_remove_word` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_after` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_before` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_inside` | Yes | 64.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_range` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_url` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `str_replace_word` | Yes | 59.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/string_operation.R` | `word_to_sentiment` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival.R` | `exp_survival` | Yes | 90.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival.R` | `glance.survdiff_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival.R` | `tidy.survdiff_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival.R` | `tidy.survfit_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `augment.ranger_survival_exploratory` | No | 97.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `calc_mean_survival` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `calc_permutation_importance_ranger_survival` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `calc_survival_curves_with_strata` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `exp_survival_forest` | Yes | 87.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `extract_survival_rate_at` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `glance.ranger_survival_exploratory` | No | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `partial_dependence.ranger_survival_exploratory` | No | 78.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `survival_rate_to_predicted_time` | No | 78.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `survival_time_to_predicted_rate` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/survival_forest.R` | `tidy.ranger_survival_exploratory` | No | 89.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `bigquery_glue_transformer` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `checkSourceConflict` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `city_code_japan` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clean_data_frame` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clean_names` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clearAmazonAthenaConnection` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clearDBConnection` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `clear_cache_file` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `convertFromJSON` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `convertToJSON` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `convertUserInputToUtf8` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `countycode` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `createAmazonAthenaConnectionString` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `createTempEnvironment` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `deleteGoogleBigQueryTable` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `downloadDataFromGoogleCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `download_data_file` | No | 73.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `executeGenericQuery` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `executeGoogleBigQuery` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `exp_cut` | Yes | 64.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `exp_cut_by_step` | Yes | 97.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `extractDataFromGoogleBigQueryToCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `filter_cascade` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `generate_random_string` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `geocode_japan_city` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `geocode_japan_prefecture` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `geocode_us_county` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `geocode_us_state` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `geocode_world_country` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getAmazonAthenaConnection` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getConnectionObject` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getConnectionPoolMode` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDBConnection` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDataFromGoogleBigQueryTable` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDataFromGoogleBigQueryTableViaCloudStorage` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getDownloadedFilePath` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGithubIssues` | Yes | 82.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryDataSets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryProjects` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryTable` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getGoogleBigQueryTables` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfColumns` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfTables` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getListOfTablesWithODBC` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getMongoCollectionNames` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getMongoCollectionNumberOfRows` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getMongoURL` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getObjectFromRdata` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getObjectListFromRdata` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getTokenInfo` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `getTwitter` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_excel_sheets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_refs_in_call` | No | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_refs_in_call_args_after_pipe` | No | 93.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_refs_in_call_args_basic` | No | 73.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_refs_in_script` | Yes | 81.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `get_variable_config` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `glob_to_regex` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `glue_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `glue_salesforce` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `glue_salesforce_internal` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `guess_csv_file_encoding` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `handleLabelledColumns` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `isConnecitonPoolEnabled` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `isNDJSON` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `js_glue_transformer` | No | 73.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `listGoogleCloudStorageBuckets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `load_fred` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `parse_html_tables` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `prefecturecode` | Yes | 94.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryAmazonAthena` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryMongoDB` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryMySQL` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryNeo4j` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryODBC` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `queryPostgres` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `readPasswordRDS` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_delim_file` | Yes | 48.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_delim_files` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_excel_file` | Yes | 50.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_excel_file_multi_sheets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_excel_files` | Yes | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_log_file` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_parquet_file` | Yes | 81.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_parquet_file_internal` | No | 62.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_parquet_files` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_raw_lines` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `read_rds_file` | Yes | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `resolve_milestone` | No | 81.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `salesforce_glue_transformer` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `saveGoogleBigQueryResultAs` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `saveOrReadPassword` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `savePasswordRDS` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `scrape_html_table` | Yes | 73.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadDelimFiles` | Yes | 73.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadExcelFileMultiSheets` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadExcelFiles` | Yes | 76.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `searchAndReadParquetFiles` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `select_columns` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setConnectionPoolMode` | Yes | 28.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setDownloadedFilePath` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setOAuthTokenCacheOptions` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `setTokenInfo` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `sql_glue_transformer` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `sql_glue_transformer_internal` | No | 76.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `statecode` | Yes | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `submitGoogleBigQueryJob` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `toDataFrame` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `toJSON` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `typeConvert` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `unnest_safe` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/system.R` | `within_date_range` | Yes | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_pooled_stderr` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_student_confint` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_student_dof` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_student_p` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_student_t` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_welch_confint` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_welch_dof` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_welch_p` | No | 90.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_welch_stderr` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `calculate_welch_t` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `do_chisq.test` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `do_chisq.test_` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `do_t.test` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `do_var.test` | Yes | 96.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_anova` | Yes | 94.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_chisq` | Yes | 85.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_chisq_ab_aggregated` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_chisq_power` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_chisq_power_for_ab_test` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_kruskal` | Yes | 85.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_normality` | Yes | 97.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_ttest` | Yes | 95.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_ttest_aggregated` | Yes | 61.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_ttest_power` | Yes | 97.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `exp_wilcox` | Yes | 86.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `gather_repeated_measures` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_chisq_density_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_chisq_density_data_for_power` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_ftest_density_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_norm_density_data` | No | 68.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_signrank_density_data` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_ttest_density_data` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_ttest_density_data_for_power` | No | 86.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `generate_wilcox_density_data` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `get_gather_repeated_measures_colnames` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `get_pairwise_contrast_df` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `glance.anova_exploratory` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `glance.chisq_exploratory` | No | 78.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `glance.ttest_exploratory` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `qqline_data` | No | 77.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `t.test.aggregated` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.anova_exploratory` | No | 88.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.chisq_exploratory` | No | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.chisq_power_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.kruskal_exploratory` | No | 97.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.shapiro_exploratory` | No | 92.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.ttest_exploratory` | No | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.ttest_power_exploratory` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `tidy.wilcox_exploratory` | No | 92.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `wilcox_norm_dist_mean` | No | 23.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/test_wrapper.R` | `wilcox_norm_dist_sd` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `dfm_to_df` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_add_cluster_column` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_get_top5_sentences_for_cluster` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_text_cluster` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_textanal` | Yes | 90.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `exp_topic_model` | Yes | 95.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `fcm_to_df` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `get_cooccurrence_graph_data` | Yes | 96.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `guess_lang_for_stopwords` | No | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `parse_comma_separated_tokens` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `tidy.text_cluster_exploratory` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `tidy.textanal_exploratory` | No | 78.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `tidy.textmodel_lda_exploratory` | No | 97.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `tokenize_with_postprocess` | No | 84.6% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `uncompress_csr_index` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/textanal.R` | `which.max.safe` | No | 75.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | `any_error` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | `generate_examples` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/tools.R` | `get_num_errors` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/toplevel_util.R` | `row_as_header` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/ts_cluster.R` | `exp_ts_cluster` | Yes | 87.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/ts_cluster.R` | `tidy.PartitionalTSClusters_exploratory` | No | 98.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/twitter_timeline.R` | `getTwitterTimeline` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_domain` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_fragment` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_param` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_parameters` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_path` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_port` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_scheme` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_subdomain` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_suffix` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/url_operation.R` | `url_tld` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%equal_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%greater_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%greater_or_equal_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%in_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%less_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%less_or_equal_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%nin%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `%not_equal_or_all%` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `add_confint` | No | 91.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `add_response` | No | 80.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `adjusted_r_squared` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `aggregate_if` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `append_colnames` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `as_numeric_matrix_` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `auroc` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `average_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `average_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `average_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `avoid_conflict` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `binary_label` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `bind_expr` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `bind_rows` | Yes | 79.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `bind_rows_safe` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calc_confint_mean` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calc_confint_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_d` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_d_aggregated` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_f` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_f_squared` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_w` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_cohens_w_for_ab_test` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_common_sd` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_common_sd_aggregated` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `calculate_epsilon_squared` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `case_when` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `categorize_numeric` | No | 71.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `ceiling` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `col_name` | Yes | 77.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `column_mutate_quosure` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `complete_date` | No | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `computeMASE` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `confint_radius` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `construct_new_labels` | No | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_rows` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_unique_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_unique_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `count_unique_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `create_model_meta` | No | 94.1% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cumall` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cumany` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cummax` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cummean` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cummin` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cumprod` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cumsum` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `cumsum_decayed` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `days_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `do_on_each_group` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `do_on_each_group_2` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `evaluate_select` | Yes | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `excel_numeric_to_date` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `excel_numeric_to_datetime` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `expand_args` | Yes | 92.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `extract_argument_names` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `extract_from_date` | Yes | 64.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `extract_from_numeric` | Yes | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `extract_numeric` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `factorize_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `false_count` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `false_pct` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `false_ratio` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `fill_mat_NA` | No | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `fill_vec_NA` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `floor` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `format_cut_output` | Yes | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `format_cut_output_levels` | No | 64.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_average_moving_range` | Yes | 85.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_confint` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_data_type` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_mode` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_multi_predicted_values` | No | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_optimized_score` | No | 93.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_row_numbers_from_index_vector` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_score` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_unique_values` | No | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_unknown_category_rows_index_vector` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `get_week_of_month` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `grouped_by` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `hours_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `inter_group_population_var` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `intersect` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `intra_group_population_var` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `is_integer` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `is_japanese_holiday` | Yes | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `last_date` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `likert_sigma` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `list_concat` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `list_extract` | Yes | 94.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `list_n` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `list_to_text` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mae` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `map_platform_locale` | No | 91.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mape` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mase` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mat_to_df` | Yes | 92.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `max_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `max_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `max_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mean_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mean_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mean_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `median_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `median_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `median_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `merge_sds` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `merge_vars` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `min_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `min_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `min_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `minutes_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `months_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `move_col` | Yes | 86.4% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mutate_group` | Yes | 79.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `mutate_predictors` | No | 90.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `na_count` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `na_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `na_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `non_na_count` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `non_na_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `non_na_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `one_hot` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `pivot` | Yes | 88.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `pivot_` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `pivot_longer` | Yes | 88.9% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `pivot_wider` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `prop_confint_radius` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `quantifiable_cols` | No | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `r_squared` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `recode` | Yes | 70.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `recode_factor` | Yes | 80.8% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `rename_with` | Yes | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `reorder_cols` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `restore_na` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `revert_factor_cols_to_logical` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `rmse` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `safe_slice` | Yes | 87.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sample_df_index` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sample_frac` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sample_n` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sample_rows` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `seconds_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `separate_japanese_address` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `set_operation_with_force_character` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `setdiff` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `simple_cast` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `slice_sample` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sparse_cast` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `str_clean` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `str_count_all` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `str_normalize` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sum_if` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sum_if_pct` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `sum_if_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `summarize_group` | Yes | 88.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `summarize_row` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `survival_auroc` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `time_between` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `to_matrix` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `to_same_type` | No | 68.2% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `to_time_unit_for_seq` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `true_count` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `true_pct` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `true_ratio` | Yes | 0.0% | Uncovered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `ts_diff` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `ts_diff_ratio` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `ts_lag` | Yes | 95.0% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `union` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `union_all` | Yes | 66.7% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `unixtime_to_datetime` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `unnest_with_drop` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `unnest_without_empty` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `unnest_without_empty_` | Yes | 83.3% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `upper_gather` | Yes | 92.5% | Partially covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `validate_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `validate_empty_data` | No | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `week` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `weekend` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `weeks_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/util.R` | `years_between` | Yes | 100.0% | Fully covered |
| `/Users/hidekoji/Work/gitrepo/exploratory_func/R/value_check.R` | `is_empty` | Yes | 0.0% | Uncovered |
