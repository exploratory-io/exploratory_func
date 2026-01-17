# Perl Regex Support for File Search Functions

## Problem

The file search functions `searchAndReadDelimFiles`, `searchAndReadExcelFiles`, and `searchAndReadParquetFiles` fail when using Perl-style regex patterns like negative lookaheads.

Example failing pattern:
```
^(?!Chart).*\.(csv|tsv|txt|text|tab)$
```

Error:
```
Error in grep(x = path, pattern = regexp, value = TRUE, invert = isTRUE(invert),
  invalid regular expression, reason 'Invalid regexp'
```

## Root Cause

`fs::dir_ls` uses R's base `grep` internally, which doesn't support Perl regex features by default.

## Solution

Replace the single-line `fs::dir_ls` call with a two-step approach:
1. Get all files with `fs::dir_ls` (no regexp filtering)
2. Filter with `grep(..., perl = TRUE)` to enable Perl regex support

### Before
```r
files <- fs::dir_ls(path = folder, regexp = stringr::str_c("(?i)", pattern))
```

### After
```r
files <- fs::dir_ls(path = folder)
if (pattern != "") {
  files <- grep(pattern, files, perl = TRUE, value = TRUE, ignore.case = TRUE)
}
```

## Affected Functions

| Function | File | Line |
|----------|------|------|
| `searchAndReadExcelFiles` | R/system.R | 3305 |
| `searchAndReadDelimFiles` | R/system.R | 3556 |
| `searchAndReadParquetFiles` | R/system.R | 3848 |

## Testing

Manual verification with the original failing query:
```r
exploratory::searchAndReadDelimFiles(
  folder = "/Users/hidekoji/Downloads/import_tests",
  pattern = "^(?!Chart).*\\.(csv|tsv|txt|text|tab)$",
  ...
)
```
