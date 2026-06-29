context("read_pdf_table")

# These tests exercise the input-validation and metadata-selection logic of
# read_pdf_table WITHOUT requiring docling / a real PDF: we stub the docling
# subprocess by pointing EXPLORATORY_DOCLING_SCRIPT at a fake script and writing a
# metadata.json + CSVs ourselves, or by asserting the early validation errors.

test_that("read_pdf_table errors when pdf_path is missing or empty", {
  expect_error(read_pdf_table(NULL), "single pdf_path")
  expect_error(read_pdf_table(""), "single pdf_path")
  expect_error(read_pdf_table(c("a", "b")), "single pdf_path")
})

test_that("read_pdf_table errors when the PDF file does not exist", {
  expect_error(read_pdf_table("/no/such/file.pdf"), "PDF file not found")
})

test_that("read_pdf_table errors when EXPLORATORY_DOCLING_SCRIPT is unset", {
  pdf <- tempfile(fileext = ".pdf")
  writeLines("dummy", pdf)
  old <- Sys.getenv("EXPLORATORY_DOCLING_SCRIPT", unset = NA)
  Sys.unsetenv("EXPLORATORY_DOCLING_SCRIPT")
  on.exit({
    if (!is.na(old)) Sys.setenv(EXPLORATORY_DOCLING_SCRIPT = old)
    unlink(pdf)
  }, add = TRUE)
  expect_error(read_pdf_table(pdf), "docling script path is not set")
})

# Helper: stub the package-internal python resolver so tests need neither a real Python
# nor docling. We point it at a no-op executable (`true`) that ignores all args and
# exits 0; the test pre-stages metadata.json + CSVs so the "subprocess" is a success.
# resolve_docling_python exists in the package namespace, so unlockBinding works.
noop_exe <- function() {
  p <- as.character(Sys.which("true"))
  if (is.null(p) || p == "") p <- "/usr/bin/true"
  p
}

stub_docling_resolver <- function() {
  ns <- asNamespace("exploratory")
  orig_resolver <- get("resolve_docling_python", envir = ns)
  unlockBinding("resolve_docling_python", ns)
  assign("resolve_docling_python", function() noop_exe(), envir = ns)
  function() {
    assign("resolve_docling_python", orig_resolver, envir = ns)
    lockBinding("resolve_docling_python", ns)
  }
}

test_that("resolve_docling_python quotes the Python -c probe as one argument", {
  source_candidates <- c(file.path("R", "pdf.R"),
                         file.path("..", "..", "R", "pdf.R"))
  source_path <- source_candidates[file.exists(source_candidates)][1]
  if (is.na(source_path)) {
    skip("R/pdf.R source file is not available")
  }

  src <- readLines(source_path, warn = FALSE)
  expect_true(any(grepl("system2\\(resolved, c\\(\"-c\", shQuote\\(check_code\\)\\)",
                        src)))
})

test_that("read_pdf_table validates a negative / non-integer table_index before running", {
  pdf <- tempfile(fileext = ".pdf")
  writeLines("dummy", pdf)
  fake_script <- tempfile(fileext = ".py")
  writeLines("noop", fake_script)
  old <- Sys.getenv("EXPLORATORY_DOCLING_SCRIPT", unset = NA)
  Sys.setenv(EXPLORATORY_DOCLING_SCRIPT = fake_script)
  restore <- stub_docling_resolver()
  on.exit({
    restore()
    if (!is.na(old)) Sys.setenv(EXPLORATORY_DOCLING_SCRIPT = old) else Sys.unsetenv("EXPLORATORY_DOCLING_SCRIPT")
    unlink(c(pdf, fake_script))
  }, add = TRUE)
  expect_error(read_pdf_table(pdf, table_index = -1), "non-negative integer")
  expect_error(read_pdf_table(pdf, table_index = "x"), "non-negative integer")
})

test_that("read_pdf_table reads the chosen table from a stubbed docling run", {
  pdf <- tempfile(fileext = ".pdf")
  writeLines("dummy", pdf)
  out_dir <- tempfile("exp_docling_test_")
  dir.create(out_dir)
  fake_script <- tempfile(fileext = ".py")
  writeLines("noop", fake_script)

  # Stress-test column names: spaces, multibyte, symbols (workflow rule 7).
  hard_col <- "航空 会社 !#$%&'()"
  csv0 <- file.path(out_dir, "doc_table0.csv")
  csv1 <- file.path(out_dir, "doc_table1.csv")
  readr::write_csv(data.frame(a = 1:2, b = c("x", "y")), csv0)
  df1 <- data.frame(v = c(10, 20))
  names(df1) <- hard_col
  readr::write_csv(df1, csv1)

  meta <- list(
    n_tables = 2,
    tables = list(
      list(index = 0, path = csv0, nrow = 2, ncol = 2),
      list(index = 1, path = csv1, nrow = 2, ncol = 1)
    )
  )
  jsonlite::write_json(meta, file.path(out_dir, "metadata.json"), auto_unbox = TRUE)

  old_script <- Sys.getenv("EXPLORATORY_DOCLING_SCRIPT", unset = NA)
  Sys.setenv(EXPLORATORY_DOCLING_SCRIPT = fake_script)
  # The metadata.json + CSVs are pre-staged, so the no-op `true` subprocess succeeds.
  restore <- stub_docling_resolver()
  on.exit({
    restore()
    if (!is.na(old_script)) Sys.setenv(EXPLORATORY_DOCLING_SCRIPT = old_script) else Sys.unsetenv("EXPLORATORY_DOCLING_SCRIPT")
    unlink(c(pdf, fake_script), force = TRUE)
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # table_index = 1 -> second table with the hard column name.
  res <- read_pdf_table(pdf, table_index = 1, out_dir = out_dir)
  expect_equal(nrow(res), 2)
  expect_true(hard_col %in% colnames(res))

  # default (NULL) -> first table.
  res0 <- read_pdf_table(pdf, out_dir = out_dir)
  expect_equal(colnames(res0), c("a", "b"))

  # out-of-range index -> clear error.
  expect_error(read_pdf_table(pdf, table_index = 5, out_dir = out_dir), "not found")
})
