# PDF table extraction via docling.
#
# Extraction runs in a dedicated Python subprocess (docling_extract.py shipped with
# exp-cli), NOT via reticulate inside the embedded R/RServe session. Running docling
# inline deadlocks: docling/torch/RapidOCR are extremely noisy on stdout/stderr and the
# embedded R session does not drain those pipes, so the child blocks once a pipe fills.
# The subprocess writes one CSV per table plus a metadata.json into out_dir; we read the
# CSV, never parse stdout, so docling log noise can never corrupt the result.
#
# The Desktop app injects EXPLORATORY_DOCLING_SCRIPT (absolute path to docling_extract.py
# inside exp-cli) into the R session at startup. The Python interpreter is resolved here
# (EXPLORATORY_PYTHON_BIN override, else python3 / python, each verified to import docling).

# Resolve a Python interpreter that has docling importable.
# Order: EXPLORATORY_PYTHON_BIN (if it imports docling), then python3, then python.
# Returns the interpreter command/path, or stops with an actionable message.
resolve_docling_python <- function() {
  can_import_docling <- function(py) {
    if (is.null(py) || is.na(py) || py == "") {
      return(FALSE)
    }
    # Resolve to an absolute path when the value is a bare command name.
    resolved <- if (file.exists(py)) py else as.character(Sys.which(py))
    if (is.null(resolved) || resolved == "") {
      return(FALSE)
    }
    status <- tryCatch(
      # shQuote the -c code: system2 pastes args into a shell command line WITHOUT
      # quoting, so an unquoted "import docling" is word-split by the shell and python
      # runs `-c import` -> SyntaxError -> docling always reported missing (#36434).
      suppressWarnings(system2(resolved, c("-c", shQuote("import docling")),
                               stdout = FALSE, stderr = FALSE)),
      error = function(e) 1L
    )
    isTRUE(status == 0)
  }

  candidates <- c(Sys.getenv("EXPLORATORY_PYTHON_BIN", ""), "python3", "python")
  for (candidate in candidates) {
    if (can_import_docling(candidate)) {
      return(if (file.exists(candidate)) candidate else as.character(Sys.which(candidate)))
    }
  }
  stop(paste0(
    "Could not find a Python interpreter with docling installed. ",
    "Install it from the AI Data PDF flow (install_docling), or set ",
    "EXPLORATORY_PYTHON_BIN to a Python (>=3.11) that has docling."
  ))
}

#' Extract a table from a PDF into a data frame via docling.
#'
#' Runs docling in a dedicated Python subprocess (shipped with exp-cli) to extract
#' tables from a PDF and reads the chosen table back as a data frame. Because the whole
#' extraction happens on each call, an Update / Re-import of the data source re-reads the
#' live PDF (unlike reading a pre-extracted CSV).
#'
#' @export
#' @param pdf_path Path to the PDF file.
#' @param table_index 0-based index of the table to extract. NULL (default) extracts all
#'   tables and returns the first one (a message lists how many were found).
#' @param out_dir Directory for the per-table CSVs + metadata.json. A fresh temp dir by
#'   default; re-extracted on every call so persistence is unnecessary.
#' @param encoding Encoding passed to readr when reading the CSV. Default "UTF-8".
#' @param timeout Seconds before the docling subprocess is aborted. Default 600.
#' @return A data frame (tibble) for the chosen table.
read_pdf_table <- function(pdf_path, table_index = NULL, out_dir = NULL,
                           encoding = "UTF-8", timeout = 600) {
  if (is.null(pdf_path) || length(pdf_path) != 1 || is.na(pdf_path) || pdf_path == "") {
    stop("read_pdf_table requires a single pdf_path.")
  }
  if (!file.exists(pdf_path)) {
    stop(paste0("PDF file not found: ", pdf_path))
  }

  script <- Sys.getenv("EXPLORATORY_DOCLING_SCRIPT", "")
  if (script == "" || !file.exists(script)) {
    stop(paste0(
      "PDF extraction is unavailable: the docling script path is not set ",
      "(EXPLORATORY_DOCLING_SCRIPT). Update Exploratory Desktop, or run the AI Data PDF ",
      "flow once so docling is installed."
    ))
  }

  python <- resolve_docling_python()

  if (is.null(out_dir) || out_dir == "") {
    out_dir <- tempfile("exp_docling_")
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  args <- c(script, pdf_path, out_dir)
  if (!is.null(table_index)) {
    # Validate up front so a bad index fails with a clear message, not a python traceback.
    idx <- suppressWarnings(as.integer(table_index))
    if (is.na(idx) || idx < 0) {
      stop(paste0("table_index must be a non-negative integer (0-based). Got: ",
                  table_index))
    }
    args <- c(args, as.character(idx))
  }

  # Drain stdout/stderr (do NOT discard at fd level -- that can break torch init) but
  # keep them only for an error message; the authoritative result is metadata.json.
  # shQuote every arg: system2 pastes them into a shell command line without quoting,
  # so a script/PDF/out-dir path containing a space (or the bare args themselves) would
  # be word-split by the shell (#36434).
  output <- tryCatch(
    suppressWarnings(system2(python, shQuote(args), stdout = TRUE, stderr = TRUE,
                             timeout = timeout)),
    error = function(e) {
      stop(paste0("PDF table extraction failed to run: ", conditionMessage(e)))
    }
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    tail_lines <- paste(utils::tail(output, 20), collapse = "\n")
    stop(paste0("PDF table extraction failed (python exited with code ", status, ").\n",
                tail_lines))
  }

  meta_path <- file.path(out_dir, "metadata.json")
  if (!file.exists(meta_path)) {
    stop("PDF table extraction produced no metadata.json.")
  }
  meta <- jsonlite::fromJSON(meta_path, simplifyVector = FALSE)

  tables <- meta$tables
  if (is.null(tables) || length(tables) == 0) {
    stop("No tables were found in the PDF.")
  }

  # Choose the table: explicit index (match metadata's `index` field) or the first one.
  chosen <- NULL
  if (!is.null(table_index)) {
    idx <- as.integer(table_index)
    for (t in tables) {
      if (!is.null(t$index) && as.integer(t$index) == idx) {
        chosen <- t
        break
      }
    }
    if (is.null(chosen)) {
      stop(paste0("table_index ", idx, " not found. The PDF has ", length(tables),
                  " table(s) (0-based indices 0..", length(tables) - 1, ")."))
    }
  } else {
    if (length(tables) > 1) {
      message(paste0(length(tables), " tables were found in the PDF; reading the first. ",
                     "Pass table_index (0-based) to choose another."))
    }
    chosen <- tables[[1]]
  }

  csv_path <- chosen$path
  if (is.null(csv_path) || csv_path == "" || !file.exists(csv_path)) {
    stop(paste0("The extracted CSV for the chosen table was not found: ",
                if (is.null(csv_path)) "<none>" else csv_path))
  }

  readr::read_csv(csv_path, locale = readr::locale(encoding = encoding),
                  show_col_types = FALSE)
}
