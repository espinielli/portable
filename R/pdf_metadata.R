#' Document metadata for a PDF
#'
#' `pdf_metadata()` collects the document information written to a PDF
#' produced by [portable_pdf()]. All fields are optional; empty fields are
#' simply omitted from the document information dictionary.
#'
#' @param title,author,subject Document title, author, and subject.
#' @param keywords Character vector of keywords, stored comma-separated.
#' @param creator Name of the software that created the document. Set to `""`
#'   to omit.
#' @param creation_date A `POSIXct` timestamp recorded as the document's
#'   creation date, or `NULL` to use the time the file is written. Setting a
#'   fixed date makes builds reproducible.
#'
#' @return A `portable_pdf_metadata` object to pass to the `metadata`
#'   argument of [portable_pdf()].
#'
#' @examples
#' pdf_metadata(title = "My report", author = "Jane",
#'              keywords = c("sales", "2026"))
#'
#' @export
pdf_metadata <- function(title = "", author = "", subject = "",
                         keywords = character(),
                         creator = "R graphics device (portable)",
                         creation_date = NULL) {
  check_string(title)
  check_string(author)
  check_string(subject)
  check_character(keywords)
  check_string(creator)
  if (!is.null(creation_date)) {
    if (!inherits(creation_date, "POSIXct") || length(creation_date) != 1) {
      cli_abort("{.arg creation_date} must be a single {.cls POSIXct} or {.code NULL}")
    }
  }
  structure(
    list(
      title = title,
      author = author,
      subject = subject,
      keywords = paste(keywords, collapse = ", "),
      creator = creator,
      creation_date = if (is.null(creation_date)) "" else
        format_pdf_date(creation_date)
    ),
    class = "portable_pdf_metadata"
  )
}

# PDF date format: D:YYYYMMDDHHmmSS with an optional timezone suffix
format_pdf_date <- function(x) {
  base <- format(x, "D:%Y%m%d%H%M%S")
  off <- format(x, "%z")
  if (off == "") return(base)
  if (off == "+0000") return(paste0(base, "Z"))
  paste0(base, substr(off, 1, 3), "'", substr(off, 4, 5), "'")
}

#' @export
print.portable_pdf_metadata <- function(x, ...) {
  cat("<portable_pdf_metadata>\n")
  fields <- c("title", "author", "subject", "keywords", "creator")
  for (f in fields) {
    if (nzchar(x[[f]])) cat(" ", f, ": ", x[[f]], "\n", sep = "")
  }
  cat("  creation_date: ",
      if (nzchar(x$creation_date)) x$creation_date else "<time of writing>",
      "\n", sep = "")
  invisible(x)
}
