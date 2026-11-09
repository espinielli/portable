# Decompress all FlateDecode streams in a PDF file, returning them as a
# list of latin1 strings
pdf_streams <- function(file) {
  raw <- readBin(file, "raw", file.size(file))
  starts <- grepRaw("\\bstream\r?\n", raw, all = TRUE)
  ends <- grepRaw("\r?\nendstream", raw, all = TRUE)
  stopifnot(length(starts) == length(ends))
  out <- list()
  for (i in seq_along(starts)) {
    # skip past the newline terminating "stream"
    s <- starts[i] + 6L
    if (raw[s] == charToRaw("\r")) s <- s + 1L
    if (raw[s] == charToRaw("\n")) s <- s + 1L
    body <- raw[s:(ends[i] - 1L)]
    decompressed <- tryCatch(
      memDecompress(body, "gzip"),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    # Only keep text streams (content); embedded font data contains nuls
    if (!is.null(decompressed) && !any(decompressed == as.raw(0))) {
      out[[length(out) + 1]] <- rawToChar(decompressed)
    }
  }
  out
}

# Render a plot on a temporary portable_pdf device and return the path
render_pdf <- function(code, ...) {
  file <- tempfile(fileext = ".pdf")
  portable_pdf(file, ...)
  force(code)
  dev.off()
  file
}
