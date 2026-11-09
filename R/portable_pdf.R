#' Create a PDF device with embedded fonts
#'
#' `portable_pdf()` opens a PDF graphics device that uses systemfonts and
#' textshaping for font lookup and text layout (the same font stack as the
#' ragg devices) and embeds all used fonts so the document renders the same on
#' any machine.
#'
#' @param file Path of the file to write to.
#' @param width,height Size of the device in inches.
#' @param scaling A scaling factor applied to the device resolution, in the
#'   same way as the `scaling` argument in ragg and the `scale` argument in
#'   svglite. Values above 1 increase the size of text and other elements
#'   relative to the physical page size.
#' @param pointsize Default point size of text.
#' @param family Default font family to use for text.
#' @param bg Background colour of the device. Use `"transparent"` for no fill.
#' @param title,author Title and author written to the PDF document metadata.
#' @param colormodel Colour model for vector graphics output: `"rgb"` (the
#'   default), `"gray"`, or `"cmyk"`. Embedded raster images are always stored
#'   as RGB.
#'
#' @return Invisibly, the path to the created file.
#'
#' @examples
#' portable_pdf(tempfile(fileext = ".pdf"))
#' plot(sin, -pi, 2 * pi)
#' dev.off()
#'
#' @export
portable_pdf <- function(file = "portable.pdf", width = 7, height = 7,
                         scaling = 1, pointsize = 12, family = "sans",
                         bg = "white", title = "", author = "",
                         colormodel = c("rgb", "gray", "cmyk")) {
  colormodel <- match.arg(colormodel)
  if (identical(bg, "transparent")) {
    bg_rgba <- c(255L, 255L, 255L, 0L)
  } else {
    bg_col <- grDevices::col2rgb(bg, alpha = TRUE)
    bg_rgba <- as.integer(bg_col[, 1])
  }
  invisible(.Call("C_portable_pdf", path.expand(file), as.double(width),
                  as.double(height), as.double(scaling), as.double(pointsize),
                  as.character(family)[1], bg_rgba, as.character(title)[1],
                  as.character(author)[1], colormodel, PACKAGE = "portable"))
}
