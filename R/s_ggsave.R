#' @export
s_ggsave <- function(
    plot = ggplot2::last_plot(), filename, path = c("script", "wd"), width = 7, height = 5,
    units = "in", dpi = 300, bg = "white") {
  if (length(path) > 1 && is.character(path)) {
    path <- path[1]
  }
  if (path == "script") {
    path <- dirname(rstudioapi::getSourceEditorContext()$path)
  } else if (path == "wd") {
    path <- getwd()
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  } else {
    path <- dirname(filename)
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )
}