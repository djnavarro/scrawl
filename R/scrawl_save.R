#' Save a scrawl plot to file
#'
#' @param plot Object to save
#' @param filename Path to file
#' @param pixels Resolution of image in pixels
#'
#' @return Invisibly returns NULL
#'
#' @details This is just a thin wrapper around ggsave()
#' @export
scrawl_save <- function(plot, filename, pixels = 5000) {

  # save it it to file
  ggplot2::ggsave(
    filename = filename,        # the filename
    plot = plot,             # the ggplot object to draw to the file
    width = pixels / 300,  # width of the image in "inches"
    height = pixels / 300, # height of the image in "inches"
    dpi = 300                   # dpi = "dots (pixels) per inch"
  )

  # this is a function called only for its side effects
  return(invisible(NULL))
}
