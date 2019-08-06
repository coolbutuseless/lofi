# nocov start

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Display a vector of hex colours.
#'
#' Display a vector of hex colours (given as character strings).
#'
#' @param colours vector of hex colours e.g. \code{c('#ffffff', '#123456')}
#' @param ncol Number of columns
#' @param width,height dimensions of each colour swatch
#' @param xfraction,yfraction fill fraction of the swatch
#'
#' @importFrom graphics par plot rect
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_palette <- function(colours, ncol = 2, width = 40, height = 10,
                         xfraction = 0.99, yfraction = 0.95) {

  n       <- length(colours)
  nrow    <- ceiling(n/ncol)

  old <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old))

  plot(c(0, ncol*width), c(0, -nrow*height), type = "n", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', axes = FALSE, asp = 1)

  ii <- 1
  for (row in seq(nrow)-1) {
    for (col in seq(ncol)-1) {
      rect(col*width, -row*height, (col+xfraction)*width, -(row+yfraction)*height, col = colours[ii], border = NA)
      ii <- ii + 1
    }
  }

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Display a vector of lofi colours.
#'
#' @param lofi integer vector of lofi colours
#' @param rgb_bits 3-element vector with number of bits to represent each of the
#'        3 colour channels. Each element must be in the range [1,8]
#' @param title title
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_lofi_palette <- function(lofi, rgb_bits, title = NULL) {
  hex_colours  <- lofi_to_hex_colour(lofi, rgb_bits = rgb_bits)
  plot_palette(hex_colours, title)
}

# nocov end
