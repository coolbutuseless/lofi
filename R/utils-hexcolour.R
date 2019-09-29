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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert between equivalent 32-bit representations
#'
#' @param rgba_vec  4-element RGBA colour with values in range [0, 255]
#' @param hex_colour string "#0a44f5', "#33ff12ff"
#' @param int32 32 bit integer
#'
#' @return Hex colour with Alpha
#'
#'
#' @importFrom grDevices col2rgb rgb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgba_vec_to_hex_colour <- function(rgba_vec) {
  args <- as.list(rgba_vec)
  args$maxColorValue <- 255

  do.call(grDevices::rgb, args)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rgba_vec_to_hex_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_colour_to_rgba_vec <- function(hex_colour) {
  stopifnot(length(hex_colour) == 1L)
  as.vector(grDevices::col2rgb(hex_colour, alpha = TRUE))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rgba_vec_to_hex_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_hex_colour <- function(int32) {
  rgba <- int32_to_rgba_vec(int32)
  grDevices::rgb(rgba[1], rgba[2], rgba[3], rgba[4], maxColorValue = 255L)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rgba_vec_to_hex_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_colour_to_int32 <- function(hex_colour) {
  mat <- grDevices::col2rgb(hex_colour, alpha = TRUE)

  apply(mat, 2, rgba_vec_to_int32)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rgba_vec_to_hex_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rgba_vec_to_int32 <- function(rgba_vec) {
  if (length(rgba_vec) != 4L) {
    stop("rgba_vec_to_int32(): rgba_vec must have 4 elements, not: ", deparse(rgba_vec))
  }

  readBin(rev(as.raw(rgba_vec)), what = 'integer', n = 1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rgba_vec_to_hex_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_rgba_vec <- function(int32) {
  stopifnot(is.integer(int32))
  as.integer(writeBin(int32, raw(), size = 4L, useBytes = TRUE, endian = 'big'))
}

# nocov end



