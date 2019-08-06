

cut_levels <- lapply(1:8, function(x) {
  as.integer(round(seq(0, 256, length.out = 2^x + 1)))
})


expand_levels <- lapply(1:8, function(x) {
  as.integer(round(seq(0, 255, length.out = 2^x)))
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Quantize hex colours into a low-bit representation
#'
#' Quantize a character vector of hex colours into a lofi (reduced bit) representation,
#' and output this as a vector of integers.
#'
#'
#'
#' A hex colour (e.g. \code{'#ffaa70'}) is a character representation of a 24-bit colour i.e. 3 colour channels, each
#' represented by an 8 bit number.
#'
#' \emph{lofi} colour is a representation of a colour using a low number of bits for
#' each colour channel, and then packed as an integer.
#'
#'
#' See wikipedia articles:
#' \itemize{
#' \item{\href{https://en.wikipedia.org/wiki/8-bit_color}{8-bit colour using 3-3-2 bits for RGB}}
#' \item{\href{https://en.wikipedia.org/wiki/List_of_monochrome_and_RGB_palettes}{Monochrome and RGB Palettes}}
#' \item{\href{https://en.wikipedia.org/wiki/List_of_color_palettes}{Colour Palettes}}
#' }
#'
#' @param cols character vector of hex colours e.g. \code{c('#ffaa70', '#00a0e2')}
#' @param lofi vector of integers. Each integer contains a low-bit representation
#'        of a colour.
#' @param rgb_bits 3-element vector with number of bits to represent each of the
#'        3 colour channels. Each element must be in the range [1,8]
#'
#' @return Return integer vector. Each integer represents a colour.
#'
#' @importFrom grDevices col2rgb rgb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_colour_to_lofi <- function(cols, rgb_bits) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check bit layout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgb_bits <- as.integer(rgb_bits)
  if (length(rgb_bits) != 3L || any(rgb_bits > 8L) || any(rgb_bits < 1L)) {
    stop("hex_colour_to_lofi(): Invalid 'rgb_bits'. Expecting a 3 element vector with elements in the range [1, 8]. ",
         "Current value: ", deparse(rgb_bits), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert each hex colour into its rgb components in matrix form
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat <- t(col2rgb(cols))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Quantize the colours and move the low-bit representation for each
  # colour channel into the appropriate position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[,1L] <- (cut(mat[,1L], cut_levels[[rgb_bits[1L]]], labels = FALSE, right = FALSE, include.lowest = TRUE) - 1L) * 2L^(rgb_bits[3L] + rgb_bits[2L])
  mat[,2L] <- (cut(mat[,2L], cut_levels[[rgb_bits[2L]]], labels = FALSE, right = FALSE, include.lowest = TRUE) - 1L) * 2L^(rgb_bits[3L])
  mat[,3L] <- (cut(mat[,3L], cut_levels[[rgb_bits[3L]]], labels = FALSE, right = FALSE, include.lowest = TRUE) - 1L) * 2L^0L

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Each low-bit colour is just the sum of the individual channels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  as.integer(rowSums(mat))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hex_colour_to_lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_hex_colour <- function(lofi, rgb_bits) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check bit layout
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgb_bits <- as.integer(rgb_bits)
  if (length(rgb_bits) != 3L || any(rgb_bits > 8L) || any(rgb_bits < 1L)) {
    stop("lofi_to_hex_colour(): Invlaid 'rgb_bits'. Expecting a 3 element vector with elements in the range [1, 8] ",
         "Current value: ", deparse(rgb_bits), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # unpack the rgb_bits from each integer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r <- bitwAnd(bitwShiftR(lofi, rgb_bits[3L] + rgb_bits[2L]), 2L^rgb_bits[1L] - 1L)
  g <- bitwAnd(bitwShiftR(lofi, rgb_bits[3L]               ), 2L^rgb_bits[2L] - 1L)
  b <- bitwAnd(bitwShiftR(lofi, 0L                         ), 2L^rgb_bits[3L] - 1L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Expand the packed representation into an 8-bit representation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r <- expand_levels[[rgb_bits[1L]]][r + 1L]
  g <- expand_levels[[rgb_bits[2L]]][g + 1L]
  b <- expand_levels[[rgb_bits[3L]]][b + 1L]


  rgb(r, g, b, maxColorValue = 255)
}

