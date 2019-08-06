

valid_types <- c('integer', 'logical', 'double', 'choice', 'custom', 'scaled')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL operator from tidyverse
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an integer to bit-mask the lower 'nbits' of an integer
#'
#' @param nbits number of lower bits to mask
#'
#' @return integer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mask <- function(nbits) {
  2L^nbits - 1L
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check a value fits in the given number of bits
#'
#' @param x value. integer, or something which can be corerced to integer
#' @param nbits number of bits in which to fit 'x'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
valid_encoded_value <- function(x, nbits) {
  !is.null(x)             &&
    length(x) == 1        &&
    !is.na(as.numeric(x)) &&
    x >= 0                &&
    x <  2^nbits
}

