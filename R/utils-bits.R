# nocov start

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname show_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits.double <- function(x, ...) {
  dbl <- x

  bits         <- dbl_to_bits(dbl)
  sign_bit     <- as.integer(bits[1])
  exponent     <- bits[2:12]
  exponent_str <- paste(as.integer(exponent), collapse = "")
  mantissa     <- bits[13:64]
  mantissa_str <- paste(as.integer(mantissa), collapse = "")

  cat(sign_bit, " ", exponent_str,
      " (", exponent_bits_to_int32(exponent), ") ",
      mantissa_str, " : ", dbl, "\n", sep = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname show_bits
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits.integer <- function(x, nbits = 32, ...) {
  bits <- int32_to_bits(x)
  if (nbits != 32) {
    bits <- rev(rev(bits)[seq_len(nbits)])
  }
  cat(paste(as.integer(bits), collapse = ""), "\n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Show a representation of the bits in a numeric value
#'
#' @param x double or integer value
#' @param nbits number of bits to dispaly if showing an integer
#' @param ... other arguments passed to specific implementations
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_bits <- function(x, ...) {
  UseMethod('show_bits')
}

# nocov end
