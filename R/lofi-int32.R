#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single R 32-bit signed integer value to bits
#'
#' @param int32 integer value
#'
#' @return vector of 32 bits, sign bit and then 31 bits with most-significant bit first.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_bits <- function(int32) {
  stopifnot(length(int32) == 1L)
  stopifnot(is.integer(int32))
  rev(rawToBits(writeBin(int32, raw(), size = 4)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert 32 bits into a a single R 32 bit signed integer value
#'
#' @param bits vector of 32 bits, sign bit and then 31 bits with most-significant bit first.
#' @return an integer value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits_to_int32 <- function(bits) {
  stopifnot(length(bits) == 32L)

  readBin(packBits(rev(bits), type = 'raw'), what = 'integer')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single R 32-bit signed integer value to a lower bit representation
#'
#' @param int32 a single R 32-bit signed integer value
#' @param lofi integer value
#' @param nbits number of bits
#' @param signed boolean. default: FALSE
#'
#' @return an integer value, only the lower 'nbits' contain valid information
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_lofi <- function(int32, nbits, signed = FALSE) {

  stopifnot(is.integer(int32))

  if (signed) {
    min_value <- -(2^(nbits - 1))
    max_value <-  (2^(nbits - 1) - 1)
    bad       <- int32 > max_value | int32 < min_value
    if (any(bad)) {
      stop("int32_to_lofi(): ", deparse(int32[bad]),
           " cannot be represented as signed ", nbits, "-bit integer", call. = FALSE)
    }
  } else {
    min_value <- 0
    max_value <- 2^nbits - 1
    bad       <- int32 > max_value | int32 < min_value
    if (any(bad)) {
      stop("int32_to_lofi(): ", deparse(int32[bad]),
           " cannot be represented as unsigned ", nbits, "-bit integer", call. = FALSE)
    }
  }

  lofi <- bitwAnd(int32, mask(nbits))

  lofi <- ifelse(int32 < 0, bitwOr(lofi, 2^(nbits - 1)), lofi)

  lofi
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname int32_to_lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_int32 <- function(lofi, nbits, signed = FALSE) {
  stopifnot(is.integer(lofi))

  int32 <- bitwAnd(lofi, mask(nbits))

  if (signed) {
    sign_bit <- bitwShiftR(int32, nbits - 1L)

    # if (sign_bit == 1) {
    #   neg_mask <- bitwNot(mask(nbits))
    #   int32    <- bitwXor(int32, neg_mask)
    # }
    int32 <- ifelse(sign_bit == 1, bitwXor(int32, bitwNot(mask(nbits))), int32)

  }

  int32
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a low-bit integer into bit representation
#'
#' Limitations: max 32 bit signed integer
#'
#' @param lofi integer
#' @param nbits number of bits
#'
#' @return vector of bits (length = nbits)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_bits <- function(lofi, nbits) {
  stopifnot(nbits <= 32)
  stopifnot(is.integer(lofi))
  stopifnot(length(lofi) == 1L)

  bits <- int32_to_bits(lofi)

  if (nbits == 32) {
    bits
  } else {
    bits[seq(32 - nbits + 1, 32)]
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a sequence of 0/1 values (MSB first) to an integer
#'
#' @param bits vector of 0/1 values
#'
#' @return lofi
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits_to_lofi <- function(bits) {
  stopifnot(length(bits) <= 32L)

  bits <- c(rep(as.raw(0), 32 - length(bits)), as.raw(bits))

  bits_to_int32(bits)
}

