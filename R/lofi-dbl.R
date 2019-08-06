


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# list of floating point formats
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
float_formats <- list(
  # double   = list(float_bits = c(1, 11, 52)),
  single   = list(float_bits = c(1,  8, 23)),  # https://en.wikipedia.org/wiki/Single-precision_floating-point_format
  half     = list(float_bits = c(1,  5, 10)),  # https://en.wikipedia.org/wiki/Half-precision_floating-point_format
  bfloat16 = list(float_bits = c(1,  8,  7)),  # https://en.wikipedia.org/wiki/Bfloat16_floating-point_format
  fp24     = list(float_bits = c(1,  7, 16))   # https://en.wikipedia.org/wiki/Minifloat
)

float_formats[['fp16']] <- float_formats[['half']]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a single double precision floating point value to bits
#'
#' @param dbl double value
#'
#' @return vector of bits, MSB first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbl_to_bits <- function(dbl) {
  stopifnot(length(dbl) == 1L)
  stopifnot(is.double(dbl))

  rev(rawToBits(writeBin(dbl, raw(), size = 8)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Bits to double
#'
#' @param bits vector of bits with 1 bit for sign, 11 bits for exponent and 52 bits for mantissa
#'
#' @return vector of bits, MSB first
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits_to_dbl <- function(bits) {
  stopifnot(length(bits) == 64)
  stopifnot(!any(is.na(bits)))
  stopifnot(all(bits >= 0))

  bits <- as.raw(bits)
  readBin(packBits(rev(bits), type = 'raw'), what = 'double')
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an exponent in offset binary format to a 32 bit signed integer
#'
#' Reference: \url{https://en.wikipedia.org/wiki/Half-precision_floating-point_format}
#'
#' @param bits vector of bits
#'
#' @return Return integer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exponent_bits_to_int32 <- function(bits) {
  lofi  <- bits_to_lofi(bits)
  int32 <- lofi_to_int32(lofi, nbits = length(bits), signed = FALSE)
  bias  <- 2^(length(bits) - 1) - 1
  as.integer(lofi - bias)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' What can the given float hold?
#'
#' @param float_bits 3 element vector allocating bits to  sign, exponent and mantiassa
#'
#' @return named list of details about the values this float could represent
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
float_info <- function(float_bits) {
  if (is.null(float_bits) || length(float_bits) != 3 ||
      !is.numeric(float_bits) || any(is.na(float_bits)) ||
      sum(float_bits) > 32 || sum(float_bits) < 1 ||
      any(float_bits < 0)) {
    stop("float_info(): Invalid 'float_bits'. Expecting 3-element numeric vector with total in range [1,32]. ",
         "Current value: ", deparse(float_bits), call. = FALSE)
  }


  nbits_s <- float_bits[1]
  nbits_e <- float_bits[2]
  nbits_m <- float_bits[3]

  max_exponent <- 2^(nbits_e - 1)

  largest_positive  <- 2 ^ (max_exponent - 1) * (2 - 2^(-nbits_m))
  smallest_positive <- 2 ^ -(max_exponent - 2)

  res <- list(
    exponent_range = exponent_range(nbits_e),
    largest_positive  = largest_positive,
    smallest_positive = smallest_positive
  )


  if (nbits_s > 0) {
    res$largest_negative  = -largest_positive
  }

  res
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Approximate algorithm for allocating bits to a float given the maximum value
#'
#' @param nbits number of bits for the low-fidelity representation
#' @param max_value the maximum value to be encoded
#' @param signed whether or not signed values will be encoded: default: FALSE
#'
#' @return named list of details about the values this float could represent
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
allocate_float_bits <- function(nbits, max_value, signed = FALSE) {

  sign_bits     <- 1 * signed
  exponent_bits <- ceiling(log2(log2(max_value))) + 1
  mantissa_bits <- nbits - sign_bits - exponent_bits

  if (mantissa_bits <= 3) {
    stop("allocate_float_bits(): with ", nbits, " total bits and a 'max_value' of ",
         max_value, " there are fewer than 4 bits for the mantissa. Automatic ",
         "bit allocation for floats will not work here. Please specify a manual 'float_bits' instead",
         call. = FALSE)
  }

  c(sign_bits, exponent_bits, mantissa_bits)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the possible range of exponents given the number of bits available
#'
#' @param nbits_e Number of bits for the exponent
#'
#' @return Return 2-element vector giving the min and max possible exponent
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exponent_range <- function(nbits_e) {
  c(-(2^(nbits_e - 1) - 1), 2^(nbits_e - 1))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 32 bit signed integer to a floating point exponent in offset binary format
#'
#' Reference: \url{https://en.wikipedia.org/wiki/Half-precision_floating-point_format}
#'
#' @param int32 signed 32 bit integer
#' @param nbits number of bits for exponent
#'
#' @return Return integer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int32_to_exponent_bits <- function(int32, nbits) {
  bias  <- 2^(nbits - 1) - 1
  int32 <- as.integer(int32 + bias)
  lofi  <- int32_to_lofi(int32, nbits, signed = FALSE)
  lofi_to_bits(lofi, nbits = nbits)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Unpack a floating point value into a list
#'
#' Assumes LSB is in the 32 or 64 bit of the representation
#'
#' @param lofi floating point number. stored as bits in a a single R integer
#' @param dbl 64 bit R floating point real
#' @param fp named list
#' @param float_bits length (in number of bits) of sign, exponent and mantissa.
#'
#' @return Return a named list with elements sign, exponent and mantissa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_fp <- function(lofi, float_bits) {

  nbits <- sum(float_bits)
  stopifnot(nbits <= 32)

  bits   <- lofi_to_bits(lofi, nbits = nbits)

  res <- list()

  nbits_s <- float_bits[1]
  nbits_e <- float_bits[2]
  nbits_m <- float_bits[3]

  if (nbits_e == 0 || nbits_m == 0) {
    stop("lofi_to_fp(): Cannot have zero bits for exponent or mantissa. Current 'float_bits': ",
         deparse(float_bits), call. = FALSE)
  }

  if (!nbits_s %in% c(0, 1)) {
    stop("lofi_to_fp(): Sign bits should only be 0 or 1. Current 'float_bits': ",
         deparse(float_bits), call. = FALSE)
  }

  res$sign     <- bits[seq_len(nbits_s)]
  res$exponent <- bits[seq_len(nbits_e) + nbits_s]
  res$mantissa <- bits[seq_len(nbits_m) + nbits_s + nbits_e]

  if (nbits_s == 0) {
    res$sign <- raw(0)
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname lofi_to_fp
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbl_to_fp <- function(dbl) {
  bits   <- dbl_to_bits(dbl)

  list(
    sign     = bits[    1],
    exponent = bits[ 2:12],
    mantissa = bits[13:64]
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname lofi_to_fp
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp_to_dbl <- function(fp) {
  nbits <- with(fp, sum(length(sign), length(exponent), length(mantissa)))
  stopifnot(nbits == 64L)

  bits <- with(fp, c(sign, exponent, mantissa))
  bits_to_dbl(bits)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname lofi_to_fp
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp_to_lofi <- function(fp) {
  nbits <- with(fp, sum(length(sign), length(exponent), length(mantissa)))
  # stopifnot(nbits == 32L)

  bits <- with(fp, c(sign, exponent, mantissa))
  bits_to_lofi(bits)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine max floating point value with given bit allocation
#'
#' @param float_bits 3-element numeric vector with bitwidths of sign, exponent and
#'        mantissa
#'
#' @return Return the value of the maximum possible value representable by this
#'         floating point representation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_dbl_with_format <- function(float_bits = c(1, 11, 52)) {
  nbits_s <- float_bits[1]
  nbits_e <- float_bits[2]
  nbits_m <- float_bits[3]

  2 ^ (2 ^ (nbits_e - 1) - 1) * (1 + (2^nbits_m - 1)/2^nbits_m)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname lofi_to_fp
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
change_format_fp <- function(fp, float_bits = c(1, 8, 7)) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # nbits in the target float_bits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nbits_ts <- float_bits[1]
  nbits_te <- float_bits[2]
  nbits_tm <- float_bits[3]



  if (nbits_te == 0 || nbits_tm == 0) {
    stop("change_format_fp(): Cannot have zero bits for exponent or mantissa. Current 'float_bits': ",
         deparse(float_bits), call. = FALSE)
  }


  if (!nbits_ts %in% c(0, 1)) {
    stop("change_format_fp(): Sign bits should only be 0 or 1. Current 'float_bits': ",
         deparse(float_bits), call. = FALSE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # bits in the current fp
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nbits_cs <- length(fp$sign)
  nbits_ce <- length(fp$exponent)
  nbits_cm <- length(fp$mantissa)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Can't change from signed to unsigned if the current value is negative
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nbits_cs != 0 && fp$sign != 0 && nbits_ts == 0) {
    stop(
      "change_format_fp(): ",
      "Can't encode a negative number into an unsigned float with 'float_bits': ",
      deparse(float_bits),
      call. = FALSE
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If currently unsigned, but moving to signed, add a sign bit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nbits_cs == 0 & nbits_ts != 0) {
    fp$sign <- as.raw(0)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if currently signed but moving to unsigned, remove the sign bit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nbits_cs != 0 & nbits_ts == 0) {
    fp$sign <- raw(0)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check that the current exponent will fit in the new exponent
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  erange <- exponent_range(nbits_te)
  current_exponent <- exponent_bits_to_int32(fp$exponent)
  if (current_exponent < erange[1] || current_exponent > erange[2]) {
    stop(
      "change_format_fp(): ",
      "Cannot represent exponent ", current_exponent,
      " in ", deparse(float_bits), " where exponent must be in range ",
      deparse(erange),
      call. = FALSE
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Repack exponent into new bit allocation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fp$exponent <- int32_to_exponent_bits(current_exponent, nbits_te)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the target mantissa is larger than the current mantissa,
  # then add zero-padding on the end
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nbits_tm > nbits_cm) {
    mantissa_padding <- rep(as.raw(0), nbits_tm - nbits_cm)
    fp$mantissa <- c(fp$mantissa, mantissa_padding)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the target mantissa is smaller than the current mantissa,
  # then truncate the current mantissa
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nbits_tm < nbits_cm) {
    fp$mantissa <- fp$mantissa[seq_len(nbits_tm)]
  }


  fp
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Pack a double floating point value into fewer than 32 bits.
#'
#'
#' Pack a double into the lower bits of an int32 with the given bits for sign,
#' exponent and mantissa.
#'
#' By packing into a low fidelity bit representation you will definitely
#' lose precision i.e. converting back into full 64 bit precision will not
#' give you back the number you started with.
#'
#' Packing a double into low-fidelity format has no explicit support for special values
#' such as \code{NaN}, \code{NA} or \code{Inf}.  These values may get converted
#' to other numeric values or other special values.  The result is undefined.
#' Operate on special values at your own risk.
#'
#'
#'
#' @param dbl 64 bit R double
#' @param lofi low-bit representation
#' @param float_name 'single', 'half', 'bfloat16'. Default: 'bfloat16'
#' @param float_bits length (in number of bits) of sign, exponent and mantissa.
#'        Default: NULL.  If this value is not null, then it will override
#'        anything the user may have specified for \code{float_name}
#'
#' @return 32 bit integer with lower bits set to represent the quantized
#'         floating point value
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbl_to_lofi <- function(dbl, float_name = 'bfloat16', float_bits = NULL) {

  # If given multiple values, return multiple lofi representation
  if (length(dbl) > 1) {
    return(vapply(dbl, dbl_to_lofi, integer(1), float_name, float_bits))
  }

  if (!is.null(float_bits)) {
    stopifnot(length(float_bits) == 3)
  } else if (float_name %in% names(float_formats)) {
    float_bits <- float_formats[[float_name]]$float_bits
  } else {
    stop("dbl_to_lofi(): Invalid 'float_name': ", deparse(float_name))
  }

  fp <- dbl_to_fp(dbl)
  fp <- change_format_fp(fp , float_bits = float_bits)
  fp_to_lofi(fp)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname dbl_to_lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_dbl <- function(lofi, float_name = 'bfloat16', float_bits = NULL) {

  # If given multiple values, return multiple lofi representation
  if (length(lofi) > 1) {
    return(vapply(lofi, lofi_to_dbl, double(1), float_name, float_bits))
  }

  if (!is.null(float_bits)) {
    stopifnot(length(float_bits) == 3)
  } else if (float_name %in% names(float_formats)) {
    float_bits <- float_formats[[float_name]]$float_bits
  } else {
    stop("lofi_to_dbl(): Invalid 'float_name': ", deparse(float_name))
  }

  fp <- lofi_to_fp(lofi, float_bits)
  fp <- change_format_fp(fp, c(1, 11, 52))
  fp_to_dbl(fp)
}

