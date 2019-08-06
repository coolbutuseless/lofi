

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Expect the given value to be a single, non-NA numeric value
#'
#' @param value value
#' @param value_desc description of value to be inserted into error message
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expect_single_numeric <- function(value, value_desc = "value") {
  if (length(value) != 1   ||
      !is.numeric(value)   ||
      is.na(value)) {
    stop("pack_spec: Invalid ", value_desc, ". Expecting single, non-NA numeric value. ",
         "Current value: ", deparse(value), call. = FALSE)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Validate 'nbits' to be a proper value
#'
#' @param nbits number of bits
#' @param value_desc description of value to be inserted into error message
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expect_valid_nbits <- function(nbits, value_desc = 'nbits') {
  expect_single_numeric(nbits, value_desc)

  if (nbits < 1 || nbits > 32) {
    stop("pack_spec: Invalid ", value_desc, ".  Expecting nbits in range [1, 32]. ",
         "Current value: ", deparse(nbits), call. = FALSE)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for a 'integer'
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_integer <- function(spec) {
  stopifnot(identical(spec$type, 'integer'))

  spec$offset <- spec$offset %||% 0L
  spec$mult   <- spec$mult   %||% 1L
  spec$signed <- spec$signed %||% FALSE

  expect_single_numeric(spec$offset, "integer 'offset'")
  expect_single_numeric(spec$mult  , "integer 'mult'"  )
  expect_valid_nbits   (spec$nbits , "integer 'nbits'" )

  spec$nbits  <- round(spec$nbits)

  spec
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for a 'scaled' value
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_scaled <- function(spec) {
  stopifnot(identical(spec$type, 'scaled'))

  spec$min      <- spec$min      %||% 0L
  spec$cyclical <- spec$cyclical %||% FALSE

  if (is.null(spec$max)) {
    stop("sanitise_spec_scaled(): Invalid 'spec$max'.  Expecting number. ",
         "Current value: ", deparse(spec$max), call. = FALSE)
  }

  expect_single_numeric(spec$min   , "scaled 'min'"  )
  expect_single_numeric(spec$max   , "scaled 'max'"  )
  expect_valid_nbits   (spec$nbits , "scaled 'nbits'")

  spec$nbits  <- round(spec$nbits)

  spec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for a 'custom' pack
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_custom <- function(spec) {
  stopifnot(identical(spec$type, 'custom'))

  spec$signed <- spec$signed %||% FALSE

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to functions if supplied as formulas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(spec$pack_func, 'formula')) {
    spec$pack_func <- formula_to_1arg_function(spec$pack_func)
  }

  if (inherits(spec$unpack_func, 'formula')) {
    spec$unpack_func <- formula_to_1arg_function(spec$unpack_func)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the pack_func and unpack_func are functions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(spec$pack_func) || !is.function(spec$pack_func)) {
    stop("sanitise_spec_custom(): Invalid spec$pack_func. Expecting formula or function. ",
         "Current value: ", deparse(spec$pack_func), call. = FALSE)
  }

  if (is.null(spec$unpack_func) || !is.function(spec$unpack_func)) {
    stop("sanitise_spec_custom(): Invalid spec$unpack_func Expecting formula or function. ",
         "Current value: ", deparse(spec$unpack_func), call. = FALSE)
  }

  expect_valid_nbits(spec$nbits , "integer 'nbits'" )

  spec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for a 'logical'
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_logical <- function(spec) {
  stopifnot(identical(spec$type, 'logical'))

  spec$nbits  <- spec$nbits %||% 1

  expect_valid_nbits(spec$nbits, "logical 'nbits'" )
  spec$nbits  <- round(spec$nbits)

  spec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for a 'double'
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_double <- function(spec) {
  stopifnot(identical(spec$type, 'double'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If user has specified an 'nbits' do an auto assigment based upon their
  # proposed maxval.
  #
  # If user has specified a 'float_name', then use that to define `float_bits`
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(spec$nbits) && is.null(spec$float_bits)) {
    if (is.null(spec$maxval)) {
      stop("sanitise_spec_double(): must specify a 'maxval' if specifying 'nbits'", call. = FALSE)
    }
    spec$signed     <- spec$signed %||% FALSE
    spec$float_bits <- allocate_float_bits(spec$nbits, max_value = spec$maxval, signed = spec$signed)
    # message("Auto allocate double bits: ", spec$nbits, ": ", deparse(spec$float_bits))
  } else if (!is.null(spec$float_name) && spec$float_name %in% names(float_formats)) {
    spec$float_bits <- float_formats[[spec$float_name]]$float_bits
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check that we have float_bits assigned to smething now.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(spec$float_bits) == 0 || !is.numeric(spec$float_bits)) {
    stop("sanitise_spec_double(): Invalid 'float_bits' for double. ",
         "Expecting non-NA numeric vector of length 3. Current value: ",
         deparse(spec$float_bits), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure 'float_bits' is integer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spec$float_bits <- as.integer(spec$float_bits)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make sure float_bits is sane
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!length(spec$float_bits) == 3 || any(is.na(spec$float_bits)) ||
      any(spec$float_bits < 0) ||
      sum(spec$float_bits) > 32) {
    stop("sanitise_spec_double(): Invalid 'float_bits' for double. ",
         "Expecting non-NA numeric vector of length 3 with sum(float_bits) in range [0, 32]. ",
         "Current value: ", deparse(spec$float_bits), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sum the bits for the float spec
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spec$nbits  <- sum(spec$float_bits)
  expect_valid_nbits(spec$nbits, "double 'nbits'" )


  spec
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for an 'choice'
#'
#' @param spec named list object with 'type', 'nbits', etc
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_choice <- function(spec) {
  stopifnot(identical(spec$type, 'choice'))

  if (length(spec$options) == 0L) {
    stop("pack_spec: Invalid choice 'options'. Expecting at least one element. ",
         "Current value: ", call. = FALSE)
  }


  spec$nbits <- spec$nbits %||% ceiling(log2(length(spec$options)))
  if (spec$nbits == 0) {
    spec$nbits <- 1L
  }
  spec$nbits  <- round(spec$nbits)


  if (spec$nbits < ceiling(log2(length(spec$options)))) {
    stop("Invalid pack_spec: Not enough bits to hold all options. Bits: ",
         spec$nbits, "  Values: ", deparse(spec$options), call. = FALSE)
  }

  expect_valid_nbits(spec$nbits , "choice 'nbits'" )

  spec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usually the ser can just specify number of bits, and we'll pick a
# pretty good bit allocation for RGB.
# Can't do bits < 3, and can't do bits > 24
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
default_rgb_bits <- list(
  NA,
  NA,
  c(1, 1, 1),
  c(1, 2, 1),
  c(2, 2, 1),
  c(2, 2, 2),
  c(2, 3, 2),
  c(3, 3, 2),
  c(3, 3, 3),
  c(3, 4, 3),
  c(4, 4, 3),
  c(4, 4, 4),
  c(4, 5, 4),
  c(5, 5, 4),
  c(5, 5, 5),
  c(5, 6, 5),
  c(6, 6, 5),
  c(6, 6, 6),
  c(6, 7, 6),
  c(7, 7, 6),
  c(7, 7, 7),
  c(7, 8, 7),
  c(8, 8, 7),
  c(8, 8, 8),
  NA, NA, NA, NA, NA, NA, NA, NA
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a spec for an 'colour'
#'
#' @param spec named list object with 'type = "colour"', 'nbits' or 'rgb_bits'
#'
#' @return Return sanitised spec with any generic default values set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec_colour <- function(spec) {
  stopifnot(identical(spec$type, 'colour'))

  if (!is.null(spec$rgb_bits)) {
    # use rgb bits as given
  } else if (is.null(spec$nbits) || length(spec$nbits) != 1L ||
             is.na(spec$nbits) || !is.numeric(spec$nbits)) {
    stop("sanitise_spec_colour(): Invalid 'nbits'. ",
         "Expecing 'nbits' to be single non-NA numeric value. ",
         "Current value: ", deparse(spec$nbits), call. = FALSE)
  } else if (spec$nbits < 3 || spec$nbits > 24) {
    stop("sanitise_spec_colour(): Invalid 'nbits'. ",
         "Expecting 'nbits' in range [3, 24]. ",
         "Current value: ", deparse(spec$nbits), call. = FALSE)
  } else {
    spec$rgb_bits <- default_rgb_bits[[spec$nbits]]
  }

  spec$nbits <- sum(spec$rgb_bits)
  expect_valid_nbits(spec$nbits , "colour 'nbits'")

  spec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise an individual spec
#'
#' @param spec single spec
#'
#' @return Return sanitised spec
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_spec <- function(spec) {

  if (is.null(spec$type)) {
    warning("sanitise_spec(): spec contains no 'type'")
    print(spec)
    stop("sanitise_spec(): spec contains no 'type'")
  }

  switch(
    spec$type,
    integer = sanitise_spec_integer(spec),
    logical = sanitise_spec_logical(spec),
    double  = sanitise_spec_double (spec),
    choice  = sanitise_spec_choice (spec),
    colour  = sanitise_spec_colour (spec),
    custom  = sanitise_spec_custom (spec),
    scaled  = sanitise_spec_scaled (spec),
    stop("sanitise_spec(): Invalid spec$type: ", deparse(spec$type))
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise a full pack_spec
#'
#' @param pack_spec pack_spec
#'
#' @return Return sanitised pack_spec
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_pack_spec <- function(pack_spec) {
  pack_spec <- lapply(pack_spec, sanitise_spec)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check bits
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  all_bits   <- vapply(pack_spec, function(x) {x$nbits}, FUN.VALUE = numeric(1))
  total_bits <- sum(all_bits)

  if (is.na(total_bits) || total_bits < 1 || total_bits > 32 || total_bits != round(total_bits)) {
    stop("validate_pack_spec(): Invalid total 'bits' in pack_spec. ",
         "Expecting total bits in range [1, 32]. ",
         "Current value: ", total_bits, call. = FALSE)
  }

  pack_spec
}

