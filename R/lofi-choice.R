

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a choice from a list of options to a lofi bit representation
#'
#' @param choice value to convert to lofi representation
#' @param lofi low-bit representation
#' @param options the reference list of options it should be coded from
#'
#' @return low-bit representation i.e. the index of the choice in the list of options
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
choice_to_lofi <- function(choice, options) {

  stopifnot(!is.factor(options))

  lofi <- match(choice, options) - 1L

  if (any(is.na(lofi))) {
    stop("choice_to_lofi(): can't pack ", deparse(choice[is.na(lofi)]),
         " if choices are ", deparse(options), call. = FALSE)
  }

  lofi
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname choice_to_lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_to_choice <- function(lofi, options) {

  stopifnot(!is.factor(options))

  if (any(lofi + 1L > length(options))) {
    stop("lofi_to_choice(): Index ", deparse(lofi[lofi + 1 > length(options)]),
         " is out of range of options: ",
         deparse(options), call. = FALSE)
  }

  options[lofi + 1L]
}
