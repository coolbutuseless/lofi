
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Conversion of formula to a 1-arg function
#'
#' This aims to be a dependency free, very simple formula-to-function convertor.
#'
#' The only supported argument is \code{.x}, all other variables are assumed to
#' come from the environment.
#'
#' @param form formula
#' @param .env environment of function
#'
#' @return function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
formula_to_1arg_function <- function (form, .env = parent.frame())  {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is the entirity of the sanity checking
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!inherits(form, 'formula') || length(form) != 2L) {
    stop("formula_to_1arg_function(): Argument 'form' must be a formula. ",
         "Current class: ", deparse(class(form)), call. = FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The list of formal arguments is always just '.x'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f              <- function() {}
  formals(f)     <- alist(.x = )
  body(f)        <- form[[-1]]
  environment(f) <- .env

  f
}

