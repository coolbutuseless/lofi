% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lofi-dbl.R
\name{max_dbl_with_format}
\alias{max_dbl_with_format}
\title{Determine max floating point value with given bit allocation}
\usage{
max_dbl_with_format(float_bits = c(1, 11, 52))
}
\arguments{
\item{float_bits}{3-element numeric vector with bitwidths of sign, exponent and
mantissa}
}
\value{
Return the value of the maximum possible value representable by this
        floating point representation
}
\description{
Determine max floating point value with given bit allocation
}
