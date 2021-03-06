% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lofi-dbl.R
\name{allocate_float_bits}
\alias{allocate_float_bits}
\title{Approximate algorithm for allocating bits to a float given the maximum value}
\usage{
allocate_float_bits(nbits, max_value, signed = FALSE)
}
\arguments{
\item{nbits}{number of bits for the low-fidelity representation}

\item{max_value}{the maximum value to be encoded}

\item{signed}{whether or not signed values will be encoded: default: FALSE}
}
\value{
named list of details about the values this float could represent
}
\description{
Approximate algorithm for allocating bits to a float given the maximum value
}
