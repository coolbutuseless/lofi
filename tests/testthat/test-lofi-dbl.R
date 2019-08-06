

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' - Generate a random spread of doubles
#' - Separate uniform sampling of mantissa and exponent
#' - This can definitely create NA, NaN and +/- Inf.
#' - this can generate all possible doubles
#'
#' - Better implementations welcomed.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rand_dbl <- function(n) {
  readBin(
    sample(as.raw(c(0:255)), size = 8 * n, replace = TRUE),
    n    = n,
    what = 'double'
  )
}


test_that("dbl_to_bits works", {
  res <- dbl_to_bits(12)
  expect_identical(
    res,
    as.raw(c(0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ))
  )
})


test_that("dbl_to_bits only accepts doubles", {
  expect_error(dbl_to_bits(12L))
  expect_error(dbl_to_bits("12.1"))
})


test_that("dbl_to_bits only accepts 1 value", {
  expect_error(dbl_to_bits(NULL))
  expect_error(dbl_to_bits(c(12.1, 2.3)))
})


test_that("bits_to_dbl works", {
  res <- bits_to_dbl(    as.raw(c(0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  )))
  expect_identical(res, 12)
})


test_that("Can convert back and forth from dbl to bits", {
  set.seed(1)
  rands <- rand_dbl(1000)
  for (input in rands) {
    bits  <- dbl_to_bits(input)
    output <- bits_to_dbl(bits)
    expect_identical(input, output)
  }
})


test_that("dbl_to_lofi and lofi_to_double work", {
  rands <- runif(100, -120, 120)
  for (rand in rands) {
    lofi <- dbl_to_lofi(rand, float_name = 'bfloat16')
    out  <- lofi_to_dbl(lofi, float_name = 'bfloat16')
    expect_equal(out, rand, tolerance = 0.01)
  }
})


test_that("dbl_to_lofi will complain if not enough bits", {

  too_big <- 100
  expect_error(
    dbl_to_lofi(too_big, float_bits = c(1, 2, 2)),
    "exponent"
  )

  expect_error(
    dbl_to_lofi(too_big, float_bits = c(0, 2, 2)),
    "exponent"
  )

  expect_error(
    dbl_to_lofi(too_big, float_bits = c(1, 2, 9)),
    "exponent"
  )

  # enough bits for exponent
  dbl_to_lofi(too_big, float_bits = c(1, 9, 2))
})


test_that("dbl_to_lofi will complain if trying to store negative number as unsigned", {
  expect_error(
    dbl_to_lofi(-10, float_bits = c(0, 10, 10)),
    "negative number"
  )
})


test_that("dbl_to_lofi checks for invalid bit allocation", {
  expect_error(dbl_to_lofi(8, float_bits = c(1, 0, 1)), "zero bits")
  expect_error(dbl_to_lofi(8, float_bits = c(0, 8, 0)), "zero bits")
  expect_error(dbl_to_lofi(8, float_bits = c(2, 8, 4)), "Sign bits")
  expect_error(dbl_to_lofi(8, float_bits = c(2, 8)), "length.float_bits")
})


test_that("lofi_to_fp checks for invalid bit allocation", {
  expect_error(lofi_to_fp(8L, float_bits = c(1, 0, 1)), "zero bits")
  expect_error(lofi_to_fp(8L, float_bits = c(0, 8, 0)), "zero bits")
  expect_error(lofi_to_fp(8L, float_bits = c(2, 8, 4)), "Sign bits")
})


test_that("max_dbl_with_format works", {
  expect_equal(max_dbl_with_format(float_bits = c(1, 11, 52)), .Machine$double.xmax)
  expect_equal(max_dbl_with_format(float_bits = c(1, 8, 7)), 3.38953139E38)
})



test_that("dbl_to_lofi handles multiple values", {
  expect_identical(
    dbl_to_lofi(c(12, 9000)),
    c(dbl_to_lofi(12), dbl_to_lofi(9000))
  )
})



test_that("lofi_to_dbl handles multiple values", {
  expect_identical(
    lofi_to_dbl(c(12L, 90L)),
    c(lofi_to_dbl(12L), lofi_to_dbl(90L))
  )
})



test_that("dbl_to_lofi error if unknown float_name", {
  expect_error(dbl_to_lofi(12, 'eight-bit'), "Invalid 'float.name'")
})


test_that("lofi_to_dbl errors within unknown float type", {
  expect_error(lofi_to_dbl(12L, "c64 6502"), "Invalid 'float.name'")
})



test_that("float_info works", {

  # Single precision. see wikipedia
  res <- float_info(float_bits = c(1, 8, 23))
  expect_equal(res$largest_positive ,  3.4028234664E38)
  expect_equal(res$largest_negative , -3.4028234664E38)
  expect_equal(res$smallest_positive, 1.1754943508E-38)

  # Half precision. see wikipedia
  res <- float_info(float_bits = c(1, 5, 10))
  expect_equal(res$largest_positive ,  65504)
  expect_equal(res$largest_negative , -65504)
  expect_equal(res$smallest_positive, 0.000061035)

  # bfloat16. see wikipedia
  res <- float_info(float_bits = c(1, 8, 7))
  expect_equal(res$largest_positive ,  3.38953139E38)
  expect_equal(res$largest_negative , -3.38953139E38)
  expect_equal(res$smallest_positive, 1.175494351E-38)


  # 8 bit float: 1, 3, 5
  # https://www.cs.jhu.edu/~jorgev/cs333/readings/8-Bit_Floating_Point.pdf
  res <- float_info(float_bits = c(1, 3, 4))
  expect_equal(res$largest_positive ,  15.5)
  expect_equal(res$largest_negative , -15.5)
  expect_equal(res$smallest_positive,  0.25)


  expect_error(float_info(c(1)))

})


test_that("allocate_float_bits works", {

  # Single precision
  res <- allocate_float_bits(nbits = 32, max_value = 3.3e38, signed = TRUE)
  expect_equal(res, c(1, 8, 23))

  # Half precisiion
  res <- allocate_float_bits(nbits = 16, max_value = 65500, signed = TRUE)
  expect_equal(res, c(1, 5, 10))

  # bfloat16
  res <- allocate_float_bits(nbits = 16, max_value = 3.3e38, signed = TRUE)
  expect_equal(res, c(1, 8, 7))

  # 8 bit float: 1, 3, 5
  res <- allocate_float_bits(nbits = 8, max_value = 15, signed = TRUE)
  expect_equal(res, c(1, 3, 4))



  # Half precisiion reduced bits until it can't allocate
  expect_error(
    res <- allocate_float_bits(nbits = 11, max_value = 3e38),
    "fewer than 4 bits"
  )
})




















