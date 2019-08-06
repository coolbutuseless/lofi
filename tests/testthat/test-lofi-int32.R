
test_that("int32_to_bits works", {
  res <- int32_to_bits(5L)
  expect_equal(res,
               as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
                        0x01)))
})


test_that("int32_to_bits expects single int32", {
  expect_error(int32_to_bits(12.0))
  expect_error(int32_to_bits("hello"))
  expect_error(int32_to_bits(c(1L, 2L)))
  expect_error(int32_to_bits(as.raw(c(1L, 2L))))
})


test_that("bits_to_int32 works", {

  bits <- as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00,
                   0x01))

  res <- bits_to_int32(bits)

  expect_equal(res, 5L)
})


test_that("bits_to_int32 fails correctly", {
  expect_error(bits_to_int32(1L))
  expect_error(bits_to_int32(rep("a", 32)))
  expect_error(bits_to_int32(rep(as.raw(0), 33)))
})


test_that("int32_to_bits fails correctly", {
  expect_error(int32_to_bits(1), "is.integer")
  expect_error(int32_to_bits(TRUE), "is.integer")
})


test_that("int32_to_lofi works with positive integers", {
  input <- 8L
  inter <- int32_to_lofi(input, nbits = 5, signed = TRUE)
  expect_identical(input, inter)

  input <- 8L
  inter <- int32_to_lofi(input, nbits = 6, signed = TRUE)
  expect_identical(input, inter)

  input <- 8L
  inter <- int32_to_lofi(input, nbits = 6, signed = FALSE)
  expect_identical(input, inter)
})


test_that("int32_to_lofi works with negative integers", {
  input  <- -8L
  inter  <- int32_to_lofi(input, nbits = 5, signed = TRUE)
  output <- lofi_to_int32(inter, nbits = 5, signed = TRUE)
  expect_identical(input, output)

  input  <- -8L
  inter  <- int32_to_lofi(input, nbits = 6, signed = TRUE)
  output <- lofi_to_int32(inter, nbits = 6, signed = TRUE)
  expect_identical(input, output)
})





test_that("int32_to_lofi works with signed integers", {

  for (input in -128:127) {
    inter  <- int32_to_lofi(input, nbits = 8, signed = TRUE)
    output <- lofi_to_int32(inter, nbits = 8, signed = TRUE)
    expect_identical(input, output)
  }
})


test_that("int32_to_lofi works with unsigned integers", {
  for (input in 0:255) {
    inter  <- int32_to_lofi(input, nbits = 8, signed = FALSE)
    output <- lofi_to_int32(inter, nbits = 8, signed = FALSE)
    expect_identical(input, output)
  }
})


test_that("int-to-bits conversion working", {
  for (input in 0:255) {
    inter  <- lofi_to_bits(input, nbits = 8)
    output <- bits_to_lofi(inter)
    expect_identical(input, output)
  }
})



test_that("int32_to_lofi errors if values un-representable", {
  expect_error(int32_to_lofi(1000L, nbits = 8, signed = TRUE ), "cannot be represented")
  expect_error(int32_to_lofi(1000L, nbits = 8, signed = FALSE), "cannot be represented")
  expect_error(int32_to_lofi( 256L, nbits = 8, signed = TRUE ), "cannot be represented")
  expect_error(int32_to_lofi(-129L, nbits = 8, signed = TRUE ), "cannot be represented")
})



test_that("lofi_to_bits works", {
  expect_identical(lofi_to_bits(8L, nbits =  4), as.raw(c(1, 0, 0, 0)))
  expect_identical(lofi_to_bits(0L, nbits = 32), raw(32))
})




