

test_that("hex_colour_to_lofi", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Exact answer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  orig <- c('#FFFFFF')
  lofi <- hex_colour_to_lofi(orig, rgb_bits = c(3, 3, 2))
  res  <- lofi_to_hex_colour(lofi, rgb_bits = c(3, 3, 2))

  expect_identical(res, orig)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Exact answer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  orig <- c('#000000')
  lofi <- hex_colour_to_lofi(orig, rgb_bits = c(3, 3, 2))
  res  <- lofi_to_hex_colour(lofi, rgb_bits = c(3, 3, 2))

  expect_identical(res, orig)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rounding to nearest colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  orig <- c('#010101')
  lofi <- hex_colour_to_lofi(orig, rgb_bits = c(3, 3, 2))
  res  <- lofi_to_hex_colour(lofi, rgb_bits = c(3, 3, 2))

  expect_identical(res, '#000000')
})



test_that("hex_colour_to_lofi rejects 0 rgb_bits", {
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(0, 3, 2)), "range")
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(1, 9, 2)), "range")
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(7, 3, 0)), "range")
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(-1, 2, 3)), "range")
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(3, 2)), "range")
  expect_error(hex_colour_to_lofi('#ffffff', rgb_bits = c(3, 2, 4, 5)), "range")
})



test_that("lofi_to_hex_colour rejects 0 rgb_bits", {
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(0, 3, 2)), "range")
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(1, 9, 2)), "range")
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(7, 3, 0)), "range")
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(-1, 2, 3)), "range")
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(3, 2)), "range")
  expect_error(lofi_to_hex_colour(12L, rgb_bits = c(3, 2, 4, 5)), "range")
})
