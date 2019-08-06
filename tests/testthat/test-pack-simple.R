

test_that("simple round trip works", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pack_spec <- list(
    age     = list(type = 'integer', nbits = 2),
    check   = list(type = 'logical', nbits = 1),
    score   = list(type = 'double' , float_bits = c(0, 2, 5)),
    zscore  = list(type = 'double' , nbits = 5, maxval = 2),
    colour  = list(type = 'colour' , nbits = 8),
    grade   = list(type = 'choice' , nbits = 2, options = c('a', 'b', 'c', 'd')),
    stupid  = list(type = 'custom' , nbits = 3, pack_func = ~.x + 1, unpack_func = ~.x - 1),
    zz      = list(type = 'scaled' , nbits = 3, max = 7, cyclical = TRUE)
  )

  values <- list(
    age     = 3,
    check   = TRUE,
    score   = 1.1,
    zscore  = 1.5,
    colour  = '#FFFFFF',
    grade   = 'd',
    stupid  = 1,
    zz = 3
  )

  pack_spec <- sanitise_pack_spec(pack_spec)

  (int <- pack(values, pack_spec))
  res <- unpack(int, pack_spec)

  expect_equal(values, res, tolerance = 1e-2)

})
