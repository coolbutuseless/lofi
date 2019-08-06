

test_that("pack_integer works", {
  integer_spec <- list(type = 'integer', nbits = 7, signed = TRUE)
  integer_spec <- sanitise_spec_integer(integer_spec)
  pack_integer( 7, integer_spec)
  pack_integer(-7, integer_spec)

  expect_error(pack_integer(130, integer_spec), "cannot be represented")
  expect_error(pack_integer(NA_integer_, integer_spec), "'value' cannot be packed as integer")
  expect_error(pack_integer(NULL       , integer_spec), "'value' cannot be packed as integer")



  integer_spec <- list(type = 'integer', nbits = 7, signed = FALSE)
  integer_spec <- sanitise_spec_integer(integer_spec)
  pack_integer( 7, integer_spec)

  expect_error(pack_integer(-7, integer_spec) , "cannot be represented")
  expect_error(pack_integer(130, integer_spec), "cannot be represented")
  expect_error(pack_integer(NA_integer_, integer_spec), "'value' cannot be packed as integer")
  expect_error(pack_integer(NULL       , integer_spec), "'value' cannot be packed as integer")
})



test_that("pack_logical works", {
  logical_spec <- list(type = 'logical', nbits = 7)
  logical_spec <- sanitise_spec_logical(logical_spec)
  pack_logical( TRUE, logical_spec)
  pack_logical(FALSE, logical_spec)

  expect_error(pack_logical(130        , logical_spec), "is.logical")
  expect_error(pack_logical(NA_integer_, logical_spec), "'value' cannot be packed as logical")
  expect_error(pack_logical(NA         , logical_spec), "'value' cannot be packed as logical")
  expect_error(pack_logical(NULL       , logical_spec), "'value' cannot be packed as logical")
})



test_that("pack_double works", {
  double_spec <- list(type = 'double', float_name = 'bfloat16')
  double_spec <- sanitise_spec_double(double_spec)
  pack_double( 7, double_spec)

  expect_error(pack_double(NA_integer_, double_spec), "'value' cannot be packed as double")
  expect_error(pack_double(NULL       , double_spec), "'value' cannot be packed as double")
})



test_that("pack_colour works", {
  colour_spec <- list(type = 'colour', nbits = 8)
  colour_spec <- sanitise_spec_colour(colour_spec)
  pack_colour('#ffffff', colour_spec)

  expect_error(pack_colour(NA_integer_, colour_spec), "'value' cannot be packed as colour")
  expect_error(pack_colour(NULL       , colour_spec), "'value' cannot be packed as colour")
})



test_that("pack_custom works", {
  custom_spec <- list(type = 'custom', nbits = 8, pack_func = ~.x + 1, unpack_func = ~.x - 1, signed = FALSE)
  custom_spec <- sanitise_spec_custom(custom_spec)
  pack_custom(12, custom_spec)

  expect_error(pack_custom(NA_integer_, custom_spec), "'value' cannot be packed")
  expect_error(pack_custom(NULL       , custom_spec), "'value' cannot be packed")

  expect_error(pack_custom(1000, custom_spec), "cannot be represented")

  custom_spec$pack_func <- "a"
  expect_error(pack_custom(10, custom_spec), "Expecting a function")

  custom_spec$unpack_func <- "a"
  expect_error(unpack_custom(10L, custom_spec), "Expecting a function")
})



test_that("pack_scaled works", {
  scaled_spec <- list(type = 'scaled', nbits = 8)
  expect_error(sanitise_spec_scaled(scaled_spec), "Expecting number")


  scaled_spec <- list(type = 'scaled', nbits = 8, max = 255)
  scaled_spec <- sanitise_spec_scaled(scaled_spec)
  pack_scaled(12, scaled_spec)

  expect_error(pack_scaled(NA_integer_, scaled_spec), "'value' cannot be packed")
  expect_error(pack_scaled(NULL       , scaled_spec), "'value' cannot be packed")

  expect_error(pack_scaled(1000, scaled_spec), "in range")
  expect_error(pack_scaled(-10 , scaled_spec), "in range")

  scaled_spec$nbits <- 0
  expect_error(sanitise_spec_scaled(scaled_spec), "nbits in range")

})











test_that("pack() check for names", {
  pack_spec <- list(
    age     = list(type = 'integer', nbits = 7),
    check   = list(type = 'logical', nbits = 1),
    score   = list(type = 'double' , float_bits = c(0, 2, 5)),
    colour  = list(type = 'colour' , nbits = 8),
    grade   = list(type = 'choice' , nbits = 2, options = c('a', 'b', 'c', 'd'))
  )

  values <- list(
    age     = 23,
    check   = TRUE,
    score   = 1.1,
    colour  = '#FFFFFF'
  )

  pack_spec <- sanitise_pack_spec(pack_spec)
  expect_error(pack(values, pack_spec), "names.*aren't available")





  values <- list(
    age     = 23,
    check   = TRUE,
    score   = c(1.1, 2.1),
    colour  = '#FFFFFF',
    grade   = 'a'
  )
  expect_error(pack(values, pack_spec), "length 1")


})








