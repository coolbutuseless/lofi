


test_that("sanitise_spec_integer works", {
  spec_integer <- list(
    type   = 'integer',
    nbits  = 3,
    offset = 0L,
    mult   = 1L,
    signed = TRUE
  )
  res <- sanitise_spec_integer(spec_integer)
  expect_identical(spec_integer, res)
})


test_that("sanitise_spec_double works", {
  spec_double <- list(
    type       = 'double',
    float_name = 'half'
  )
  res <- sanitise_spec_double(spec_double)
  expect_equal(res$nbits, 16)
})



test_that("sanitise_spec_double throws errors when necessary", {
  spec_double <- list(
    type       = 'double',
    float_bits = NULL
  )
  expect_error(sanitise_spec_double(spec_double), "Invalid 'float_bits'")

  spec_double <- list(
    type       = 'double',
    float_bits = c(1, 2, 3, 4)
  )
  expect_error(sanitise_spec_double(spec_double), "Invalid 'float_bits'")

  spec_double <- list(
    type       = 'double',
    float_bits = c(1, 2)
  )
  expect_error(sanitise_spec_double(spec_double), "Invalid 'float_bits'")

  spec_double <- list(
    type       = 'double',
    float_bits = c(1, 2, NA)
  )
  expect_error(sanitise_spec_double(spec_double), "Invalid 'float_bits'")

  spec_double <- list(
    type       = 'double',
    float_bits = c('1', '2', '3')
  )
  expect_error(sanitise_spec_double(spec_double), "Invalid 'float_bits'")


  spec_double <- list(
    type       = 'double',
    nbits      = 10
  )
  expect_error(sanitise_spec_double(spec_double), "maxval")


})




test_that("sanitise_spec_logical works", {
  spec_logical<- list(
    type       = 'logical'
  )
  res <- sanitise_spec_logical(spec_logical)
  expect_equal(res$nbits, 1)
})





test_that("sanitise_spec_choice works", {
  spec_choice <- list(
    type       = 'choice',
    options    = c('a', 'b', 'c')
  )
  res <- sanitise_spec_choice(spec_choice)
  expect_equal(res$nbits, 2)
})


test_that("sanitise_spec_choice errors appropriately", {
  spec_choice <- list(
    type       = 'choice',
    options    = c()
  )
  expect_error(sanitise_spec_choice(spec_choice), "at least one element")


  spec_choice <- list(
    type       = 'choice',
    options    = c(1)
  )
  sanitise_spec_choice(spec_choice)


  spec_choice <- list(
    type       = 'choice',
    options    = c(1, 2, 3),
    nbits      = 1
  )
  expect_error(sanitise_spec_choice(spec_choice), "Not enough bits")
})







test_that("sanitise_spec_colour works", {
  spec_colour <- list(
    type       = 'colour',
    nbits      = 8
  )
  res <- sanitise_spec_colour(spec_colour)
  expect_equal(res$nbits, 8)
})


test_that("sanitise_spec_colour errors appropriately", {
  spec_colour <- list(
    type       = 'colour',
    nbits      = NA
  )
  expect_error(sanitise_spec_colour(spec_colour), "Invalid 'nbits'")

  spec_colour <- list(
    type       = 'colour',
    nbits      = NULL
  )
  expect_error(sanitise_spec_colour(spec_colour), "Invalid 'nbits'")


  spec_colour <- list(
    type       = 'colour',
    nbits      = c()
  )
  expect_error(sanitise_spec_colour(spec_colour), "Invalid 'nbits'")

  spec_colour <- list(
    type       = 'colour',
    nbits      = c(10, 11)
  )
  expect_error(sanitise_spec_colour(spec_colour), "Invalid 'nbits'")

  spec_colour <- list(
    type       = 'colour',
    nbits      = 1
  )
  expect_error(sanitise_spec_colour(spec_colour), "in range")

  spec_colour <- list(
    type       = 'colour',
    nbits      = 33
  )
  expect_error(sanitise_spec_colour(spec_colour), "in range")

  spec_colour <- list(
    type       = 'colour',
    nbits      = -10
  )
  expect_error(sanitise_spec_colour(spec_colour), "in range")

  spec_colour <- list(
    type       = 'colour',
    nbits      = "10"
  )
  expect_error(sanitise_spec_colour(spec_colour), "Invalid 'nbits'")
})





test_that("sanitise_spec_custom works", {
  spec_custom <- list(
    type        = 'custom',
    nbits       = 8,
    pack_func   = ~.x + 1,
    unpack_func = ~.x - 1
  )
  res <- sanitise_spec_custom(spec_custom)
  expect_equal(res$nbits, 8)


  spec_custom <- list(
    type        = 'custom',
    nbits       = 8,
    pack_func   = function(x) {x + 1},
    unpack_func = function(x) {x - 1}
  )
  res <- sanitise_spec_custom(spec_custom)
  expect_equal(res$nbits, 8)
})


test_that("sanitise_spec_custom errors appropriately", {
  spec_custom <- list(
    type        = 'custom',
    nbits       = 8,
    pack_func   = ~.x + 1,
    unpack_func = NULL
  )
  expect_error(sanitise_spec_custom(spec_custom), "Expecting formula or function")

  spec_custom <- list(
    type        = 'custom',
    nbits       = 8,
    pack_func   = NULL,
    unpack_func = ~.x + 1
  )
  expect_error(sanitise_spec_custom(spec_custom), "Expecting formula or function")

  spec_custom <- list(
    type        = 'custom',
    nbits       = 8,
    pack_func   = "x",
    unpack_func = ~.x + 1
  )
  expect_error(sanitise_spec_custom(spec_custom), "Expecting formula or function")


})












test_that("sanitise_pack_spec works", {

  pack_spec <- list(
    choice = list(type = 'choice', options = c('a', 'b', 'c')),
    colour = list(type = 'colour', nbits = 8)

  )
  res <- sanitise_pack_spec(pack_spec)
  expect_length(res, 2)
})



test_that("sanitise_pack_spec errors appropriately", {
  pack_spec <- list(
    choice = list(type = 'monkey', options = c('a', 'b', 'c')),
    colour = list(type = 'colour', nbits = 8)

  )
  expect_error(sanitise_pack_spec(pack_spec), "Invalid spec.type")


  pack_spec <- list(
  )
  expect_error(sanitise_pack_spec(pack_spec), "Invalid total")


  pack_spec <- list(
    list(nbits = 3)
  )

  expect_error(
    expect_warning(
      sanitise_pack_spec(pack_spec),
      "no 'type'"
    ),
  "no 'type'"
  )



})



test_that("expect_valid_nbits works", {
  expect_valid_nbits(1)
  expect_error(expect_valid_nbits(33  ), "Expecting nbits in range .1, 32.")
  expect_error(expect_valid_nbits(0   ), "Expecting nbits in range .1, 32.")
  expect_error(expect_valid_nbits(-1  ), "Expecting nbits in range .1, 32.")
  expect_error(expect_valid_nbits(1033), "Expecting nbits in range .1, 32.")


  expect_error(expect_valid_nbits(NA         ), "Expecting single")
  expect_error(expect_valid_nbits(NA_integer_), "Expecting single")
  expect_error(expect_valid_nbits(NULL       ), "Expecting single")
  expect_error(expect_valid_nbits("12"       ), "Expecting single")

})





