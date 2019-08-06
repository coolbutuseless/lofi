

test_that("choices_to_lofi works", {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Works with integers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  options <- sample(1:10, size = 100, replace = TRUE)
  for (choice in options) {
    lofi <- choice_to_lofi(choice, options)
    res  <- lofi_to_choice(lofi  , options)
    expect_identical(choice, res)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # works with character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  options <- sample(c(letters, LETTERS), size = 100, replace = TRUE)
  for (choice in options) {
    lofi <- choice_to_lofi(choice, options)
    res  <- lofi_to_choice(lofi  , options)
    expect_identical(choice, res)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bulk conversion
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lofi <- choice_to_lofi(options, options)
  res  <- lofi_to_choice(lofi   , options)
  expect_identical(res, options)
})


test_that("choice_to_lofi rejects factors", {
  expect_error(choice_to_lofi('a', as.factor(letters)), "is.factor")
})


test_that("choice_to_lofi NAs work", {
  orig <- NA_integer_
  lofi <- choice_to_lofi(orig, c(0, NA_integer_, 1, 2, 3))
  res  <- lofi_to_choice(lofi, c(0, NA_integer_, 1, 2, 3))
  expect_equal(orig, res)
})


test_that("can't encode values that aren't in options", {
  expect_error(choice_to_lofi(1L, 5:10), "can't pack")
  expect_error(lofi_to_choice(4, 1:3), "out of range")
})





