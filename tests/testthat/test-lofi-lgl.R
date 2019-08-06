

test_that("lgl_to_lofi works", {
  lgl <- sample(c(T, F), 10, replace = TRUE)
  lofi <- lgl_to_lofi(lgl)
  res  <- lofi_to_lgl(lofi)

  expect_identical(lgl, res)
})


test_that("lgl_to_lofi rejects non logical input", {
  expect_error(lgl_to_lofi(1), "is.logical")
  expect_error(lgl_to_lofi(1L), "is.logical")
  expect_error(lgl_to_lofi(0L), "is.logical")
})
