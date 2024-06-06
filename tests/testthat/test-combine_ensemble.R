test_that("widen_NULL() accepts empty list", {
  expect_equal(casteval:::widen_NULL(list(), 1), list())
})

test_that("widen_NULL() widens to 1", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, 1, NULL, 2, NULL, 3), 1),
    list(NA_real_, 1, NA_real_, 2, NA_real_, 3)
  )
})

test_that("widen_NULL() widens to multiple", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, c(1,2,3), NULL), 3),
    list(c(NA_real_, NA_real_, NA_real_), c(1,2,3), c(NA_real_, NA_real_, NA_real_))
  )
})

test_that("widen_NULL() widens to 0", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, 1, NA, c(2,3,4), NULL), 0),
    list(numeric(0), 1, NA, c(2,3,4), numeric(0))
  )
})
