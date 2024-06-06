test_that("widen_NULL() accepts empty list", {
  expect_equal(casteval:::widen_NULL(list(), 1), list())
})

test_that("widen_NULL() widens to 1", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, 1, NULL, 2, NULL, 3), 1),
    list(NA, 1, NA, 2, NA, 3)
  )
})
