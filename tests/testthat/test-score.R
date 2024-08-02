test_that("score() works", {
  expect_error(
    score(5),
    "`fcsts` must be a single forecast object or list of forecast objects"
  )
})