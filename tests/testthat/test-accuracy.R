test_that("validate_interval() works", {
  expect_error(
    validate_interval("a"),
    "must be either NULL or vector of 2 numbers"
  )

  expect_error(
    validate_interval(2),
    "vector must have length 2"
  )

  expect_error(
    validate_interval(1:3),
    "vector must have length 2"
  )

  expect_error(
    validate_interval(list(1,2)),
    "must be either NULL or vector of 2 numbers"
  )

  expect_error(
    validate_interval(c(2,2)),
    "`interval.*1.*` must be less than `interval.*2.*`"
  )
  
  expect_error(
    validate_interval(c(2,1)),
    "`interval.*1.*` must be less than `interval.*2.*`"
  )

  expect_error(
    validate_interval(c(-1, 90)),
    "`interval.*1.*` and `interval.*2.*` must be between 0 and 100, inclusive"
  )

  expect_error(
    validate_interval(c(10, 101)),
    "`interval.*1.*` and `interval.*2.*` must be between 0 and 100, inclusive"
  )

  expect_equal(
    validate_interval(c(0,100)),
    NULL
  )
})
