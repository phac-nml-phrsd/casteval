test_that("validate_quant_order() works", {
  expect_error(
    validate_quant_order(data.frame(time=1:3, val_q25=4:6, val_q75=c(4,4,4))),
    "quantiles have impossible values in row 2"
  )
  expect_equal(
    validate_quant_order(data.frame(time=1:3, val_q25=4:6)),
    NULL
  )
  expect_equal(
    validate_quant_order(data.frame(time=1:3, val_2.5=4:6, val_50=4:6)),
    NULL
  )
  expect_equal(
    validate_quant_order(data.frame(time=1:3, val_q2.5=4:6, val_q50=7:9)),
    NULL
  )
})

test_that("validate_quant_pair() works", {
  expect_error(
    validate_quant_pair("a"),
    "quantile pair must be vector of 2 numbers"
  )

  expect_error(
    validate_quant_pair(2),
    "quantile pair must have length 2"
  )

  expect_error(
    validate_quant_pair(1:3),
    "quantile pair must have length 2"
  )

  expect_error(
    validate_quant_pair(list(1,2)),
    "quantile pair must be vector of 2 numbers"
  )

  expect_error(
    validate_quant_pair(c(2,2)),
    "first quantile in pair must be less than second quantile in pair"
  )
  
  expect_error(
    validate_quant_pair(c(2,1)),
    "first quantile in pair must be less than second quantile in pair"
  )

  expect_error(
    validate_quant_pair(c(-1, 90)),
    "quantiles in pair must be between 0 and 100, inclusive"
  )

  expect_error(
    validate_quant_pair(c(10, 101)),
    "quantiles in pair must be between 0 and 100, inclusive"
  )

  expect_equal(
    validate_quant_pair(c(0,100)),
    NULL
  )
})

test_that("validate_quant() works", {
  expect_equal(validate_quant(50), NULL)
  expect_equal(validate_quant(0), NULL)
  expect_equal(validate_quant(100), NULL)
  expect_equal(validate_quant(2.5), NULL)

  expect_error(validate_quant("a"), "`quant` must be numeric")
  expect_error(validate_quant(-1), "`quant` must be between 0 and 100")
  expect_error(validate_quant(numeric(0)), "`quant` must be exactly 1 number, received numeric vector of length 0")
  expect_error(validate_quant(c(1,2)), "`quant` must be exactly 1 number, received numeric vector of length 2")
})