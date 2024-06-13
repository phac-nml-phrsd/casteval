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

test_that("accuracy() validates", {
  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, raw=4:6)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "`interval` parameter required for computing accuracy from raw data"
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_25=4:6)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "2 or more quantiles required to calculate accuracy"
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_25=4:6, quant_74=7:9)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "outermost quantiles must be equidistant from 50th percentile"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, raw=4:6),
      c(1, 25)
    ),
    "column named `quant_1` not in data frame"
  )
  
  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, raw=4:6),
      c(25,74.9)
    ),
    "column named `quant_74.9` not in data frame"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3, mean=4:6)),
      data.frame(time=1:3, raw=4:6),
      NULL
    ),
    "`raw` or `quant.*columns required to calculate accuracy"
  )
})

