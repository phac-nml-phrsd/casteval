test_that("validate_forecast() works", {
  expect_error(
    validate_forecast(5),
    "forecast must be named list"
  )

  expect_error(
    validate_forecast(data.frame(time=1:3,raw=4:6)),
    "forecast must be named list.*not just a data frame"
  )

  expect_error(
    validate_forecast(list(1,2,3)),
    "forecast must specify.*time_type"
  )

  expect_error(
    validate_forecast(list(time_type="whatever")),
    "forecast must specify.*data_types"
  )

  expect_error(
    validate_forecast(list(time_type="hi", data_types="bye")),
    "forecast must contain `data`"
  )

  expect_error(
    validate_forecast(list(
      time_type="hi", data_types="bye", data=data.frame(time=1:3, raw=4:6))
    ),
    "stated time type does not match data frame time type"
  )
})

test_that("validate_forecast() checks data types properly", {
  expect_error(
    validate_forecast(list(
      time_type="numeric", data_types="mean", data=data.frame(time=1:3, raw=4:6)
    )),
    "stated data types do not match data frame data types"
  )

  expect_error(
    validate_forecast(list(
      time_type="numeric", data_types="mean", data=data.frame(time=1:3, mean=4:6, quant_50=7:9)
    )),
    "stated data types do not match data frame data types"
  )

  expect_error(
    validate_forecast(list(
      time_type="numeric", data_types=c("raw", "quant"), data=data.frame(time=1:3, raw=4:6)
    )),
    "stated data types do not match data frame data types"
  )

  expect_equal(
    validate_forecast(list(
      time_type="numeric", data_types=c("quant", "mean"), data=data.frame(time=1:3, mean=4:6, quant_50=7:9)
    )),
    NULL
  )
  
  expect_equal(
    validate_forecast(list(
      time_type="numeric", data_types=c("mean", "quant"), data=data.frame(time=1:3, mean=4:6, quant_50=7:9)
    )),
    NULL
  )
})
