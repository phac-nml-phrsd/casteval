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

  expect_error(
    validate_forecast(list(
      time_type="numeric", data_types="raw", data=data.frame(time=1,raw=4), forecast_time=lubridate::ymd("2024-01-01")
    )),
    "type of `t` does not match `fcst.*time_type`"
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

test_that("validate_time() works", {
  expect_equal(
    validate_time(5, create_forecast(data.frame(time=6,raw=7))),
    NULL
  )

  expect_equal(
    validate_time(lubridate::ymd("2024-01-02"), create_forecast(data.frame(time=lubridate::ymd("2024-01-01"), raw=7))),
    NULL
  )

  expect_equal(
    validate_time(
      lubridate::ymd_hms("2024-01-01_03:03:03"),
      create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_04:04:04"),raw=8))
    ),
    NULL
  )
  
  expect_error(
    validate_time(
      lubridate::ymd_hms("2024-01-01_04:04:04"),
      create_forecast(data.frame(time=5,raw=6))
    ),
    "type of `t` does not match `fcst.*time_type`"
  )

  expect_error(
    validate_time(
      lubridate::ymd("2024-01-01"),
      create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_00:00:00"),raw=6))
    ),
    "type of `t` does not match `fcst.*time_type`"
  )

  expect_error(
    validate_time(
      lubridate::ymd_hms("2024-01-01_00:00:00"),
      create_forecast(data.frame(time=lubridate::ymd("2024-01-01"),raw=7))
    ),
    "type of `t` does not match `fcst.*time_type`"
  )
})

test_that("validate_column() works", {
  df1 <- data.frame(time=numeric(0),raw=NULL)
  df2 <- data.frame()
  df3 <- dplyr::tibble(time=1:3, raw=list(1,2,3))
  df4 <- dplyr::tibble(time=1:3, quant_2.5=4:6)

  expect_equal(validate_column(df1, "time"), NULL)
  expect_error(validate_column(df1, "raw"), "not in data frame")

  expect_error(validate_column(df2, ""), "not in data frame")

  expect_equal(validate_column(df3, "raw"), NULL)

  expect_equal(validate_column(df4, "quant_2.5"), NULL)

  expect_error(validate_column(df3, "mean"), "not in data frame")

  expect_error(validate_column(df4, "quant_"), "not in data frame")
})
