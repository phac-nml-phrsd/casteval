test_that("filter_forecast_time() works", {
  expect_equal(
    filter_forecast_time(
      data.frame(time=1:10, raw=11:20),
      NULL
    ),
    data.frame(time=1:10, raw=11:20)
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=1:10,raw=11:20),
      5
    ),
    data.frame(time=5:10,raw=15:20)
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=numeric(0), raw=numeric(0)),
      5
    ),
    data.frame(time=numeric(0),raw=numeric(0))
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=1:5, raw=11:15),
      6
    ),
    data.frame(time=numeric(0),raw=numeric(0))
  )
})

test_that("validate_fcst_obs_pair() works", {
  expect_equal(
    validate_fcst_obs_pair(
      create_forecast(data.frame(time=1:10, raw=11:20)),
      data.frame(time=101:110, raw=111:120)
    ),
    NULL
  )

  expect_error(
    validate_fcst_obs_pair(
      create_forecast(data.frame(time=1:10, raw=11:20)),
      data.frame(time=lubridate::ymd("2024-01-01"), raw=5)
    ),
    "observations time type must match forecast time type"
  )
})
