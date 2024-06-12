test_that("filter_forecast_time() works", {
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
