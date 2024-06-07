test_that("create_forecast() does input validation", {
  expect_error(create_forecast(5), "has invalid type")
  expect_error(create_forecast(list()), "list of data frames is empty")
  expect_error(create_forecast(list(5, data.frame())), "list.*non-data-frames")
  
  expect_error(
    create_forecast(list(
      data.frame(time=1,raw=2),
      data.frame(time=lubridate:::ymd("2024-01-01"),raw=2),
      data.frame(time=3,raw=4)
    )),
    "all data frames must have same time type"
  )

  expect_error(
    create_forecast(list(
      data.frame(time=1,raw=2),
      data.frame(time=2,mean=5)
    )),
    "all data frames must contain raw data"
  )
})
