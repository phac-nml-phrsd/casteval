test_that("get_time_type() works", {
  expect_error(get_time_type(data.frame(val=1:3)), "data frame does not contain time column")
  expect_equal(
    get_time_type(data.frame(time=c(1,2,3))),
    "numeric"
  )
  expect_equal(
    get_time_type(data.frame(time=c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02")))),
    "date"
  )
  expect_equal(
    get_time_type(data.frame(time=lubridate::ymd_hms("2024-01-01_12:34:56"))),
    "date-time"
  )
  expect_error(
    get_time_type(data.frame(time=c("January 1", "January 2"))),
    "time column has unsupported type"
  )
})
