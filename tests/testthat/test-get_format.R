test_that("column_all() works", {
  expect_equal(
    column_all(list(1, 2.5, 1.4e7, -10, 5L, Inf, NaN), is.numeric),
    TRUE
  )
  expect_equal(
    column_all(list(1, 2, 3), function(x) {x < 3}),
    FALSE
  )
  expect_equal(
    column_all(list(1, NA), is.numeric),
    FALSE
  )
  expect_equal(
    column_all(c(1,NA), is.numeric),
    TRUE
  )
})

test_that("get_time_type() works", {
  expect_error(get_time_type(list()), "no times present")
  expect_equal(
    get_time_type(c(1,2,3)),
    "numeric"
  )
  expect_equal(
    get_time_type(c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02"))),
    "date"
  )
  expect_equal(
    get_time_type(list(lubridate::ymd_hms("2024-01-01_12:34:56"))),
    "date-time"
  )
  expect_error(
    get_time_type(list(1, lubridate::ymd("2024-01-01"))),
    "time column has.*inconsistent.*types"
  )
  expect_error(
    get_time_type(list("January 1", "January 2")),
    "time column has.*unsupported.*types"
  )
})

test_that("get_format() validates", {
  expect_error(get_format(data.frame()), "data frame is empty")
  expect_error(get_format(data.frame(raw=1:3)), "does not contain.*time.*column")
  expect_error(
    get_format(data.frame(time=1:3, raw=4:6, mean=7:9)),
    "both raw and mean values provided"
  )
  expect_error(
    get_format(data.frame(time=1:3, raw=4:6, quant_50=7:9)),
    "both raw and quantile values provided"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3, raw=list(1,"a",3))),
    "raw column not all numeric"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3, raw=list(c(1,2), c(1,2,3), c(1,2)))),
    "raw.*inconsistent lengths"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3, raw=list(numeric(0),1,2))),
    "raw.*inconsistent lengths"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3, mean=list(1,2,FALSE))),
    "mean column not all numeric"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3, quant_25=list(1,2,"hi"))),
    "quant.*column not all numeric"
  )
  expect_error(
    get_format(data.frame(time=1, quant_=2)),
    "quantile column name.*badly formatted"
  )
  expect_error(
    get_format(data.frame(time=1, quant_1_2=2)),
    "quantile column name.*badly formatted"
  )
  expect_error(
    get_format(data.frame(time=1, quant_abc123=2)),
    "quantile column.*does not specify percentage"
  )
  expect_error(
    get_format({
      df<-data.frame(time=1)
      df[["quant_-1"]]<-2
      df
    }),
    "quantile percentage out of range"
  )
  expect_error(
    get_format(data.frame(time=1, quant_101=2)),
    "quantile percentage out of range"
  )
  expect_error(
    get_format(dplyr::tibble(time=1:3)),
    "contains no data columns"
  )
})

test_that("get_format() returns correct format", {
  expect_equal(
    get_format(data.frame(time=1:3, raw=4:6)),
    list(time_type="numeric", data_types="raw")
  )
  expect_equal(
    get_format(data.frame(time=c(
      lubridate::ymd("2024-01-01"),
      lubridate::ymd("2024-01-02"),
      lubridate::ymd("2024-01-03")),
      mean=c(10, 11, 12))),
      list(time_type="date", data_types="mean")
  )
  expect_equal(
    get_format(data.frame(time=c(
      lubridate::ymd_hms("2024-01-01_01:01:01"),
      lubridate::ymd_hms("2024-01-02_02:02:02"),
      lubridate::ymd_hms("2024-01-03_03:03:03")),
      quant_45=c(10, 11, 12))),
      list(time_type="date-time", data_types="quant")
  )
  expect_equal(
    get_format(data.frame(time=1:3, quant_0=4:6, quant_100=7:9, mean=10:12)),
    list(time_type="numeric", data_types=c("mean","quant"))
  )
})
