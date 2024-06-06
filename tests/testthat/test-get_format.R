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
    "inconsistent.*types"
  )
  expect_error(
    get_time_type(list("January 1", "January 2")),
    "unsupported.*types"
  )
})

test_that("get_format() validates", {
  expect_error(get_format(data.frame()), "data frame is empty")
  expect_error(get_format(data.frame(raw=1:3), "time.*column"))
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
    get_format(dplyr::tibble(time=1:3, raw=list(NULL,1,2))),
    "raw.*inconsistent lengths"
  )
})

# test_that("get_format() returns correct format", {
#   expect_equal()
# })
