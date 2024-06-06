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
    "inconsistent"
  )
  expect_error(
    get_time_type(list("January 1", "January 2")),
    "unsupported"
  )
})

# test_that("get_format() works", {
#   expect_equal(
#     column_all(c(1,2), is.numeric),
#     TRUE
#   )
# })
