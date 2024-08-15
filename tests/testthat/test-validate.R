test_that("validate_time() works", {
  expect_equal(
    validate_time(5, create_forecast(data.frame(time=6,val=7))),
    NULL
  )

  expect_equal(
    validate_time(lubridate::ymd("2024-01-02"), create_forecast(data.frame(time=lubridate::ymd("2024-01-01"), val=7))),
    NULL
  )

  expect_equal(
    validate_time(
      lubridate::ymd_hms("2024-01-01_03:03:03"),
      create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_04:04:04"),val=8))
    ),
    NULL
  )
  
  expect_error(
    validate_time(
      lubridate::ymd_hms("2024-01-01_04:04:04"),
      create_forecast(data.frame(time=5,val=6))
    ),
    "type of `t` does not match `fcst\\$time_type`"
  )

  expect_error(
    validate_time(
      lubridate::ymd("2024-01-01"),
      create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_00:00:00"),val=6))
    ),
    "type of `t` does not match `fcst\\$time_type`"
  )

  expect_error(
    validate_time(
      lubridate::ymd_hms("2024-01-01_00:00:00"),
      create_forecast(data.frame(time=lubridate::ymd("2024-01-01"),val=7))
    ),
    "type of `t` does not match `fcst\\$time_type`"
  )
})

test_that("validate_column() works", {
  df1 <- data.frame(time=numeric(0),val=NULL)
  df2 <- data.frame()
  df3 <- dplyr::tibble(time=1:3, val=c(1,2,3))
  df4 <- dplyr::tibble(time=1:3, val_q2.5=4:6)

  expect_equal(validate_column(df1, "time"), NULL)
  expect_error(validate_column(df1, "val"), "not in data frame")

  expect_error(validate_column(df2, ""), "not in data frame")

  expect_equal(validate_column(df3, "val"), NULL)

  expect_equal(validate_column(df4, "val_q2.5"), NULL)

  expect_error(validate_column(df3, "val_mean"), "not in data frame")

  expect_error(validate_column(df4, "val_q"), "not in data frame")
})

test_that("validate_group_names() works", {
  expect_error(
    validate_group_names(c("hi", "")),
    "empty string present in group names"
  )

  expect_equal(
    validate_group_names(c("variable", "scenario", "___12345")),
    NULL
  )

  expect_equal(
    validate_group_names(character(0)),
    NULL
  )
})