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
    "provided empty group name"
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

test_that("validate_plotting_groups() works", {
  expect_equal(
    validate_plotting_groups(data.frame(time=1, val=2)),
    NULL
  )

  expect_equal(
    validate_plotting_groups(data.frame(time=1:5, val=2:6, grp_variable=3:7)),
    NULL
  )

  expect_equal(
    validate_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=8:10)),
    NULL
  )

  expect_error(
    validate_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=10:12, grp_sce=13:15)),
    "more than 2 groups contain multiple values"
  )

  expect_equal(
    validate_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=c(1,1,1), grp_sce=13:15)),
    NULL
  )

  expect_equal(
    validate_plotting_groups(data.frame(time=1, val=2, grp_var=3, grp_loc=4, grp_sce=5, grp_foo=6, grp_bar=7)),
    NULL
  )
})