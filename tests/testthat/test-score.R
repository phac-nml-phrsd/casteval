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
      data.frame(time=101:110, obs=111:120)
    ),
    NULL
  )

  expect_error(
    validate_fcst_obs_pair(
      create_forecast(data.frame(time=1:10, raw=11:20)),
      data.frame(time=lubridate::ymd("2024-01-01"), obs=5)
    ),
    "observations time type must match forecast time type"
  )
})

test_that("remove_raw_NAs() works", {
  expect_error(
    remove_raw_NAs(data.frame(time=6, mean=3)),
    "data frame does not contain `raw` column"
  )

  expect_error(
    remove_raw_NAs(dplyr::tibble(time=4:5, raw=list(c(1,NA), c(NA,NA)))),
    "data frame contains row with no raw data"
  )

  expect_equal(
    remove_raw_NAs(dplyr::tibble(time=1:3, raw=list(c(NA, 1), c(1, 2, 3), c(NA, NA, 4)))),
    dplyr::tibble(time=1:3, raw=list(1, 1:3, 4))
  )
})

test_that("join_fcst_obs() works", {
  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6, obs=7:9),
      data.frame(time=1:3, obs=10:12)
    ),
    "`obs` column already present in forecast data frame"
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=2:3, obs=7:8)
    ),
    "missing observations for some forecast time points"
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=1:3, obs=c(NA, 7, 8))
    ),
    "missing observations for some forecast time points"
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=2:4, obs=c(NA, NA, 7)),
      na.rm=TRUE
    ),
    "no rows remain after removing NA observations"
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, mean=4:6),
      data.frame(time=1:3, obs=7:9)
    ),
    data.frame(time=1:3, mean=4:6, obs=7:9)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=1:3, obs=c(NA, 7, 8)),
      na.rm=TRUE
    ),
    data.frame(time=2:3, raw=5:6, obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=2:3, obs=7:8),
      na.rm=TRUE
    ),
    data.frame(time=2:3, raw=5:6, obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=0:4, obs=7:11)
    ),
    data.frame(time=1:3, raw=4:6, obs=8:10)
  )
})

test_that("get_time_point() works", {
  expect_error(
    get_time_point(data.frame(time=1:3,raw=4:6), 4),
    "no rows in data frame with given time"
  )

  expect_error(
    get_time_point(data.frame(time=c(1,2,2,3), raw=4:7), 2),
    "multiple rows in data frame with given time"
  )

  expect_equal(
    get_time_point(data.frame(time=1:3, raw=4:6, mean=7:9), 2),
    data.frame(time=2, raw=5, mean=8)
  )
})
