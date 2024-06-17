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
      data.frame(time=1:3, raw=10:12)
    ),
    "`obs` column already present in forecast data frame"
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=2:3, raw=7:8)
    ),
    "missing observations for some forecast time points"
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=1:3, raw=c(NA, 7, 8))
    )
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, mean=4:6),
      data.frame(time=1:3, raw=7:9)
    ),
    data.frame(time=1:3, mean=4:6, obs=7:9)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=1:3, raw=c(NA, 7, 8)),
      na.rm=TRUE
    ),
    data.frame(time=2:3, raw=5:6, obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=2:3, raw=7:8),
      na.rm=TRUE
    ),
    data.frame(time=2:3, raw=5:6, obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, raw=4:6),
      data.frame(time=0:4, raw=7:11)
    ),
    data.frame(time=1:3, raw=4:6, obs=8:10)
  )
})
