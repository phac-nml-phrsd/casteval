test_that("is_forecast() works", {
  expect_equal(is_forecast(5), FALSE)
  expect_equal(is_forecast(list()), FALSE)

  expect_equal(
    is_forecast(c(name=5,forecast_time=5,data=7)),
    FALSE
  )

  expect_equal(
    is_forecast(data.frame(name=1,forecast_time=2,data=3)),
    FALSE
  )

  expect_equal(
    is_forecast(list(name=NULL,forecast_time=NULL,data=3)),
    TRUE
  )

  expect_equal(
    is_forecast(list(create_forecast(data.frame(time=1,val=1)))),
    FALSE
  )
})

test_that("is_valid_forecast() works", {
  expect_equal(is_valid_forecast(5), FALSE)
  expect_equal(is_valid_forecast(list()), FALSE)

  expect_equal(
    is_valid_forecast(c(name=5,forecast_time=5,data=7)),
    FALSE
  )

  expect_equal(
    is_valid_forecast(data.frame(name=1,forecast_time=2,data=3)),
    FALSE
  )

  expect_equal(
    is_valid_forecast(list(name=NULL,forecast_time=NULL,data=3)),
    FALSE
  )

  expect_equal(
    is_valid_forecast(list(create_forecast(data.frame(time=1,val=1)))),
    FALSE
  )

  expect_equal(
    is_valid_forecast(data.frame(time=1,val=2)),
    FALSE
  )

  expect_equal(
    is_valid_forecast(create_forecast(data.frame(time=1,val=2))),
    TRUE
  )
})

test_that("filter_forecast_time() works", {
  expect_equal(
    filter_forecast_time(
      data.frame(time=1:10, val=11:20),
      NULL
    ),
    data.frame(time=1:10, val=11:20)
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=1:10,val=11:20),
      5
    ),
    data.frame(time=5:10,val=15:20)
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=numeric(0), val=numeric(0)),
      5
    ),
    data.frame(time=numeric(0),val=numeric(0))
  )

  expect_equal(
    filter_forecast_time(
      data.frame(time=1:5, val=11:15),
      6
    ),
    data.frame(time=numeric(0),val=numeric(0))
  )
})

test_that("join_fcst_obs() works", {
  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6, val_obs=7:9),
      data.frame(time=1:3, val_obs=10:12)
    ),
    "`val_obs` column already present in forecast data frame"
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6),
      data.frame(time=2:3, val_obs=7:8)
    ),
    data.frame(time=2:3, val=5:6, val_obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val_q50=4:6),
      data.frame(time=1:3, val_obs=c(NA, 7, 8))
    ),
    data.frame(time=2:3, val_q50=5:6, val_obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=c(1,2,3,1,2,3), sim=c(1,1,1,2,2,2), val=4:9),
      data.frame(time=c(1,2,3), val_obs=c(100,200,300))
    ),
    data.frame(time=c(1,2,3,1,2,3), sim=c(1,1,1,2,2,2), val=4:9, val_obs=c(100,200,300,100,200,300))
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=c(1,1,2,2,3,3), val=4:9),
      data.frame(time=c(1,2,3), val_obs=c(100,200,300))
    ),
    data.frame(time=c(1,1,2,2,3,3), val=4:9, val_obs=c(100,100,200,200,300,300))
  )

  expect_error(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6),
      data.frame(time=2:4, val_obs=c(NA, NA, 7))
    ),
    "forecast and observations data do not share any time points"
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val_mean=4:6),
      data.frame(time=1:3, val_obs=7:9)
    ),
    data.frame(time=1:3, val_mean=4:6, val_obs=7:9)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6),
      data.frame(time=1:3, val_obs=c(NA, 7, 8))
    ),
    data.frame(time=2:3, val=5:6, val_obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6),
      data.frame(time=2:3, val_obs=7:8)
    ),
    data.frame(time=2:3, val=5:6, val_obs=7:8)
  )

  expect_equal(
    join_fcst_obs(
      data.frame(time=1:3, val=4:6),
      data.frame(time=0:4, val_obs=7:11)
    ),
    data.frame(time=1:3, val=4:6, val_obs=8:10)
  )

  expect_equal(
    join_fcst_obs(groupex, groupex_obs),
    dplyr::inner_join(groupex, groupex_obs, dplyr::join_by(time, grp_variable, grp_province, grp_scenario))
  )
})

test_that("get_time_point() works", {
  expect_error(
    get_time_point(data.frame(time=1:3,val=4:6), 4),
    "no rows in data frame with given time"
  )

  expect_equal(
    get_time_point(data.frame(time=c(1,1,2,2,3,3), val=c(4,5,6,7,8,9)), 2),
    data.frame(time=c(2,2), val=c(6,7))
  )

  expect_equal(
    get_time_point(data.frame(time=1:3, val=4:6, val_mean=7:9), 2),
    data.frame(time=2, val=5, val_mean=8)
  )
})

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

test_that("calc_specified_time() works", {
  fc <- create_forecast(data.frame(time=1:3, val=4:6), forecast_time=2)

  expect_equal(calc_specified_time(fc, at=1), 1)
  expect_equal(calc_specified_time(fc, after=1), 3)

  expect_error(
    calc_specified_time(fc, at=1, after=1),
    "`at` and `after` parameters cannot both be provided"
  )

  expect_error(
    calc_specified_time(fc, after=FALSE),
    "`after` not numeric"
  )

  fc2 <- create_forecast(data.frame(time=1:3, val=4:6))

  expect_error(
    calc_specified_time(fc2, after=1),
    "`after` cannot be used if `fcst\\$forecast_time` is NULL"
  )

  expect_error(
    calc_specified_time(fc),
    "either `at` or `after` must be provided"
  )

  expect_error(
    calc_specified_time(fc, at=lubridate::as_date(1)),
    "type of `at` must match type of forecast times"
  )
})