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
})

test_that("get_time_point() works", {
  expect_error(
    get_time_point(data.frame(time=1:3,val=4:6), 4),
    "no rows in data frame with given time"
  )

  expect_error(
    get_time_point(data.frame(time=c(1,2,2,3), val=4:7), 2),
    "multiple rows in data frame with given time"
  )

  expect_equal(
    get_time_point(data.frame(time=1:3, val=4:6, val_mean=7:9), 2),
    data.frame(time=2, val=5, val_mean=8)
  )
})

