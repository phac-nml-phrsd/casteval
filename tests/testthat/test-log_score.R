test_that("log_score() validates", {
  expect_error(
    log_score(
      create_forecast(dplyr::tibble(time=1:3, val_mean=4:6)),
      data.frame(time=1:3, val_obs=4:6)
    ),
    "log_score\\(\\) requires raw forecast data"
  )

  expect_error(
    log_score(
      create_forecast(dplyr::tibble(time=c(1,1,2), val=c(2,3, 4))),
      data.frame(time=1:2, val_obs=3:4)
    ),
    "not enough data points at time 2 \\(at least 2 required to calculate KDE\\)"
  )

  df <- dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), val=c(1:5, 1:5, 1:5))

  expect_error(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      at=2,
      after=2
    ),
    "`at` and `after` parameters cannot both be provided"
  )

  expect_error(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      after=lubridate::ymd("2024-01-01")
    ),
    "`after` not numeric"
  )

  expect_error(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      after=5
    ),
    "`after` cannot be used if `fcst.*forecast_time` is NULL"
  )

  expect_error(
    log_score(
      create_forecast(df, forecast_time=2),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      after=2
    ),
    "score was not calculated for time 4"
  )

  expect_error(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5))
    ),
    "either `at` or `after` must be provided"
  )

  expect_error(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      at=1,
      bw=c(1,1)
    ),
    "`bw` must be either NULL or a single number"
  )
})

test_that("log_score() works", {
  df <- dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), val=c(1:5, 1:5, 1:5))

  expect_equal(
    log_score(
      create_forecast(df),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      summarize=FALSE
    ),
    dplyr::tibble(
      time=1:3, val_obs=c(-1, 2.5, 5), score=c(-4.03779, -1.649454, -2.004065)
    ),
    tolerance=0.0001
  )

  expect_equal(
    log_score(
      create_forecast(df, forecast_time=1),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      at=2
    ),
    -1.649454,
    tolerance=0.0001
  )

  expect_equal(
    log_score(
      create_forecast(df, forecast_time=1),
      data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
      after=1
    ),
    -1.649454,
    tolerance=0.0001
  )

  df2 <- dplyr::tibble(time=c(1,1), val=c(1,1))

  expect_equal(
    log_score(
      create_forecast(df2),
      data.frame(time=1, val_obs=1),
      at=1
    ),
    Inf
  )

  expect_equal(
    log_score(
      create_forecast(df2),
      data.frame(time=1, val_obs=1.1),
      at=1
    ),
    -Inf
  )

  expect_equal(
    log_score(
      create_forecast(dplyr::tibble(time=c(1,1,1), val=c(1,2,3))),
      data.frame(time=1, val_obs=27),
      at=1
    ),
    -715.7497,
    tolerance=0.0001
  )

  expect_equal(
    log_score(
      create_forecast(dplyr::tibble(time=c(1,1,1), val=c(1,2,3))),
      data.frame(time=1, val_obs=28),
      at=1
    ),
    -Inf
  )

  # generated with `rnorm(100)`
  dat <- c(1.65333590, -0.45354373, -0.72833227, -0.57932896,  0.22393422,  1.66474380,  1.25860070, -0.45031426,
    -0.58147908, -1.73164891,  0.24132984,  0.72108102,  0.27306302,  0.78953771,  0.21258096,  0.51148515,
    1.60507022,  0.13291209,  2.07626932,  0.28595342,  2.02495293,  0.18406124, -1.14137975,  0.65767118,
    -1.81017731,  1.08294741, -0.65729608, -1.01951425, -0.06479469,  0.33779109, -1.68815086, -0.27856043,
    -0.33903125,  0.50082201,  1.83337494, -0.49905936, -0.49954973,  0.93032174,  1.52456456,  0.41078019,
    0.51515480,  2.36816279, -1.70674549,  0.71729515,  0.34001180,  0.04716158,  1.02993176, -0.20778095,
    -0.33842630, -0.91057128, -1.22348392,  0.85716805,  1.57524314,  0.33917055,  0.27631658,  2.08379505,
    -0.41617193, -1.05603959, -2.36761241, -0.90434849,  0.61006285, -0.20332468,  1.03185942, -0.33730511,
    -0.39846780,  0.84869596,  0.23811838, -1.16306244,  0.57000307, -0.36170627, -0.85645369,  0.11934929,
    1.31981967, -0.63112338,  0.53136779,  1.34573168,  1.44116216, -1.70906870,  1.47288145, -1.06678870,
    1.02354016, -1.62647574, -0.23338090,  0.66764487,  0.67915059,  1.83020184,  0.79276787, -0.25752067,
    -0.36335945, -0.27205022,  1.43151611, -0.30642817, -0.73670236,  0.47512493, -0.87048155, -0.57383307,
    -1.58870961, -0.93340630, -1.05583011,  1.55527561)
  
  expect_equal(
    log_score(
      create_forecast(dplyr::tibble(time=rep(1,100), val=dat)),
      data.frame(time=1, val_obs=0),
      at=1
    ),
    -1.115905,
    tolerance=0.001
  )

  expect_equal(
    log_score(
      create_forecast(dplyr::tibble(time=rep(1,100), val=dat)),
      data.frame(time=1, val_obs=0),
      at=1,
      bw=1
    ),
    -1.31011,
    tolerance=0.001
  )

  # check that the internals of scoringRules::logs_sample work the way we expect
  # (specifically the way the bandwidth is calculated) 
  dat2 <- c(5,6,6,7,5.5,1,8,7,6,4)
  s1 <- log_score(
    create_forecast(data.frame(time=rep(1,10), val=dat2)),
    data.frame(time=1, val_obs=4.1),
    at=1
  )
  s2 <- log_score(
    create_forecast(data.frame(time=rep(1,10), val=dat2)),
    data.frame(time=1, val_obs=4.1),
    at=1,
    bw=bw.nrd(dat2)
  )
  expect_equal(s1,s2)
})

test_that("make_log_score() works", {
  # copied from above
  dat <- c(1.65333590, -0.45354373, -0.72833227, -0.57932896,  0.22393422,  1.66474380,  1.25860070, -0.45031426,
    -0.58147908, -1.73164891,  0.24132984,  0.72108102,  0.27306302,  0.78953771,  0.21258096,  0.51148515,
    1.60507022,  0.13291209,  2.07626932,  0.28595342,  2.02495293,  0.18406124, -1.14137975,  0.65767118,
    -1.81017731,  1.08294741, -0.65729608, -1.01951425, -0.06479469,  0.33779109, -1.68815086, -0.27856043,
    -0.33903125,  0.50082201,  1.83337494, -0.49905936, -0.49954973,  0.93032174,  1.52456456,  0.41078019,
    0.51515480,  2.36816279, -1.70674549,  0.71729515,  0.34001180,  0.04716158,  1.02993176, -0.20778095,
    -0.33842630, -0.91057128, -1.22348392,  0.85716805,  1.57524314,  0.33917055,  0.27631658,  2.08379505,
    -0.41617193, -1.05603959, -2.36761241, -0.90434849,  0.61006285, -0.20332468,  1.03185942, -0.33730511,
    -0.39846780,  0.84869596,  0.23811838, -1.16306244,  0.57000307, -0.36170627, -0.85645369,  0.11934929,
    1.31981967, -0.63112338,  0.53136779,  1.34573168,  1.44116216, -1.70906870,  1.47288145, -1.06678870,
    1.02354016, -1.62647574, -0.23338090,  0.66764487,  0.67915059,  1.83020184,  0.79276787, -0.25752067,
    -0.36335945, -0.27205022,  1.43151611, -0.30642817, -0.73670236,  0.47512493, -0.87048155, -0.57383307,
    -1.58870961, -0.93340630, -1.05583011,  1.55527561)

  fc <- create_forecast(dplyr::tibble(time=rep(1,100), val=dat), forecast_time=1)
  obs <- data.frame(time=1, val_obs=0)
  
  logs <- make_log_score(at=1)
  expect_equal(logs(fc, obs), -1.115905, tolerance=0.001)
  logs2 <- make_log_score(after=0, bw=0.3)
  expect_equal(logs2(fc, obs), -1.151939, tolerance=0.001)
})