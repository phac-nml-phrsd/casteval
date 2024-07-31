test_that("crps() works", {
  fc1 <- create_forecast(list(time=1, vals=list(0,0,0)), forecast_time=1)

  fc2 <- create_forecast(list(
    time=c(1,2,3),
    vals=list(c(1,1,1), c(2,2,2), c(3,3,3), c(4,4,4), c(5,5,5)), forecast_time=1)
  )

  # `rnorm(20) + 10`
  dat <- c(10.255639,  9.522598,  9.449350, 11.150542,  9.319630,  9.968895, 10.890636,
    9.361238,  9.975821,  8.979497, 11.532041,  9.869000, 10.397184,  9.839293,
    11.006322,  9.980781,  9.791051,  9.299143, 11.246390, 10.178951)
  fc3 <- create_forecast(data.frame(time=rep(1,20), val=dat), forecast_time=1)

  expect_error(
    crps(
      create_forecast(data.frame(time=1,val_q50=2)),
      data.frame(time=1,val_obs=2)
    ),
    "crps\\(\\) requires raw forecast data \\(`val`\\)"
  )

  expect_error(
    crps(fc1, data.frame(time=1,val_obs=1), at=6),
    "score was not calculated for time 6"
  )

  expect_equal(
    crps(fc1, data.frame(time=1, val_obs=5), after=0),
    5
  )

  expect_equal(
    crps(fc1, data.frame(time=1, val_obs=-5), at=1),
    5
  )

  expect_equal(
    crps(fc1, data.frame(time=1, val_obs=0), at=1),
    0
  )

  expect_equal(
    crps(fc2, data.frame(time=1:3, val_obs=c(3,4,5)), summarize=FALSE),
    dplyr::tibble(time=1:3, val_obs=3:5, score=c(.4, .6, 1.2))
  )

  
})