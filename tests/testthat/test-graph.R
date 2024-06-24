test_that("wide2long() works", {
  df <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

  expect_equal(
    wide2long(df),
    dplyr::tibble(
      time=c(1,1,1,2,2,2,3,3,3),
      realization=c(1,2,3,1,2,3,1,2,3),
      raw=c(4,5,6,7,8,9,10,11,12)
    )  
  )

  expect_equal(
    wide2long(data.frame(time=1:3, raw=4:6)),
    dplyr::tibble(time=1:3, realization=c(1,1,1), raw=4:6)
  )

  df2 <- dplyr::tibble(
    time=1:4,
    raw=list(c(NA,10,11), c(12,NA,13), c(14,15,NA), c(NA,NA,NA))
  )
  expect_equal(
    wide2long(df2),
    dplyr::tibble(
      time=c(1,1,2,2,3,3),
      realization=c(2,3,1,3,1,2),
      raw=c(10,11,12,13,14,15)
    )
  )
})

test_that("graph_ensemble() works", {
  expect_error(
    graph_ensemble(NULL, create_forecast(data.frame(time=1:3,mean=4:6))),
    "raw data needed to graph ensemble"
  )

  df1 <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

  df2 <- dplyr::tibble(
    time=lubridate::as_date(0:399),
    raw=c(1:400)
  )

  df3 <- data.frame(
    time=lubridate::as_datetime(c(0,20000,100000)),
    raw=c(20,30,40)
  )

  vdiffr::expect_doppelganger(
    "ens1",
    graph_ensemble(NULL, create_forecast(df1))
  )

  vdiffr::expect_doppelganger(
    "ens2",
    graph_ensemble(NULL, create_forecast(df2))
  )

  vdiffr::expect_doppelganger(
    "ens3",
    graph_ensemble(NULL, create_forecast(df3))
  )
})

test_that("graph_observations() works", {
  df1 <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

  # TODO tests with quantiles & accuracy & raw->quantiles->accuracy
  # df2 <- dplyr::tibble(
  #   time=1:3,
  #   quant_25=c(10,20,10),
  # )

  obs <- data.frame(time=1:3, obs=c(5,9,13))
  fc1 <- create_forecast(df1)

  vdiffr::expect_doppelganger("obs1", graph_observations(NULL, obs))
  vdiffr::expect_doppelganger("obs2", graph_observations(NULL, neglog(fc1, obs)))
  vdiffr::expect_doppelganger(
    "obs3",
    graph_observations(graph_ensemble(NULL, fc1), obs)
  )
  vdiffr::expect_doppelganger(
    "obs4",
    graph_observations(graph_ensemble(NULL, fc1), neglog(fc1, obs))
  )

})
