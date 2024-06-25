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

  vdiffr::expect_doppelganger("ens1",
    graph_ensemble(NULL, create_forecast(df1))
  )

  vdiffr::expect_doppelganger("ens2",
    graph_ensemble(NULL, create_forecast(df2))
  )

  vdiffr::expect_doppelganger("ens3",
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
  vdiffr::expect_doppelganger("obs3",
    graph_observations(graph_ensemble(NULL, fc1), obs)
  )
  vdiffr::expect_doppelganger("obs4",
    graph_observations(graph_ensemble(NULL, fc1), neglog(fc1, obs))
  )

})

test_that("graph_quantiles() works", {
  fc1 <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(0:4, 5:9, 10:14)
  ))

  fc2 <- create_forecast(dplyr::tibble(
    time=1:3,
    quant_0=c(4:6),
    quant_2.5=c(7:9),
    quant_50=c(10:12)
  ))

  vdiffr::expect_doppelganger("quant1",
    graph_quantiles(NULL, fc1, quants=c(0,50,100))
  )

  vdiffr::expect_doppelganger("quant2",
    graph_quantiles(graph_ensemble(NULL, fc1), fc1, c(2.5, 25,51,75))
  )

  vdiffr::expect_doppelganger("quant3",
    graph_quantiles(NULL, fc2, c(0, 2.5))
  )

  expect_error(
    graph_quantiles(NULL, fc2, c(2.5,50,75)),
    "could not compute/obtain.*quantile from data frame"
  )

  expect_error(
    graph_quantiles(NULL, fc1),
    "no quantiles specified and none found in data frame"
  )

  vdiffr::expect_doppelganger("quant4",
    graph_quantiles(NULL, fc2)
  )
})


test_that("quants2confs() works", {
  expect_equal(quants2confs(numeric(0)), numeric(0))
  expect_error(quants2confs(c(25,50,75)), "even number of quantiles needed")
  expect_error(quants2confs(c(25,75,100), "quantiles must be symmetric around median"))
  expect_error(quants2confs(c(5,25), "quantiles must be symmetric around median"))
  expect_equal(quants2confs(c(0,2.5,5,10,25,75,90,95,97.5,100)), c(50,80,90,95,100))
  expect_equal(quants2confs(c(25,10,90,75)), c(50,80))
})

test_that("get_confidence_intervals()", {
  fc1 <- create_forecast(dplyr::tibble(
    time=1:3,
    quant_5=8:6, quant_95=22:20,
    quant_10=10:8, quant_90=20:18,
    quant_25=14:12, quant_75=16:14
  ))
  
  fc2 <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))

  vdiffr::expect_doppelganger("conf1",
    graph_confidence_intervals(NULL, fc1, c(90,50,80))
  )

  expect_error(
    graph_confidence_intervals(NULL, fc1, c(90,50,80,2)),
    "could not compute/obtain.*quantile from data frame"
  )
  
  vdiffr::expect_doppelganger("conf2",
    graph_confidence_intervals(NULL, fc1, c(50,80))
  )

  vdiffr::expect_doppelganger("conf3",
    graph_confidence_intervals(NULL, fc1)
  )

  expect_error(
    graph_confidence_intervals(NULL, fc2),
    "no confidence intervals specified and none inferrable from data frame"
  )

  vdiffr::expect_doppelganger("conf4",
    graph_confidence_intervals(NULL, fc2, c(90,50))
  )

  vdiffr::expect_doppelganger("conf5",
    NULL |> graph_ensemble(fc2) |> graph_confidence_intervals(fc2, c(90,50))
  )
})