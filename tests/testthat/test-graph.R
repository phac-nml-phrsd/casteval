test_that("graph_observations() works", {
  df1 <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

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

  # TODO tests with quantiles & accuracy & raw->quantiles->accuracy
  fc2 <- create_forecast(dplyr::tibble(
    time=1:12,
    quant_25=c(5,4,6,5,7,8,5,4,3,5,5,7),
    quant_75=c(13,14,12,15,12,12,20,17,15,16,13,13)
  ))

  obs2 <- data.frame(time=1:12, obs=3:14)

  vdiffr::expect_doppelganger("obs5",
    NULL |> graph_quantiles(fc2) |> graph_observations(obs2)
  )
  vdiffr::expect_doppelganger("obs6",
    NULL |> graph_quantiles(fc2) |> graph_observations(accuracy(fc2, obs2, summarize=FALSE))
  )

  fc3 <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))

  obs3 <- data.frame(
    time=1:3,
    obs=c(4,10,14)
  )
  vdiffr::expect_doppelganger("obs7",
    NULL |> graph_confidence_intervals(fc3, c(50)) |> graph_observations(accuracy(fc3, obs3, interval=c(25,75), summarize=FALSE))
  )

  fc4 <- create_forecast(dplyr::tibble(
    time=1:10,
    raw=list(
      c(1,2,4,5,2), c(3,5,4,6,3), c(7,6,5,8,8), c(9,8,7,6,8), c(3,5,8,7,6),
      c(9,8,7,10,8), c(10,11,13,12,9), c(15,14,15,20,10), c(9,8,10,15,14), c(6,8,7,7,5)
    )
  ))

  obs4 <- data.frame(time=1:10, obs=1:10)

  vdiffr::expect_doppelganger("obs8",
    NULL |> graph_ensemble(fc4) |> graph_observations(neglog(fc4, obs4, summarize=FALSE))
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

test_that("legends work", {
  fc <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))
  vdiffr::expect_doppelganger("legend1",
    NULL |> graph_quantiles(fc, c(25,75)) |> graph_confidence_intervals(fc, c(50,90))
  )
})