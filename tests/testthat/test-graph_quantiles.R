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