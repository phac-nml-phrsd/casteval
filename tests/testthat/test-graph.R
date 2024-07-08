test_that("legends work", {
  fc <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))
  vdiffr::expect_doppelganger("legend1",
    NULL |> graph_quantiles(fc, c(25,75)) |> graph_confidence_intervals(fc, c(50,90))
  )
})

# test_that("graph_forecasts() works", {
#   fc1 <- create_forecast(dplyr::tibble(
#     time=1:3,
#     raw=list(c(4,5,4), c(7,6,6), c(8,7,6))
#   ))

#   fc2 <- create_forecast(dplyr::tibble(
#     time=2:4,
#     raw=list(c(17,18,19), c(19,17,18), c(19,20,22))
#   ))

# })

test_that("graph_forecast() works", {
  expect_error(
    graph_forecast(
      create_forecast(data.frame(time=1,mean=1)),
      score=accuracy
    ),
    "scoring function provided without observations"
  )

  expect_error(
    graph_forecast(
      create_forecast(data.frame(time=1,quant_25=1,quant_75=2))
    ),
    "nothing was graphed"
  )

  fc1 <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(4,5,4), c(7,6,6), c(8,7,6))
  ))

  obs1 <- data.frame(time=1:3, obs=5:7)

  vdiffr::expect_doppelganger("graph1", graph_forecast(fc1))
  vdiffr::expect_doppelganger("graph2", graph_forecast(fc1, obs1))
  vdiffr::expect_doppelganger("graph3", graph_forecast(fc1, obs1, score=accuracy))
  vdiffr::expect_doppelganger("graph4", graph_forecast(fc1, obs1, confs=c(50,95)))
  vdiffr::expect_doppelganger("graph5", graph_forecast(fc1, obs1, score=neglog))

  fc2 <- create_forecast(data.frame(
    time=1:3,
    quant_5=4:6,
    quant_25=5:7,
    quant_75=6:8,
    quant_95=7:9
  ))

  obs2 <- data.frame(time=1:3, obs=c(4,6,8))

  vdiffr::expect_doppelganger("graph6", graph_forecast(fc2, confs=50))
  expect_error(graph_forecast(fc2, confs=70), "could not compute.*obtain.*quantile from data frame")
  vdiffr::expect_doppelganger("graph7", graph_forecast(fc2, obs2, confs=50, score=\(...)accuracy(..., interval=c(25,75))))
  vdiffr::expect_doppelganger("graph8", graph_forecast(fc2, obs2, confs=90, score=\(...)accuracy(...,interval=c(5,95))))
  expect_error(graph_forecast(fc2, obs2, score=neglog), "forecast data frame does not contain `raw` column")
})