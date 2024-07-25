test_that("legends work", {
  fc <- create_forecast(list(
    time=1:3,
    #raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
    vals=list(c(3,6,11), c(5,8,15), c(6,7,13), c(7,8,14), c(3,7,17))
  ))
  obs <- data.frame(time=1:3, val_obs=4:6)

  acc <- \(...) accuracy(..., quant_pairs=c(5,95))
  vdiffr::expect_doppelganger("legend1",
    NULL |> graph_ensemble(fc) |> graph_observations(acc(fc,obs,summarize=FALSE)) |> graph_quant_intervals(fc, c(5,95))
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
      create_forecast(data.frame(time=1,val_mean=1)),
      score=accuracy
    ),
    "scoring function provided without observations"
  )

  # expect_error(
  #   graph_forecast(
  #     create_forecast(data.frame(time=c(1,2),val_q25=c(1,1),val_q75=c(2,2)))
  #   ),
  #   "nothing was graphed. Please specify raw data, confidence intervals, and/or observations to be graphed."
  # )
  expect_error(
    graph_forecast(
      create_forecast(data.frame(time=1:3, val_mean=4:6))
    ),
    "nothing was graphed. Please specify raw data, quantiles, and/or observations to be graphed."
  )

  fc1 <- create_forecast(list(
    time=1:3,
    # raw=list(c(4,5,4), c(7,6,6), c(8,7,6))
    vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
  ))

  obs1 <- data.frame(time=1:3, val_obs=5:7)

  vdiffr::expect_doppelganger("graph1", graph_forecast(fc1))
  vdiffr::expect_doppelganger("graph2", graph_forecast(fc1, obs1))
  vdiffr::expect_doppelganger("graph3", graph_forecast(fc1, obs1, score=make_accuracy(c(5,95))))
  vdiffr::expect_doppelganger("graph4", graph_forecast(fc1, obs1, score=make_accuracy(c(5,95)), quant_pairs=list(c(5,95),c(25,75))))
  vdiffr::expect_doppelganger("graph5", graph_forecast(fc1, obs1, score=neglog))

  fc2 <- create_forecast(data.frame(
    time=1:3,
    val_q5=4:6,
    val_q25=5:7,
    val_q75=6:8,
    val_q95=7:9
  ))

  obs2 <- data.frame(time=1:3, val_obs=c(4,6,8))

  vdiffr::expect_doppelganger("graph6", graph_forecast(fc2, quant_pairs=c(25,75)))
  expect_error(graph_forecast(fc2, quant_pairs=c(15,85)), "could not compute/obtain 15% quantile from data frame")
  vdiffr::expect_doppelganger("graph7", graph_forecast(fc2, obs2, quant_pairs=c(25,75), score=make_accuracy(c(25,75))))
  vdiffr::expect_doppelganger("graph8", graph_forecast(fc2, obs2, quant_pairs=c(5,95), score=make_accuracy(c(5,95))))
  expect_error(graph_forecast(fc2, obs2, score=neglog), "neglog\\(\\) requires raw forecast data")
})

test_that("forecast_time vline works", {
  fc1 <- create_forecast(
    list(
      time=1:3,
      vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
    ),
    forecast_time=2
  )

  obs1 <- data.frame(time=1:3, val_obs=5:7)

  fc2 <- create_forecast(data.frame(
    time=1:3,
    val_q5=4:6,
    val_q25=5:7,
    val_q75=6:8,
    val_q95=7:9
  ), forecast_time=2)

  obs2 <- data.frame(time=1:3, val_obs=c(4,6,8))

  vdiffr::expect_doppelganger("vline1", graph_forecast(fc1, obs1, quant_pairs=list(c(25,75), c(2.5,97.5)), score=neglog))

  vdiffr::expect_doppelganger("vline2", graph_forecast(fc2, obs2, quant_pairs=c(25,75), score=make_accuracy(c(5,95))))
})

test_that("labels work", {
  fc1 <- create_forecast(
    list(
      time=1:3,
      vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
    ),
    name="a forecast"
  )

  obs1 <- data.frame(time=1:3, val_obs=5:7)

  fc2 <- create_forecast(
    data.frame(
      time=1:3,
      val_q5=4:6,
      val_q25=5:7,
      val_q75=6:8,
      val_q95=7:9
    ),
    name=""
  )

  obs2 <- data.frame(time=1:3, val_obs=c(4,6,8))

  vdiffr::expect_doppelganger("lab1", graph_forecast(fc1, obs1))
  vdiffr::expect_doppelganger("lab2", graph_forecast(fc2, obs2))
})
