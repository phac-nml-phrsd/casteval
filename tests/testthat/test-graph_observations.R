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
    NULL |> graph_quantiles(fc2) |> graph_observations(accuracy(fc2, obs2, summarize=FALSE, interval=c(25, 75)))
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