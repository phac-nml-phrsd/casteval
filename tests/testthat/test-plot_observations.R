test_that("plot_observations() works", {
  fc1 <- create_forecast(list(
    time=1:3,
    vals=list(c(4,7,10), c(5,8,11), c(6,9,12))
  ))
  obs <- data.frame(time=1:3, val_obs=c(5,9,13))

  vdiffr::expect_doppelganger("obs1", plot_observations(NULL, obs))
  vdiffr::expect_doppelganger("obs2", plot_observations(NULL, log_score(fc1, obs,summarize=FALSE)))
  vdiffr::expect_doppelganger("obs3",
    plot_observations(plot_ensemble(NULL, fc1), obs)
  )
  vdiffr::expect_doppelganger("obs4",
    plot_observations(plot_ensemble(NULL, fc1), log_score(fc1, obs,summarize=FALSE))
  )

  fc2 <- create_forecast(dplyr::tibble(
    time=1:12,
    val_q25=c(5,4,6,5,7,8,5,4,3,5,5,7),
    val_q75=c(13,14,12,15,12,12,20,17,15,16,13,13)
  ))

  obs2 <- data.frame(time=1:12, val_obs=3:14)

  vdiffr::expect_doppelganger("obs5",
    NULL |> plot_quant_intervals(fc2) |> plot_observations(obs2)
  )
  vdiffr::expect_doppelganger("obs6",
    NULL |> plot_quant_intervals(fc2) |> plot_observations(accuracy(fc2, obs2, summarize=FALSE, quant_pairs=c(25, 75)))
  )

  fc3 <- create_forecast(list(
    time=1:3,
    # TODO add a format for this
    # raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
    vals = list(c(3,6,11), c(5,8,15), c(6,7,13), c(7,8,14), c(3,7,17))
  ))

  obs3 <- data.frame(
    time=1:3,
    val_obs=c(4,10,14)
  )
  vdiffr::expect_doppelganger("obs7",
    NULL |> plot_quant_intervals(fc3, c(25,75)) |> plot_observations(accuracy(fc3, obs3, quant_pairs=c(25,75), summarize=FALSE))
  )

  fc4 <- create_forecast(list(
    time=1:10,
    # raw=list(
    #   c(1,2,4,5,2), c(3,5,4,6,3), c(7,6,5,8,8), c(9,8,7,6,8), c(3,5,8,7,6),
    #   c(9,8,7,10,8), c(10,11,13,12,9), c(15,14,15,20,10), c(9,8,10,15,14), c(6,8,7,7,5)
    # )
    vals=list(
      c(1,3,7,9,3,9,10,15,9,6),
      c(2,5,6,8,3,9,10,15,9,6),
      c(4,4,5,7,8,7,13,15,10,7),
      c(5,6,8,6,7,10,12,20,15,7),
      c(2,3,8,8,6,8,9,10,14,5)
    )
  ))

  obs4 <- data.frame(time=1:10, val_obs=1:10)

  vdiffr::expect_doppelganger("obs8",
    NULL |> plot_ensemble(fc4) |> plot_observations(log_score(fc4, obs4, summarize=FALSE))
  )
})
