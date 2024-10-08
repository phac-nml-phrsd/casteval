test_that("plot_quant_intervals() works", {
  fc1 <- create_forecast(dplyr::tibble(
    time=1:3,
    val_q5=8:6, val_q95=22:20,
    val_q10=10:8, val_q90=20:18,
    val_q25=14:12, val_q75=16:14
  ))

  fc2 <- create_forecast(dplyr::tibble(
    time=rep(1:3, each=5),
    sim=rep(1:5, 3),
    val=c(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))

  vdiffr::expect_doppelganger("interval1",
    plot_quant_intervals(NULL, fc1, quant_intervals=list(c(25,75), c(5,95), c(10,90)))
  )

  expect_error(
    plot_quant_intervals(NULL, fc1, quant_intervals=list(c(2.5, 97.5))),
    "could not compute/obtain 2.5% quantile from data frame"
  )

  vdiffr::expect_doppelganger("interval2",
    plot_quant_intervals(NULL, fc1)
  )

  vdiffr::expect_doppelganger("interval3",
    plot_quant_intervals(NULL, fc1, list(c(25,75), c(10,90)), palette="Greens", alpha=1)
  )

  expect_error(
    plot_quant_intervals(NULL, fc2),
    "could not infer quantile pairs from forecast data"
  )

  vdiffr::expect_doppelganger("interval4",
    plot_quant_intervals(NULL, fc2, list(c(5, 95), c(25,75)))
  )

  vdiffr::expect_doppelganger("interval5",
    NULL |> plot_ensemble(fc2) |> plot_quant_intervals(fc2, list(c(25, 75), c(5, 95)))
  )
})
