test_that("plot_quantiles() works", {
  fc <- create_forecast(list(
    time=1:10,
    vals=list(
      c(1,2,3,5,4,5,4,6,6,5),
      c(1,3,5,4,6,5,7,9,8,8),
      c(1,4,3,4,5,6,5,3,2,2),
      c(1,2,4,5,7,8,7,9,10,9)
    )
  ))

  vdiffr::expect_doppelganger("quant1",
    NULL |> plot_ensemble(fc) |> plot_quantiles(fc, quants=50)
  )

  vdiffr::expect_doppelganger("quant2",
    NULL |> plot_ensemble(fc) |> plot_quantiles(fc, quants=c(2.5, 5, 10, 25, 50, 90, 95, 97.5))
  )

  vdiffr::expect_doppelganger("quant3",
    NULL |> plot_ensemble(fc) |> plot_quantiles(fc, quants=c(2.5, 50, 97.5), colour="green", alpha=0.5)
  )

  expect_error(
    NULL |> plot_quantiles(fc),
    "no quantile columns in forecast data and `quants` not provided"
  )

  fc2 <- create_forecast(data.frame(
    time=1:10,
    val_q2.5=1:10,
    val_q25=2:11,
    val_q50=3:12,
    val_q75=4:13,
    val_q97.5=5:14
  ))

  vdiffr::expect_doppelganger("quant4",
    NULL |> plot_quantiles(fc2)
  )

  vdiffr::expect_doppelganger("quant5",
    NULL |> plot_quantiles(fc2, quants=c(2.5,50,97.5))
  )

  expect_error(
    NULL |> plot_quantiles(fc2, quants=c(50,50)),
    "`quants` contains duplicate quantiles"
  )
})