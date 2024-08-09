test_that("plot_mean() works", {
  expect_error(
    plot_mean(NULL, create_forecast(data.frame(time=1,val_q50=2))),
    "mean or raw data required to plot mean"
  )

  fc <- create_forecast(
    data.frame(time=1:5, val_mean=6:10)
  )

  vdiffr::expect_doppelganger("mean1",
    NULL |> plot_mean(fc, colour="red", alpha=0.5)
  )

  fc2 <- create_forecast(
    data.frame(time=rep(1:5, each=3), val=c(1,2,3, 4,7,5, 10,11,10, 0,0,0, 1,2,2))
  )

  vdiffr::expect_doppelganger("mean2",
    NULL |> plot_mean(fc2)
  )
})