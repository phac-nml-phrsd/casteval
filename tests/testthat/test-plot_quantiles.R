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

  NULL |> plot_ensemble(fc) |> plot_quantiles(fc, quants=50)
})