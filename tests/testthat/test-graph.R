test_that("legends work", {
  fc <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
  ))
  vdiffr::expect_doppelganger("legend1",
    NULL |> graph_quantiles(fc, c(25,75)) |> graph_confidence_intervals(fc, c(50,90))
  )
})