# test_that("get_confidence_intervals()", {
#   fc1 <- create_forecast(dplyr::tibble(
#     time=1:3,
#     quant_5=8:6, quant_95=22:20,
#     quant_10=10:8, quant_90=20:18,
#     quant_25=14:12, quant_75=16:14
#   ))
  
#   fc2 <- create_forecast(dplyr::tibble(
#     time=1:3,
#     raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
#   ))

#   vdiffr::expect_doppelganger("conf1",
#     graph_confidence_intervals(NULL, fc1, c(90,50,80))
#   )

#   expect_error(
#     graph_confidence_intervals(NULL, fc1, c(90,50,80,2)),
#     "could not compute/obtain.*quantile from data frame"
#   )
  
#   vdiffr::expect_doppelganger("conf2",
#     graph_confidence_intervals(NULL, fc1, c(50,80))
#   )

#   vdiffr::expect_doppelganger("conf3",
#     graph_confidence_intervals(NULL, fc1)
#   )

#   expect_error(
#     graph_confidence_intervals(NULL, fc2),
#     "no confidence intervals specified and none inferrable from data frame"
#   )

#   vdiffr::expect_doppelganger("conf4",
#     graph_confidence_intervals(NULL, fc2, c(90,50))
#   )

#   vdiffr::expect_doppelganger("conf5",
#     NULL |> graph_ensemble(fc2) |> graph_confidence_intervals(fc2, c(90,50))
#   )
# })