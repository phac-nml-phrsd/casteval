test_that("graph_ensemble() works", {
  expect_error(
    graph_ensemble(NULL, create_forecast(data.frame(time=1:3,val_mean=4:6))),
    "raw data needed to graph ensemble"
  )

  df1 <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

  df2 <- dplyr::tibble(
    time=lubridate::as_date(0:399),
    raw=c(1:400)
  )

  df3 <- data.frame(
    time=lubridate::as_datetime(c(0,20000,100000)),
    raw=c(20,30,40)
  )

  vdiffr::expect_doppelganger("ens1",
    graph_ensemble(NULL, create_forecast(df1))
  )

  vdiffr::expect_doppelganger("ens2",
    graph_ensemble(NULL, create_forecast(df2))
  )

  vdiffr::expect_doppelganger("ens3",
    graph_ensemble(NULL, create_forecast(df3))
  )
})
