test_that("plot_ensemble() works", {
  expect_error(
    plot_ensemble(NULL, create_forecast(data.frame(time=1:3,val_mean=4:6))),
    "raw data needed to plot ensemble"
  )

  fc0 <- create_forecast(data.frame(
    time=c(1,1,1,2,2,2,3,3,3),
    val=4:12
  ))

  fc1 <- create_forecast(list(
    time=1:3,
    vals=list(c(4,7,10), c(5,8,11), c(6,9,12))
  ))


  fc2 <- create_forecast(dplyr::tibble(
    time=lubridate::as_date(0:399),
    sim=rep(1,400),
    val=c(1:400)
  ))

  fc3 <- create_forecast(data.frame(
    time=lubridate::as_datetime(c(0,20000,100000)),
    sim=c(1,1,1),
    val=c(20,30,40)
  ))

  vdiffr::expect_doppelganger("ens0",
    plot_ensemble(NULL, fc0)
  )

  vdiffr::expect_doppelganger("ens1",
    plot_ensemble(NULL, fc1)
  )

  vdiffr::expect_doppelganger("ens2",
    plot_ensemble(NULL, fc2)
  )

  vdiffr::expect_doppelganger("ens3",
    plot_ensemble(NULL, fc3)
  )

  vdiffr::expect_doppelganger("ens4",
    plot_ensemble(NULL, fc3, alpha=0.9, colour="#33ccff")
  )
})
