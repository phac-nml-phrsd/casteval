test_that("wide2long() works", {
  df <- dplyr::tibble(
    time=1:3,
    raw=list(4:6, 7:9, 10:12)
  )

  expect_equal(
    wide2long(df),
    dplyr::tibble(
      time=c(1,1,1,2,2,2,3,3,3),
      realization=c(1,2,3,1,2,3,1,2,3),
      raw=c(4,5,6,7,8,9,10,11,12)
    )  
  )

  expect_equal(
    wide2long(data.frame(time=1:3, raw=4:6)),
    dplyr::tibble(time=1:3, realization=c(1,1,1), raw=4:6)
  )

  df2 <- dplyr::tibble(
    time=1:4,
    raw=list(c(NA,10,11), c(12,NA,13), c(14,15,NA), c(NA,NA,NA))
  )
  expect_equal(
    wide2long(df2),
    dplyr::tibble(
      time=c(1,1,2,2,3,3),
      realization=c(2,3,1,3,1,2),
      raw=c(10,11,12,13,14,15)
    )
  )
})

test_that("graph_ensemble() works", {
  expect_error(
    graph_ensemble(NULL, create_forecast(data.frame(time=1:3,mean=4:6))),
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

  vdiffr::expect_doppelganger(
    "ens1",
    graph_ensemble(NULL, create_forecast(df1))
  )

  vdiffr::expect_doppelganger(
    "ens2",
    graph_ensemble(NULL, create_forecast(df2))
  )
})
