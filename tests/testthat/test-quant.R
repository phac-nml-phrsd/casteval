test_that("get_quant_percentages() works", {
  expect_equal(
    get_quant_percentages(data.frame(val_q0=0, val_q2.5=1, val_q50=2, val_q97.5=3, val_q100=4)),
    c(0, 2.5, 50, 97.5, 100)
  )
})

test_that("get_quant_col() works", {
  expect_equal(
    get_quant_col(data.frame(val_q2.5=1:10, val_q50=11:20), 2.5),
    1:10
  )

  expect_equal(
    get_quant_col(data.frame(val_q2.5=1:10, val_q50=11:20), 50),
    11:20
  )

  expect_equal(
    get_quant_col(dplyr::tibble(val_q2.5=as.list(1:10), val_q50=as.list(11:20)), 50),
    11:20
  )
})

test_that("quant_name() works", {
  expect_equal(quant_name(50), "val_q50")
  expect_equal(quant_name(0), "val_q0")
  expect_equal(quant_name(100), "val_q100")
  expect_equal(quant_name(2.5), "val_q2.5")
  expect_equal(quant_name(1.2345), "val_q1.2345")
})

test_that("get_quantile() works", {
  # error if not found
  expect_error(
    get_quantile(data.frame(time=1:3, val_q51=6:8), 50),
    "could not compute/obtain.*quantile from data frame"
  )

  df <- dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=c(4:12))

  df2 <- dplyr::tibble(time=1:3, val_q50=c(1000,2000,3000))

  expect_equal(
    get_quantile(df, 50),
    dplyr::tibble(time=1:3, quant=c(5, 8, 11))
  )

  expect_equal(
    get_quantile(df2, 50),
    dplyr::tibble(time=1:3, quant=c(1000,2000,3000))
  )

  expect_equal(
    get_quantile(dplyr::tibble(time=c(1,1,2,2,2), val=c(1,2, 4:6)), 50),
    dplyr::tibble(time=1:2, quant=c(1.5, 5))
  )

  expect_error(
    get_quantile(dplyr::tibble(time=1:2, val=c(NA,2)), 50),
    "missing values and NaN's not allowed if 'na.rm' is FALSE"
  )
})

test_that("pair_quantiles() works", {
  expect_equal(
    pair_quantiles(numeric(0)),
    list(paired=list(), unpaired=numeric(0))
  )

  expect_equal(
    pair_quantiles(c(50)),
    list(paired=list(), unpaired=c(50))
  )

  expect_equal(
    pair_quantiles(c(20, 30, 10)),
    list(paired=list(), unpaired=c(10, 20, 30))
  )

  expect_equal(
    pair_quantiles(c(0, 10, 15, 20, 30, 50, 51, 60, 70, 75, 85)),
    list(paired=list(c(15,85), c(30,70)), unpaired=c(0,10,20,50,51,60,75))
  )
})