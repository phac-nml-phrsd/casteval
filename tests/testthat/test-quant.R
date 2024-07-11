test_that("get_quant_percentages() works", {
  expect_equal(
    get_quant_percentages(data.frame(quant_0=0, quant_2.5=1, quant_50=2, quant_97.5=3, quant_100=4)),
    c(0, 2.5, 50, 97.5, 100)
  )
})

test_that("get_quant_col() works", {
  expect_equal(
    get_quant_col(data.frame(quant_2.5=1:10, quant_50=11:20), 2.5),
    1:10
  )

  expect_equal(
    get_quant_col(data.frame(quant_2.5=1:10, quant_50=11:20), 50),
    11:20
  )

  expect_equal(
    get_quant_col(dplyr::tibble(quant_2.5=as.list(1:10), quant_50=as.list(11:20)), 50),
    11:20
  )
})

test_that("quant_name() works", {
  expect_equal(quant_name(50), "quant_50")
  expect_equal(quant_name(0), "quant_0")
  expect_equal(quant_name(100), "quant_100")
  expect_equal(quant_name(2.5), "quant_2.5")
  expect_equal(quant_name(1.2345), "quant_1.2345")
})

test_that("raw2quant() works", {
  expect_error(raw2quant(list(c(1,2,NA),4:6)), "raw data contains NA values")
  expect_error(raw2quant(c(1,NA,3)), "raw data contains NA values")
  expect_error(raw2quant(list(1, NA, 3), "raw data contains NA values"))
  expect_equal(raw2quant(list(), 50), numeric(0))
  expect_equal(raw2quant(list(1:3, 2, 3:6, 7:11), 50), c(2, 2, 4.5, 9))
  expect_equal(raw2quant(list(1:3, 2, 3:6, 7:11), 0), c(1, 2, 3, 7))
  expect_equal(raw2quant(list(1:3, 2, 3:6, 7:11), 100), c(3, 2, 6, 11))
  expect_equal(raw2quant(list(0:100), 26), 26)
})

test_that("get_quantile() works", {
  # error if not found
  expect_error(
    get_quantile(data.frame(time=1:3, quant_51=6:8), 50),
    "could not compute/obtain.*quantile from data frame"
  )

  df <- dplyr::tibble(time=1:3, quant_50=c(1000,2000,3000), raw=list(4:6, 7:9, 10:12))

  # raw supersedes quant_*
  expect_equal(
    get_quantile(df, 50),
    c(5, 8, 11)
  )

  expect_equal(
    get_quantile(df |> dplyr::select(time, quant_50), 50),
    c(1000,2000,3000)
  )

  expect_equal(
    get_quantile(dplyr::tibble(time=1:2, raw=list(c(1,2,NA), 4:6)), 50),
    c(1.5, 5)
  )

  expect_error(
    get_quantile(dplyr::tibble(time=1:2, raw=c(NA,2)), 50),
    "forecast data frame contains row with no raw data"
  )
})