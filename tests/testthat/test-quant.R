test_that("get_quantiles() works", {
  expect_equal(
    get_quantiles(data.frame(quant_0=0, quant_2.5=1, quant_50=2, quant_97.5=3, quant_100=4)),
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

