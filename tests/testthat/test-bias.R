test_that("bias() works", {
  expect_error(
    bias(create_forecast(data.frame(time=1:3, quant_25=4:6)), data.frame(time=1:3, obs=4:6)),
    "raw data, mean, or median required to compute bias"
  )

  fc1 <- create_forecast(dplyr::tibble(
    time=1:3,
    raw=list(c(10,NA), c(NA,10), c(10,10))
  ))
  obs <- data.frame(time=1:3, obs=c(11, 9, 9))
  expect_equal(
    bias(fc1, obs),
    0.5
  )

  fc2 <- create_forecast(dplyr::tibble(time=1:3, raw=c(5,5,5)))
  expect_equal(bias(fc2, obs), -1)

  fc3 <- create_forecast(data.frame(time=1:3, raw=c(12,12,12)))
  expect_equal(bias(fc3, obs), 1)

  fc4 <- create_forecast(data.frame(time=1:3, raw=c(5,5,15)))
  expect_equal(bias(fc4, obs), -1/3)

  fc5 <- create_forecast(data.frame(time=1:3, mean=c(12,12,9), quant_50=c(0,0,0)))
  expect_equal(bias(fc5, obs), 2/3)

  fc6 <- create_forecast(data.frame(time=1:3, quant_50=c(9,9,12)))
  expect_equal(bias(fc6, obs), 0)
})