test_that("score() works", {
  expect_error(
    score(5, 0, accuracy),
    "`fcsts` must be a single forecast object or list of forecast objects"
  )

  expect_error(
    score(list(create_forecast(data.frame(time=1, val=4)), 4), 0, accuracy),
    "forecast number 2 is not valid"
  )

  fc1 <- create_forecast(data.frame(time=1:5, val=6:10))
  fc2 <- create_forecast(data.frame(time=1:5, val=7:11))

  obs <- data.frame(time=1:5, val_obs=c(8,8,8,8,8))

  expect_equal(score(fc1, obs, bias), 0)
  expect_equal(score(fc2, obs, bias), 0.4)
  expect_equal(score(list(fc1, fc2), obs, bias), list(0,0.4))
  expect_equal(
    score(list(fc1, fc2), obs, bias, summarize=FALSE),
    list(
      dplyr::tibble(time=1:5, val_obs=c(8,8,8,8,8), score=c(-1,-1,0,1,1)),
      dplyr::tibble(time=1:5, val_obs=c(8,8,8,8,8), score=c(-1, 0, 1, 1, 1))
    )
  )
})