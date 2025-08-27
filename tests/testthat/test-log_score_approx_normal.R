test_that("log_score_approx_normal() works", {

  fcst <- create_forecast(
    dplyr::tibble(
      time    = 1:3,
      val_q5  = seq(1,8, length.out = 3),
      val_q10 = 9:11,
      val_q25 = 12:14,
      val_q50 = 100:102,
      val_q75 = 200:202,
      val_q90 = 203:205,
      val_q95 = seq(210,220, length.out = 3)
    ))

  obs <- data.frame(time=1:3, val_obs=c(105,101,300))

  s1 = log_score_approx_normal(fcst, obs,
                              summarize = FALSE,
                              quantpair.choice = 1)
  # print(s1)
  expect_equal(s1$score[1], -5.070, tolerance = 0.01)
  expect_equal(s1$implied_normal_sd[1], 63.53, tolerance = 0.01)
  expect_equal(s1$implied_normal_mean[1], 105.5, tolerance = 0.01)


  s2 = log_score_approx_normal(fcst, obs,
                              summarize = TRUE,
                              quantpair.choice = 2)
  # print(s2)
  expect_equal(s2, -6.319, tolerance = 0.01)

  expect_error(
    log_score_approx_normal(fcst, obs,
                            summarize = TRUE,
                            quantpair.choice = 6)
  )

})
