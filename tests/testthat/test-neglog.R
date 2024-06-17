# # expected values were generated using scoringRules::logs_sample()
# test_that("neglog_point() works", {
#   expect_equal(
#     neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 10),
#     3.536169,
#     tolerance=0.01
#   )

#   expect_equal(
#     neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 0),
#     2.148256,
#     tolerance=0.01
#   )

#   expect_equal(
#     neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 17),
#     Inf
#   )
# })

test_that("neglog() validates", {
  expect_error(
    neglog(
      create_forecast(dplyr::tibble(time=1:3, mean=4:6)),
      data.frame(time=1:3, raw=4:6)
    ),
    "data frame does not contain `raw` column"
  )

  expect_error(
    neglog(
      create_forecast(dplyr::tibble(time=1:2, raw=list(c(NA,2,3), c(NA,NA,4)))),
      data.frame(time=1:2, raw=3:4)
    ),
    "at least 2 raw data points.*required to calculate KDE"
  )
})

test_that("neglog() works", {
  expect_equal(
    neglog(
      create_forecast(dplyr::tibble(time=1, raw=list(1:5))),
      data.frame(time=1, raw=4.5)
    ),
    1.812532
  )
})
