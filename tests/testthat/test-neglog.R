# expected values were generated using scoringRules::logs_sample()
test_that("neglog_point() works", {
  expect_equal(
    neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 10),
    3.536169,
    tolerance=0.01
  )

  expect_equal(
    neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 0),
    2.148256,
    tolerance=0.01
  )

  expect_equal(
    neglog_point(c(-1, 0, 1, 10, 5, -4, -9, 2, 0.5), 17),
    Inf
  )
})
