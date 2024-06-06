test_that("widen_NULL() accepts empty list", {
  expect_equal(casteval:::widen_NULL(list(), 1), list())
})

test_that("widen_NULL() widens to 1", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, 1, NULL, 2, NULL, 3), 1),
    list(NA_real_, 1, NA_real_, 2, NA_real_, 3)
  )
})

test_that("widen_NULL() widens to multiple", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, c(1,2,3), NULL), 3),
    list(c(NA_real_, NA_real_, NA_real_), c(1,2,3), c(NA_real_, NA_real_, NA_real_))
  )
})

test_that("widen_NULL() widens to 0", {
  expect_equal(
    casteval:::widen_NULL(list(NULL, 1, c(2,3,4), NULL), 0),
    list(numeric(0), 1, c(2,3,4), numeric(0))
  )
})

test_that("widen_NULL() does not modify non-NULL", {
  expect_equal(
    casteval:::widen_NULL(list(list(), NA, logical(0), numeric(0), NA_real_, 1, c(1,2,3), "hi", 0, FALSE), 2),
    list(list(), NA, logical(0), numeric(0), NA_real_, 1, c(1,2,3), "hi", 0, FALSE)
  )
})

test_that("combine_two_data_frames() demands non-empty data frames", {
  expect_error(
    casteval:::combine_two_data_frames(data.frame(), data.frame(a=1))
  )
  expect_error(
    casteval:::combine_two_data_frames(data.frame(a=1), data.frame())
  )
})

test_that("combine_two_data_frames() works", {
  expect_equal(
    casteval:::combine_two_data_frames(
      data.frame(time=c(1,2,3), raw=c(10, 11, 12)),
      data.frame(time=c(3,2,1), raw=c(20, 21, 22))
    ),

    # we have to do this because inserting list columns with I() introduces attributes
    {
      df <- data.frame(time=c(1,2,3))
      df$raw <- list(c(10, 22), c(11, 21), c(12, 20))
      df
    }
  )

  expect_equal(
    casteval:::combine_two_data_frames(
      data.frame(time=c(1, 2, 3), raw=c(10, 11, 12)),
      data.frame(time=c(2, 3, 4), raw=c(20, 21, 22))
    ),
    {
      df <- data.frame(time=c(1,2,3,4))
      df$raw <- list(c(10, NA), c(11, 20), c(12, 21), c(NA, 22))
      df
    }
  )
})

