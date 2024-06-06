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

test_that("combine_two_data_frames() works with all time types", {
  # numeric
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

  # dates
  expect_equal(
    casteval:::combine_two_data_frames(
      data.frame(time=c(lubridate::ymd("2024-01-01"),lubridate::ymd("2024-01-02"),lubridate::ymd("2024-01-03")), raw=c(10, 11, 12)),
      data.frame(time=c(lubridate::ymd("2024-01-03"),lubridate::ymd("2024-01-02"),lubridate::ymd("2024-01-01")), raw=c(20, 21, 22))
    ),
    {
      df <- data.frame(time=c(lubridate::ymd("2024-01-01"),lubridate::ymd("2024-01-02"),lubridate::ymd("2024-01-03")))
      df$raw <- list(c(10, 22), c(11, 21), c(12, 20))
      df
    }
  )

  # date-times
  expect_equal(
    casteval:::combine_two_data_frames(
      data.frame(time=c(lubridate::ymd_hms("2024-01-01_01-01-01"),lubridate::ymd_hms("2024-01-02_01-01-01"),lubridate::ymd_hms("2024-01-03_01-01-01")), raw=c(10, 11, 12)),
      data.frame(time=c(lubridate::ymd_hms("2024-01-03_01-01-01"),lubridate::ymd_hms("2024-01-02_01-01-01"),lubridate::ymd_hms("2024-01-01_01-01-01")), raw=c(20, 21, 22))
    ),
    {
      df <- data.frame(time=c(lubridate::ymd_hms("2024-01-01_01-01-01"),lubridate::ymd_hms("2024-01-02_01-01-01"),lubridate::ymd_hms("2024-01-03_01-01-01")))
      df$raw <- list(c(10, 22), c(11, 21), c(12, 20))
      df
    }
  )
})

test_that("combine_two_data_frames() handles NAs correctly", {
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

test_that("combine_two_data_frames() works with list columns", {
  # df1 list column
  expect_equal(
    casteval:::combine_two_data_frames(
      # we use tibbles here for convenience
      dplyr::tibble(time=c(1,2,3), raw=list(c(10,11), c(12,13), c(14,15))),
      dplyr::tibble(time=c(2,3,4), raw=c(20,21,22))
    ),
    dplyr::tibble(time=c(1,2,3,4), raw=list(c(10,11,NA), c(12,13,20), c(14,15,21), c(NA,NA,22)))
  )

  # df2 list column
  expect_equal(
    casteval:::combine_two_data_frames(
      dplyr::tibble(time=c(2,3,4), raw=c(20,21,22)),
      dplyr::tibble(time=c(1,2,3), raw=list(c(10,11), c(12,13), c(14,15)))
    ),
    dplyr::tibble(time=c(2,3,4,1), raw=list(c(20,12,13), c(21,14,15), c(22,NA,NA), c(NA,10,11)))
  )

  # both list column
  expect_equal(
    casteval:::combine_two_data_frames(
      dplyr::tibble(time=c(1,2,3), raw=list(10, 11, 12)),
      dplyr::tibble(time=c(2,3,4), raw=list(20, 21, 22))
    ),
    dplyr::tibble(time=c(1,2,3,4), raw=list(c(10,NA), c(11,20), c(12,21), c(NA,22)))
  )
})


