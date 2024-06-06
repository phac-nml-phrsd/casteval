## widen_NULL() tests

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

## combine_two_data_frames() tests

test_that("combine_two_data_frames() demands non-empty data frames", {
  expect_error(
    casteval:::combine_two_data_frames(data.frame(), data.frame(a=1)),
    "nrow.*df1"
  )
  expect_error(
    casteval:::combine_two_data_frames(data.frame(a=1), data.frame()),
    "nrow.*df2"
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

# this is unlikely to occur but for robustness it should still work
test_that("combine_two_data_frames() handles empty values", {
  expect_equal(
    casteval:::combine_two_data_frames(
      dplyr::tibble(time=c(1,2), raw=c(1,2)),
      dplyr::tibble(time=c(1,2), raw=list(NULL, numeric(0)))
    ),
    dplyr::tibble(time=c(1,2), raw=list(1,2))
  )
  expect_equal(
    casteval:::combine_two_data_frames(
      dplyr::tibble(time=c(1,2), raw=list(NULL, numeric(0))),
      dplyr::tibble(time=c(1,2), raw=c(1,2))
    ),
    dplyr::tibble(time=c(1,2), raw=list(1,2))
  )
  expect_equal(
    casteval:::combine_two_data_frames(
      dplyr::tibble(time=c(1,2), raw=list(NULL, numeric(0))),
      dplyr::tibble(time=c(1,2), raw=list(numeric(0),NULL))
    ),
    dplyr::tibble(time=c(1,2), raw=list(numeric(0),numeric(0)))
  )
})

## combine_data_frames() tests

test_that("combine_data_frames() demands > 0 data frames", {
  expect_error(combine_data_frames(list()), "dfs.*length.*0")
})

test_that("combine_data_frames() accepts 1 data frame", {
  expect_equal(
    casteval:::combine_data_frames(list(data.frame(time=c(1,2,3), raw=c(4,5,6)))),
    data.frame(time=c(1,2,3), raw=c(4,5,6))
  )
})

test_that("combine_data_frames() works", {
  expect_equal(
    casteval:::combine_data_frames(list(
      data.frame(time=c(1,2,3), raw=c(10,11,12)),
      data.frame(time=c(2,3,4), raw=c(20,21,22)),
      data.frame(time=c(3,4,5), raw=c(30,31,32))
    )),
    {
      df <- data.frame(time=1:5)
      df$raw <- list(c(10,NA,NA), c(11,20,NA), c(12,21,30), c(NA,22,31), c(NA,NA,32))
      df
    }
  )
})

test_that("combine_data_frames() discards extraneous columns", {
  expect_equal(
    casteval:::combine_data_frames(list(
      data.frame(time=c(1,2,3), raw=c(10,11,12), whatever=c(100,200,300)),
      data.frame(time=c(2,3,4), raw=c(20,21,22), whatever=c(-1,-2,-3)),
      data.frame(time=c(3,4,5), raw=c(30,31,32), something=c("a","b","c"))
    )),
    {
      df <- data.frame(time=1:5)
      df$raw <- list(c(10,NA,NA), c(11,20,NA), c(12,21,30), c(NA,22,31), c(NA,NA,32))
      df
    }
  )
})

## combine_ensemble() tests

test_that("combine_ensemble() validates input", {
  # demands > 0 arguments
  expect_error(combine_ensemble(), "no arguments")

  # rejects non-lists
  expect_error(combine_ensemble(list(), 1), "non-lists")

  # rejects data frames not inside lists
  expect_error(combine_ensemble(list(), data.frame()), "data frame")
})

# test_that("combine_ensemble() accepts 1 data frame", {
#   expect_equal(
#     combine_ensemble(data=list(data.frame(time=c(1,2,3), raw=c(4,5,6)))),
#     data.frame(time=c(1,2,3), raw=c(4,5,6))
#   )
# })
