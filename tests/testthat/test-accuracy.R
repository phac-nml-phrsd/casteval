test_that("validate_interval() works", {
  expect_error(
    validate_interval("a"),
    "must be either NULL or vector of 2 numbers"
  )

  expect_error(
    validate_interval(2),
    "vector must have length 2"
  )

  expect_error(
    validate_interval(1:3),
    "vector must have length 2"
  )

  expect_error(
    validate_interval(list(1,2)),
    "must be either NULL or vector of 2 numbers"
  )

  expect_error(
    validate_interval(c(2,2)),
    "`interval.*1.*` must be less than `interval.*2.*`"
  )
  
  expect_error(
    validate_interval(c(2,1)),
    "`interval.*1.*` must be less than `interval.*2.*`"
  )

  expect_error(
    validate_interval(c(-1, 90)),
    "`interval.*1.*` and `interval.*2.*` must be between 0 and 100, inclusive"
  )

  expect_error(
    validate_interval(c(10, 101)),
    "`interval.*1.*` and `interval.*2.*` must be between 0 and 100, inclusive"
  )

  expect_equal(
    validate_interval(c(0,100)),
    NULL
  )
})

test_that("accuracy() validates", {
  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, raw=4:6)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "`interval` parameter required for computing accuracy from raw data"
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_25=4:6)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "2 or more quantiles required to calculate accuracy"
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_25=4:6, quant_74=7:9)),
        data.frame(time=1:3, raw=4:6),
        NULL
    ),
    "outermost quantiles must be equidistant from 50th percentile"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, raw=4:6),
      c(1, 25)
    ),
    "column named `quant_1` not in data frame"
  )
  
  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, raw=4:6),
      c(25,74.9)
    ),
    "column named `quant_74.9` not in data frame"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3, mean=4:6)),
      data.frame(time=1:3, raw=4:6),
      NULL
    ),
    "`raw` or `quant.*columns required to calculate accuracy"
  )
})

test_that("accuracy() handles NAs", {
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, raw=list(c(1,NA, 3), c(4, NA, NA), as.numeric(c(NA,NA,NA)))
      )),
      data.frame(time=1:3, raw=c(1.5, 3, 100)),
      c(25,75)
    ),
    0.5
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=2:4, raw=c(8, 100, 100)),
      c(0,100)
    ),
    0.5
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=1:3, raw=c(NA,8,10)),
      c(0,50)
    ),
    1
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:4, quant_25=c(10,NA,11,12), quant_75=c(13,14,NA,15)
      )),
      data.frame(time=1:4, raw=c(10, 100, 100, 15.1)),
      c(25,75)
    ),
    0.5
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, quant_25=c(10, NA, NA), quant_75=c(NA, 11, NA)
      )),
      data.frame(time=1:3, raw=c(10, 11, 100)),
      c(25,75)
    ),
    "forecast quantiles contain NA in every row"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(NA_real_,NA_real_,NA_real_))),
      data.frame(time=1:3, raw=4:6),
      c(0,100)
    ),
    "forecast quantiles contain NA in every row"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=4:6, raw=1:3),
      c(0,100)
    ),
    "observations don't overlap with forecast data at all"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,as.numeric(c(NA,NA,NA)),10:12))),
      data.frame(time=2:4, raw=c(5, NA, 10)),
      c(0, 100)
    ),
    "observations don't overlap with forecast data at all"
  )
})


test_that("accuracy() raw works", {
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, raw=c(5, 7.5, 11.5)),
      c(25, 75)
    ),
    1
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, raw=c(5, 7.4, 11.6)),
      c(25, 75)
    ),
    1/3
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, raw=c(0, 7.4, 11.6)),
      c(25, 75)
    ),
    0
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=c(4,5,6))),
      data.frame(time=1:3, raw=c(4,5,7)),
      c(25, 75)
    ),
    2/3
  )
  
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, raw=c(4, 9, 13)),
      c(0,100)
    ),
    2/3
  )
})
