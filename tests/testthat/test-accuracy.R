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
      data.frame(time=1:3, obs=4:6),
      NULL
    ),
    "`interval` must be numeric vector$"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3, raw=4:6)),
      data.frame(time=1:3, obs=4:6),
      50
    ),
    "`interval` must be numeric vector with length 2"
  )

  expect_equal(
    accuracy(
        create_forecast(data.frame(time=1:3, raw=4:6)),
        data.frame(time=1:3, obs=4:6),
    ),
    1
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_25=4:6)),
        data.frame(time=1:3, obs=4:6),
    ),
    "could not compute/obtain 2.5% quantile from data frame"
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, quant_2.5=4:6, quant_74=7:9)),
        data.frame(time=1:3, obs=4:6),
    ),
    "could not compute/obtain 97.5% quantile from data frame"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, obs=4:6),
      c(1, 25)
    ),
    "could not compute/obtain 1% quantile from data frame"
  )
  
  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3,quant_25=4:6, quant_75=7:9)),
      data.frame(time=1:3, obs=4:6),
      c(25,74.9)
    ),
    "could not compute/obtain 74.9% quantile from data frame"
  )

  expect_error(
    accuracy(
      create_forecast(data.frame(time=1:3, mean=4:6)),
      data.frame(time=1:3, obs=4:6),
    ),
    "could not compute/obtain 2.5% quantile from data frame"
  )
})

test_that("accuracy() handles NAs", {
  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, raw=list(c(1,NA, 3), c(4, NA, NA), as.numeric(c(NA,NA,NA)))
      )),
      data.frame(time=1:3, obs=c(1.5, 3, 100)),
      c(25,75)
    ),
    "data frame contains row with no raw data"
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=1:4, obs=c(4, 8, 100, 100)),
      c(0,100)
    ),
    2/3
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=1:3, obs=c(NA,8,10)),
      c(0,50)
    ),
    "missing observations for some forecast time points"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:4, quant_25=c(10,NA,11,12), quant_75=c(13,14,NA,15)
      )),
      data.frame(time=1:4, obs=c(10, 100, 100, 15.1)),
      c(25,75)
    ),
    "some/all forecast quantiles are NA"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, quant_25=c(10, 11, NA), quant_75=c(20, 21, 22)
      )),
      data.frame(time=1:3, obs=c(10, 11, 100)),
      c(25,75)
    ),
    "some/all forecast quantiles are NA"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(NA_real_,NA_real_,NA_real_))),
      data.frame(time=1:3, obs=4:6),
      c(0,100)
    ),
    "data frame contains row with no raw data"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
      data.frame(time=4:6, obs=1:3),
      c(0,100)
    ),
    "missing observations for some forecast time points"
  )

  expect_error(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,as.numeric(c(NA,NA,NA)),10:12))),
      data.frame(time=2:4, obs=c(5, NA, 10)),
      c(0, 100)
    ),
    "data frame contains row with no raw data"
  )
})


test_that("accuracy() raw works", {
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, obs=c(5, 7.5, 11.5)),
      c(25, 75)
    ),
    1
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, obs=c(5, 7.4, 11.6)),
      c(25, 75)
    ),
    1/3
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, obs=c(0, 7.4, 11.6)),
      c(25, 75)
    ),
    0
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=c(4,5,6))),
      data.frame(time=1:3, obs=c(4,5,7)),
      c(25, 75)
    ),
    2/3
  )
  
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
      data.frame(time=1:3, obs=c(4, 9, 13)),
      c(0,100)
    ),
    2/3
  )
})

test_that("accuracy() quant works", {
  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, quant_25=4:6, quant_50=7:9, mean=100:102, quant_75=200:202
      )),
      data.frame(time=1:3, obs=c(4, 201, 1000)),
      c(25, 75)
    ),
    2/3
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(
        time=1:3, quant_25=4:6, quant_50=7:9, mean=100:102, quant_75=200:202
      )),
      data.frame(time=1:3, obs=c(4, 201, 1000)),
      c(25,50)
    ),
    1/3
  )

  expect_equal(
    accuracy(
      create_forecast(
        dplyr::tibble(time=1:5, quant_25=c(6,6,6,6,6), quant_75=c(10,10,10,10,10)),
        forecast_time=3
      ),
      data.frame(time=1:5, obs=c(0, 2.4, 5, 9.5, 10)),
      c(25, 75)
    ),
    2/3
  )
})

test_that("accuracy(..., summarize=FALSE) works", {
  expect_equal(
    accuracy(
      create_forecast(
        dplyr::tibble(time=1:5, quant_25=c(2,2,2,2,2), quant_75=c(7,7,7,7,7)),
        forecast_time=3
      ),
      data.frame(time=3:5, obs=c(0,5,10)),
      interval=c(25, 75),
      summarize=FALSE
    ),
    dplyr::tibble(time=3:5, obs=c(0,5,10), score=c(FALSE,TRUE,FALSE))
  )
})
