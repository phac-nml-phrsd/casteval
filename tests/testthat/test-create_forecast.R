test_that("create_forecast() does input validation", {
  expect_error(
    create_forecast(5),
    "`dat` has invalid type. See `\\?create_forecast` or `vignette\\(topic='casteval', package='casteval'\\)` for proper usage"
  )
  expect_error(create_forecast(list()), "`dat` list must contain `time` and `vals` fields")

  expect_error(
    create_forecast(data.frame(time=1,val=2), forecast_time=lubridate::ymd("2024-01-01")),
    "type of `t` does not match `fcst\\$time_type`"
  )

  expect_error(
    create_forecast(list(a=5,time=1:3)),
    "`dat` list must contain `time` and `vals` fields"
  )
  expect_error(
    create_forecast(list(vals=6, a=4)),
    "`dat` list must contain `time` and `vals` fields"
  )
})

test_that("create_forecast() accepts data frame", {
  expect_error(
    create_forecast(data.frame(time=1:3, sim=c(1,1,1))),
    "sim column present but val column missing"
  )

  expect_error(
    create_forecast(data.frame(time=c(1,2,2,3), sim=c(1,1,1,1), val=1:4)),
    "data frame contains duplicate entries"
  )

  expect_error(
    create_forecast(data.frame(time=1:3, val=4:6, val_q2.5=7:9)),
    "both summarized and unsummarized.*data provided"
  )

  expect_error(
    create_forecast(data.frame(time=1:3)),
    "data frame contains no data columns"
  )

  expect_error(
    create_forecast(data.frame(time=1:3, val_q2.5=c(10,10,10), val_q97.5=c(11,10,9))),
    "quantiles have impossible values in row 3"
  )

  expect_equal(
    create_forecast(
      data.frame(time=1:3, val=4:6),
      name="a forecast",
      forecast_time=2
    ),
    list(
      name="a forecast",
      forecast_time=2,
      data=data.frame(time=1:3,val=4:6)
    )
  )

  expect_equal(
    create_forecast(
      data.frame(time=1,val_mean=4,val_q50=5),
    ),
    list(
      name=NULL,
      forecast_time=NULL,
      data=data.frame(time=1,val_mean=4,val_q50=5)
    )
  )

  expect_equal(
    create_forecast(dplyr::tibble(time=c(1,1,2,2), sim=c(1,2,1,2), val=c(10:13))),
    list(
      name=NULL,
      forecast_time=NULL,
      data=dplyr::tibble(time=c(1,1,2,2), sim=c(1,2,1,2), val=c(10:13))
    )
  )

  expect_equal(
    create_forecast(dplyr::tibble(time=c(1,1,2,2), val=c(10:13))),
    list(
      name=NULL,
      forecast_time=NULL,
      data=dplyr::tibble(time=c(1,1,2,2), val=10:13)
    )
  )
})

test_that("create_forecast() works with ensemble of realizations", {
  expect_error(
    create_forecast(
      list(time="a", vals=3)
    ),
    "time column must be either numeric, Date, or date-time"
  )

  expect_error(
    create_forecast(
      list(time=1, vals=4)
    ),
    "`dat\\$vals` must be a list$"
  )

  expect_error(
    create_forecast(
      list(time=numeric(0), vals=list())
    ),
    "`dat\\$time` is empty"
  )

  expect_error(
    create_forecast(
      list(time=1:3, vals=list())
    ),
    "`dat.*vals` is empty"
  )

  expect_error(
    create_forecast(
      list(time=1:3, vals=list("1"))
    ),
    "`dat.*vals` must be list of numeric vectors"
  )

  expect_error(
    create_forecast(
      list(time=1:3, vals=list(1:3,4:5,7:9))
    ),
    "all vectors in `dat.*vals` must have the same length as `dat.*time`"
  )

  expect_equal(
    create_forecast(
      list(time=1:3, vals=list(4:6, 7:9, 10:12)), name="hi", forecast_time=1
    ),
    list(
      name="hi",
      forecast_time=1,
      data=dplyr::tibble(time=c(1,2,3,1,2,3,1,2,3), sim=c(1,1,1,2,2,2,3,3,3), val=c(4:12))
    )
  )
})

test_that("create_forecast() checks quantile pairings", {
  # {testthat} idiom for "expect no warning"
  expect_warning(
    create_forecast(data.frame(time=1, val_q50=2, val_q2.5=0, val_q97.5=9)),
    NA
  )

  expect_warning(
    create_forecast(data.frame(time=1, val_q25=3, val_q75=5, val_q40=4)),
    "40% quantile is unpaired"
  )
})