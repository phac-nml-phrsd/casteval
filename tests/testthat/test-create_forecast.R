test_that("create_forecast() does input validation", {
  expect_error(create_forecast(5), "has invalid type")
  expect_error(create_forecast(list()), "list of data frames is empty")
  expect_error(create_forecast(list(5, data.frame())), "list.*non-data-frames")
  
  expect_error(
    create_forecast(list(
      data.frame(time=1,raw=2),
      data.frame(time=lubridate:::ymd("2024-01-01"),raw=2),
      data.frame(time=3,raw=4)
    )),
    "all data frames must have same time type"
  )

  expect_error(
    create_forecast(list(
      data.frame(time=1,raw=2),
      data.frame(time=2,mean=5)
    )),
    "all data frames must contain raw data"
  )

  expect_error(
    create_forecast(data.frame(time=1,raw=2), forecast_time=lubridate::ymd("2024-01-01")),
    "type of `t` does not match `fcst.*time_type`"
  )
})

test_that("create_forecast() works with single data frame", {
  expect_equal(
    create_forecast(
      data.frame(time=1:3, raw=4:6),
      name="a forecast",
      forecast_time=2
    ),
    list(
      name="a forecast",
      forecast_time=2,
      time_type="numeric",
      data_types="raw",
      data=data.frame(time=1:3,raw=4:6)
    )
  )

  expect_equal(
    create_forecast(
      data.frame(time=1,mean=4,quant_50=5),
    ),
    list(
      name=NULL,
      forecast_time=NULL,
      time_type="numeric",
      data_types=c("mean","quant"),
      data=data.frame(time=1,mean=4,quant_50=5)
    )
  )

  expect_equal(
    create_forecast(dplyr::tibble(time=1:2, raw=list(10:11, 12:13))),
    list(
      name=NULL,
      forecast_time=NULL,
      time_type="numeric",
      data_types="raw",
      data=dplyr::tibble(time=1:2, raw=list(10:11, 12:13))
    )
  )
})

test_that("create_forecast() works with list of data frames", {
  expect_equal(
    create_forecast(
      list(
        dplyr::tibble(time=1:5,raw=6:10),
        dplyr::tibble(time=2:6,raw=7:11),
        dplyr::tibble(time=3:7,raw=8:12)
      ),
      name="salutations",
      forecast_time=567
    ),
    list(
      name="salutations",
      forecast_time=567,
      time_type="numeric",
      data_types="raw",
      data=dplyr::tibble(
        time=1:7,
        raw=list(
          c(6,NA,NA),
          c(7,7,NA),
          c(8,8,8),
          c(9,9,9),
          c(10,10,10),
          c(NA,11,11),
          c(NA,NA,12)
        )
      )
    )
  )

  expect_equal(
    create_forecast(
      list(data.frame(time=lubridate::ymd("2024-01-01"),raw=1))
    ),
    list(
      name=NULL,
      forecast_time=NULL,
      time_type="date",
      data_types="raw",
      data=data.frame(time=lubridate::ymd("2024-01-01"),raw=1)
    )
  )
})
