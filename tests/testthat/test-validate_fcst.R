test_that("validate_forecast() works", {
  expect_error(
    validate_forecast(5),
    "forecast must be named list"
  )

  expect_error(
    validate_forecast(data.frame(time=1:3,raw=4:6)),
    "forecast must be named list containing data frame, not just a data frame"
  )

  expect_error(
    validate_forecast(list(name="hello", forecast_time=45)),
    "forecast must contain `data`"
  )

  expect_error(
    validate_forecast(list(data=data.frame(time=1:3,sim=4:6))),
    "sim column present but val column missing"
  )

  expect_error(
    validate_forecast(list(
      data=data.frame(time=1,val=4), forecast_time=lubridate::ymd("2024-01-01")
    )),
    "type of `t` does not match `fcst.*time_type`"
  )

  expect_equal(
    validate_forecast(list(
      name="hi", forecast_time=40, data=data.frame(time=1:3, val=4:6)
    )),
    NULL
  )
})

test_that("validate_obs() works", {
  expect_error(validate_obs(list(data=data.frame(time=1,val_obs=3))), "obs must be data frame")

  expect_error(
    validate_obs(data.frame(time=NULL,val_obs=NULL)),
    "obs data frame has no rows"
  )

  expect_error(
    validate_obs(data.frame(val_obs=1)),
    "obs data frame requires time column"
  )

  expect_error(
    validate_obs(data.frame(time=1)),
    "obs data frame requires val_obs column"
  )

  expect_error(
    validate_obs(data.frame(time=c("hi", "bye"))),
    "time column must be either numeric, Date, or date-time"
  )

  expect_error(
    validate_obs(data.frame(time=4,val_obs=c("hi"))),
    "obs\\$val_obs must be numeric"
  )

  expect_error(
    validate_obs(data.frame(time=c(1,2,2), val_obs=c(1,2,3))),
    "obs contains duplicate observations at time 2"
  )

  expect_equal(
    validate_obs(groupex_obs),
    NULL
  )

  expect_error(
    validate_obs(dplyr::select(groupex_obs, time, grp_scenario, grp_variable, val_obs)),
    "obs contains duplicate observations at time 1"
  )

  expect_error(
    validate_obs(data.frame(time=1, val_obs=4, grp_variable=3, grp_=2)),
    "provided empty group name"
  )

  expect_equal(
    validate_obs(data.frame(time=1:3, val_obs=4:6)),
    NULL
  )
})

test_that("validate_fcst_obs_pair() works", {
  expect_equal(
    validate_fcst_obs_pair(
      create_forecast(data.frame(time=1:10, val=11:20)),
      data.frame(time=101:110, val_obs=111:120)
    ),
    NULL
  )

  expect_error(
    validate_fcst_obs_pair(
      create_forecast(data.frame(time=1:10, val=11:20)),
      data.frame(time=lubridate::ymd("2024-01-01"), val_obs=5)
    ),
    "observations time type must match forecast time type"
  )
})

test_that("validate_data_frame() works", {
  expect_error(
    validate_data_frame(data.frame(time=NULL,val=NULL)),
    "data frame has no rows"
  )

  expect_error(
    validate_data_frame(data.frame(
      val=1:3
    )),
    "data frame must contain `time` column"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=c("hi", "bye")
    )),
    "time column must be either numeric, Date, or date-time"
  )

  expect_error(
    validate_data_frame(dplyr::tibble(
      time=list(1,2,3)
    )),
    "time column must be either numeric, Date, or date-time"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val_q1q2=4:6
    )),
    "invalid quantile column name"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val_qabc=4:6
    )),
    "invalid quantile percentage"
  )

  expect_error(
    validate_data_frame({
      df<-data.frame(time=1:3)
      df[["val_q-1"]]<-4:6
      df
    }),
    "quantile percentage -1 out of range"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val_q100.1=4:6
    )),
    "quantile percentage 100\\.1 out of range"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, sim=c(TRUE,TRUE,TRUE), val=4:6
    )),
    "sim column must be numeric"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val_q5=c("hi","bye","hey")
    )),
    "val_q5 column must be numeric"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, sim=4:6
    )),
    "sim column present but val column missing"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3
    )),
    "data frame contains no data columns"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val=4:6, val_q2.5=7:9, val_q50=10:12, val_mean=13:15
    )),
    "both summarized and unsummarized \\(`val`\\) data provided"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1:3, val_q2.5=10:12, val_q97.5=7:9
    )),
    "quantiles have impossible values in row 1"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=c(1,1,2,2,2,3,3), sim=c(1,2,1,2,2,1,2), val=1:7
    )),
    "data frame contains duplicate entries"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=c(1,1,2), val_q50=c(4,5,6)
    )),
    "data frame contains duplicate entries"
  )

  expect_equal(
    validate_data_frame(groupex),
    NULL
  )

  expect_error(
    validate_data_frame(dplyr::select(groupex, time, val_q5, val_q95, grp_variable, grp_province)),
    "data frame contains duplicate entries"
  )

  expect_error(
    validate_data_frame(data.frame(
      time=1, val=2, grp_scenario=3, grp_=4
    )),
    "provided empty group name"
  )

  expect_equal(
    validate_data_frame(data.frame(
      time=1:3, val_q2.5=7:9, val_q50=10:12, val_mean=13:15
    )),
    NULL
  )

  expect_equal(
    validate_data_frame(data.frame(
      time=c(1,1,2,2,3,3), sim=c(1,2,1,2,1,2), val=c(1,2,3,4,5,6)
    )),
    NULL
  )
})
