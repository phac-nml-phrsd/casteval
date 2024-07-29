test_that("accuracy() validates", {
  fc <- create_forecast(data.frame(time=1:3, val=4:6))
  obs <- data.frame(time=1:3, val_obs=4:6)
  expect_error(
    accuracy(fc, obs),
    "could not infer quantile pairs from forecast data"
  )

  expect_error(
    accuracy(fc, obs, "hi"),
    "`quant_pairs` must be either NULL, pair of quantiles, or list of pairs of quantiles"
  )

  expect_error(
    accuracy(fc, obs, list()),
    "`quant_pairs` is empty"
  )

  expect_error(
    accuracy(
      fc, obs, list(c(20,80), c(60,40))
    ),
    "first quantile in pair must be less than second quantile in pair"
  )

  expect_error(
    accuracy(fc, obs, numeric(0)),
    "quantile pair must have length 2"
  )

  expect_equal(
    accuracy(
        fc, obs, list(c(2.5, 97.5))
    ),
    1
  )

  expect_error(
    accuracy(
        create_forecast(data.frame(time=1:3, val_q25=4:6, val_q75=7:9)),
        data.frame(time=1:3, val_obs=4:6),
        list(c(2.5, 97.5))
    ),
    "could not compute/obtain 2.5% quantile from data frame"
  )

  # wrapping expect_warning in expect_error to test for both
  # probably bad practice but it works
  expect_error(
    expect_warning(
      val <- accuracy(
          create_forecast(data.frame(time=1:3, val_q2.5=4:6, val_q74=7:9)),
          data.frame(time=1:3, val_obs=4:6),
      ),
      "2.5% quantile is unpaired"
    ),
    "could not infer quantile pairs from forecast data"
  )

  expect_error(
    expect_warning(
      accuracy(
          create_forecast(data.frame(time=1:3, val_q2.5=4:6, val_q74=7:9)),
          data.frame(time=1:3, val_obs=4:6),
          list(c(2.5, 97.5))
      ),
      "2.5% quantile is unpaired"
    ),
    "could not compute/obtain 97.5% quantile from data frame"
  )

  expect_error(
    accuracy(
      fc, data.frame(time=4:6, val_obs=7:9), c(0.5,99.5)
    ),
    "observations and forecast data share no time points"
  )
})

# TODO figure out what to do with this once NAs are re-figured out
# test_that("accuracy() handles NAs", {
#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(
#         time=1:3, raw=list(c(1,NA, 3), c(4, NA, NA), as.numeric(c(NA,NA,NA)))
#       )),
#       data.frame(time=1:3, obs=c(1.5, 3, 100)),
#       c(25,75)
#     ),
#     "data frame contains row with no raw data"
#   )

#   expect_equal(
#     accuracy(
#       create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
#       data.frame(time=1:4, obs=c(4, 8, 100, 100)),
#       c(0,100)
#     ),
#     2/3
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
#       data.frame(time=1:3, obs=c(NA,8,10)),
#       c(0,50)
#     ),
#     "missing observations for some forecast time points"
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(
#         time=1:4, quant_25=c(10,NA,11,12), quant_75=c(13,14,NA,15)
#       )),
#       data.frame(time=1:4, obs=c(10, 100, 100, 15.1)),
#       c(25,75)
#     ),
#     "some/all forecast quantiles are NA"
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(
#         time=1:3, quant_25=c(10, 11, NA), quant_75=c(20, 21, 22)
#       )),
#       data.frame(time=1:3, obs=c(10, 11, 100)),
#       c(25,75)
#     ),
#     "some/all forecast quantiles are NA"
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(time=1:3, raw=list(NA_real_,NA_real_,NA_real_))),
#       data.frame(time=1:3, obs=4:6),
#       c(0,100)
#     ),
#     "data frame contains row with no raw data"
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,7:9,10:12))),
#       data.frame(time=4:6, obs=1:3),
#       c(0,100)
#     ),
#     "missing observations for some forecast time points"
#   )

#   expect_error(
#     accuracy(
#       create_forecast(dplyr::tibble(time=1:3, raw=list(4:6,as.numeric(c(NA,NA,NA)),10:12))),
#       data.frame(time=2:4, obs=c(5, NA, 10)),
#       c(0, 100)
#     ),
#     "data frame contains row with no raw data"
#   )
# })


test_that("accuracy() raw values works", {
  fc <- create_forecast(dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=4:12))
  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(5.1, 7.5, 11.5)),
      list(c(25, 75), c(40,60), c(49,51))
    ),
    c(1, 1/3, 0)
  )

  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(5, 7.4, 11.6)),
      list(c(25, 75))
    ),
    1/3
  )

  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(0, 7.4, 11.6)),
      c(25, 75)
    ),
    0
  )

  expect_equal(
    accuracy(
      create_forecast(dplyr::tibble(time=1:3, val=c(4,5,6))),
      data.frame(time=1:3, val_obs=c(4,5,7)),
      c(25, 75)
    ),
    2/3
  )
  
  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(4, 9, 13)),
      c(0,100)
    ),
    2/3
  )
})

test_that("accuracy() quant works", {
  fc <- create_forecast(dplyr::tibble(
    time=1:3, val_q25=4:6, val_q50=7:9, val_mean=100:102, val_q75=200:202
  ))
  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(4, 201, 1000)),
      list(c(25, 75), c(25, 50))
    ),
    c(2/3, 1/3)
  )

  expect_equal(
    accuracy(
      fc,
      data.frame(time=1:3, val_obs=c(4, 201, 1000)),
      c(25,50)
    ),
    1/3
  )

  expect_equal(
    accuracy(
      create_forecast(
        dplyr::tibble(time=1:5, val_q25=c(6,6,6,6,6), val_q75=c(10,10,10,10,10)),
        forecast_time=3
      ),
      data.frame(time=1:5, val_obs=c(0, 2.4, 5, 9.5, 10)),
      list(c(25, 75))
    ),
    2/3
  )
})

test_that("accuracy(..., summarize=FALSE) works", {
  fc1 <- create_forecast(dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=4:12))
  expect_equal(
    accuracy(
      fc1,
      data.frame(time=1:3, val_obs=c(5.1, 7.5, 11.5)),
      quant_pairs=list(c(25, 75), c(40,60), c(49,51)),
      summarize=FALSE
    ),
    data.frame(
      time=rep(1:3, 3),
      val_obs=rep(c(5.1,7.5,11.5), 3),
      score=c(rep(TRUE, 4), rep(FALSE, 5)),
      pair=rep(1:3, each=3)
    )
  )

  fc2 <- create_forecast(dplyr::tibble(
    time=1:3, val_q25=4:6, val_q50=7:9, val_mean=100:102, val_q75=200:202
  ))
  expect_equal(
    accuracy(
      fc2,
      data.frame(time=1:3, val_obs=c(4, 201, 1000)),
      list(c(25, 75), c(25, 50)),
      summarize=FALSE
    ),
    data.frame(
      time=rep(1:3, 2),
      val_obs=rep(c(4, 201, 1000), 2),
      score=c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
      pair=rep(1:2, each=3)
    )
  )

  expect_equal(
    accuracy(
      create_forecast(
        dplyr::tibble(time=1:5, val_q25=c(2,2,2,2,2), val_q75=c(7,7,7,7,7)),
        forecast_time=3
      ),
      data.frame(time=3:5, val_obs=c(0,5,10)),
      quant_pairs=c(25, 75),
      summarize=FALSE
    ),
    data.frame(time=3:5, val_obs=c(0,5,10), score=c(FALSE,TRUE,FALSE), pair=rep(1,3))
  )
})

test_that("make_accuracy() works", {
  fc <- create_forecast(list(
    time=1:3,
    vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
  ))
  fc2 <- create_forecast(data.frame(time=1:3, val_q10=c(6,6,6), val_q90=c(7,7,7)))

  obs <- data.frame(time=1:3, val_obs=5:7)

  expect_equal(
    make_accuracy(c(5,95))(fc, obs),
    2/3
  )

  expect_equal(
    make_accuracy(list(c(5,95), c(53,90)))(fc,obs),
    c(2/3,0)
  )

  expect_equal(
    make_accuracy(NULL)(fc2,obs),
    2/3
  )
})