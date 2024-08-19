test_that("bias() works", {
  expect_error(
    bias(create_forecast(data.frame(time=1:3, val_q25=4:6, val_q75=7:9)), data.frame(time=1:3, val_obs=4:6)),
    "raw, mean, or median forecast values required to compute bias"
  )

  obs <- data.frame(time=1:5, val_obs=rep(10,5))

  fc1 <- create_forecast(dplyr::tibble(
    time=c(1,1,2,2,3,3,4,4,5,5),
    val=c(9, 9, 9, 10, 10, 10, 10, 11, 11, 11)
  ))
  expect_equal(
    bias(fc1, obs, summarize=FALSE),
    dplyr::tibble(time=1:5, val_obs=rep(10,5), score=c(-1, -0.5, 0, 0.5, 1))
  )
  expect_equal(
    bias(fc1, obs),
    0
  )

  fc2 <- create_forecast(data.frame(
    time=c(1,1,1,2,2,2,3,3,3),
    val=c(9,9,9,10,10,10,11,9,9)
  ))
  expect_equal(
    bias(fc2, obs),
    -4/9
  )
  expect_equal(
    bias(fc2, obs,summarize=FALSE),
    dplyr::tibble(time=c(1,2,3), val_obs=c(10,10,10), score=c(-1,0,-1/3))
  )


  fc3 <- create_forecast(dplyr::tibble(time=1:3, val=c(5,5,5)))
  expect_equal(bias(fc3, obs), -1)

  fc4 <- create_forecast(data.frame(time=1:3, val=c(12,12,12)))
  expect_equal(bias(fc4, obs), 1)

  fc5 <- create_forecast(data.frame(time=1:3, val=c(5,5,15)))
  expect_equal(bias(fc5, obs), -1/3)

  fc6 <- create_forecast(data.frame(time=1:3, val_mean=c(12,12,10), val_q50=c(0,0,0)))
  expect_equal(bias(fc6, obs), 2/3)

  fc7 <- create_forecast(data.frame(time=1:3, val_q50=c(0,0,0)))
  expect_equal(bias(fc7, obs), -1)

  expect_equal(
    bias(fc7, obs, summarize=FALSE),
    dplyr::tibble(time=1:3, val_obs=c(10,10,10), score=c(-1, -1, -1))
  )
})

test_that("bias() grouping works", {
  fc <- create_forecast(groups1)
  obs <- groups_obs
  
  expect_equal(
    bias(fc, obs),
    join_fcst_obs(fc$data, obs) |> dplyr::mutate(score=sign(val_mean-val_obs)) |>
        group_all() |> dplyr::summarize(val_obs=val_obs[[1]], score=mean(score), .groups="drop")
  )

  expect_equal(
    bias(fc, obs, summarize=FALSE),
    join_fcst_obs(fc$data, obs) |> dplyr::mutate(score=sign(val_mean-val_obs)) |>
        group_all() |> dplyr::group_by(time, .add=TRUE) |>
        dplyr::summarize(val_obs=val_obs[[1]], score=mean(score), .groups="drop")
  )
})

# TODO test summarizing/unsummarizing of raw, mean, and median formats
# add output messages for whether using raw, mean, or median
# test graphing of bias unsummarized scoring