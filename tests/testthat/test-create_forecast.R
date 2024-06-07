test_that("create_forecast() does input validation", {
  expect_error(create_forecast(5), "has invalid type")
  expect_error(create_forecast(list()), "list of data frames is empty")
  expect_error(create_forecast(list(5, data.frame())), "list.*non-data-frames")
  
})
