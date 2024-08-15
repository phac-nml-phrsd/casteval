test_that("get_group_names() works", {
  expect_equal(get_group_names(data.frame(time=1)), character(0))

  expect_equal(
    get_group_names(data.frame(time=1, grp_variable=2, grp_scenario=3)),
    c("variable", "scenario")
  )

  expect_equal(
    get_group_names(data.frame(time=1, grp_1_2=3, grp___=4)),
    c("1_2", "__")
  )
})