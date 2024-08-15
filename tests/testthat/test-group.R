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

test_that("group_all() works", {
  expect_equal(
    group_all(groupex),
    dplyr::group_by(groupex, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    groupex |> dplyr::group_by(time) |> group_all(),
    dplyr::group_by(groupex, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    groupex |> dplyr::group_by(time) |> group_all(.add=TRUE),
    dplyr::group_by(groupex, time, grp_variable, grp_province, grp_scenario)
  )
})