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
    group_all(groups1),
    dplyr::group_by(groups1, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    groups1 |> dplyr::group_by(time) |> group_all(),
    dplyr::group_by(groups1, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    groups1 |> dplyr::group_by(time) |> group_all(.add=TRUE),
    dplyr::group_by(groups1, time, grp_variable, grp_province, grp_scenario)
  )


  tb <- dplyr::tibble(time=1:3, val=1:3)
  expect_equal(
    group_all(tb),
    tb
  )

  expect_equal(
    tb |> dplyr::group_by(time) |> group_all(),
    tb
  )

  expect_equal(
    tb |> dplyr::group_by(time) |> group_all(.add=TRUE),
    dplyr::group_by(tb, time)
  )
})

test_that("has_groups() works", {
  expect_equal(
    has_groups(data.frame(time=1, val=2, grp_scenario=3)),
    TRUE
  )

  expect_equal(
    has_groups(data.frame(time=1,val=2)),
    FALSE
  )
})

test_that("get_plotting_groups() works", {
  expect_equal(
    get_plotting_groups(data.frame(time=1, val=2)),
    character(0)
  )

  expect_equal(
    get_plotting_groups(data.frame(time=1:5, val=2:6, grp_variable=3:7)),
    c("grp_variable")
  )

  expect_equal(
    get_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=8:10)),
    c("grp_var", "grp_loc")
  )

  expect_equal(
    get_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=10:12, grp_sce=13:15)),
    c("grp_var", "grp_loc", "grp_sce")
  )

  expect_equal(
    get_plotting_groups(data.frame(time=1:3, val=4:6, grp_var=7:9, grp_loc=c(1,1,1), grp_sce=13:15)),
    c("grp_var", "grp_sce")
  )

  expect_equal(
    get_plotting_groups(data.frame(time=1, val=2, grp_var=3, grp_loc=4, grp_sce=5, grp_foo=6, grp_bar=7)),
    character(0)
  )
})