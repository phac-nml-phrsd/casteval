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
  tb <- dplyr::tribble(
    ~time,  ~grp_variable,  ~grp_province,  ~grp_scenario,  ~val_q5,    ~val_q95,
    1,      "hosp",         "ON",           1,              10,         20,
    1,      "case",         "ON",           1,              1000,       2000,
    1,      "hosp",         "QC",           1,              11,         21,
    1,      "case",         "QC",           1,              1100,       2100,
    1,      "hosp",         "ON",           2,              15,         25,
    1,      "case",         "ON",           2,              1500,       2500,
    1,      "hosp",         "QC",           2,              16,         26,
    1,      "case",         "QC",           2,              1600,       2600,
    2,      "hosp",         "ON",           1,              50,         60,
    2,      "case",         "ON",           1,              5000,       6000,
    2,      "hosp",         "QC",           1,              51,         61,
    2,      "case",         "QC",           1,              5100,       6100,
    2,      "hosp",         "ON",           2,              55,         65,
    2,      "case",         "ON",           2,              5500,       6500,
    2,      "hosp",         "QC",           2,              56,         66,
    2,      "case",         "QC",           2,              5600,       6600
  )

  expect_equal(
    group_all(tb),
    dplyr::group_by(tb, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    tb |> dplyr::group_by(time) |> group_all(),
    dplyr::group_by(tb, grp_variable, grp_province, grp_scenario)
  )

  expect_equal(
    tb |> dplyr::group_by(time) |> group_all(.add=TRUE),
    dplyr::group_by(tb, time, grp_variable, grp_province, grp_scenario)
  )
})