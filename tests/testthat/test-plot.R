# test the facets on every function that does facets
test_that("facets work", {
  vdiffr::expect_doppelganger("facet1",
    NULL |> plot_observations(groups_obs |> dplyr::filter(grp_variable=="hosp"))
  )

  vdiffr::expect_doppelganger("facet2",
    NULL |> plot_observations(groups_obs |> dplyr::filter(grp_scenario==1))
  )

  vdiffr::expect_doppelganger("facet3",
    NULL |> plot_observations(groups_obs |> dplyr::filter(grp_province=="ON"))
  )

  vdiffr::expect_doppelganger("facet4",
    NULL |> plot_observations(groups_obs |> dplyr::filter(grp_province=="ON", grp_scenario==1))
  )

  expect_error(
    NULL |> plot_observations(groups_obs),
    "more than 2 groups contain multiple values"
  )

  vdiffr::expect_doppelganger("facet5",
    NULL |> plot_ensemble(create_forecast(groups2 |> dplyr::filter(grp_province=="ON")))
  )

  vdiffr::expect_doppelganger("facet6",
    NULL |> plot_ensemble(create_forecast(groups2 |> dplyr::filter(grp_province=="ON", grp_variable=="case")))
  )


})