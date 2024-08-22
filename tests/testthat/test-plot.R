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
})