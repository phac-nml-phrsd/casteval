# TODO legends, axes, plot titles, scales. some graphs will have multiple legends so that has to be taken into consideration.

#' Graph one or more forecasts
#'
#' Graph one or more forecasts, along with corresponding observations, quantiles, etc.
#'
#' @param fcsts A single forecast object, or a list of forecast objects (see output of `create_forecast()`).
#' @param obs (Optional) An observations data frame.
#'  If provided, they will be overlaid over the forecast(s) as points.
#' @param confs (Optional) A vector of numbers from 0 to 100.
#'  The corresponding confidence intervals will be displayed in the resulting graph(s)
#' @param score (Optional) A scoring function.
#'  The function will be used to score the `obs` against the forecast(s).
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
graph_forecasts <- function(fcsts, obs=NULL, confs=NULL, score=NULL) {
    # in case fcsts is a single forecast, wrap it in a list for consistency
    if(is.data.frame(fcsts)) {
        stop("TODO")
        fcsts <- list(fcsts)
    }

    if(length(fcsts) == 0) {
        stop("0 forecasts provided")
    }

    # if obs provided, validate against every forecast
    if(!is.null(obs)) {
        fcsts |> purrr::map(\(fc) validate_fcst_obs_pair(fc, obs))
    }

    # check that all forecast time types are the same
    time_types <- fcsts |> purrr::map(\(fc) fc$time_type) |> unique()
    if(length(time_types) > 1) {
        # this could be changed into a warning
        stop("provided forecasts with different time types")
    }

    # get all the names (strings and NULLs)
    names <- fcsts |> purrr::map(\(fc) fc$name)

    # complain if there are duplicate names
    if(names |> purrr::discard(is.null) |> duplicates() |> any()) {
        stop("provided duplicate forecast names")
    }

    # come up with placeholder names for all the NULLs
    n <- 1
    for(i in seq_along(names)) {
        if(!is.null(names[[i]])) {
            next
        }

        # increase n until "Forecast {n}" is available
        name <- paste("Forecast", n)
        while(name %in% names) {
            n <- n + 1
            name <- paste("Forecast", n)
        }

        names[[i]] <- name
    }
}

# TODO make long-form scenarios, provinces, etc. work with facets & everything else
# TODO <1 default alpha in every graphing function