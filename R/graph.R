# TODO legends, axes, plot titles, scales. some graphs will have multiple legends so that has to be taken into consideration.

#' Graph one or more forecasts
#'
#' Graph one or more forecasts, along with corresponding observations, quantiles, etc.
#'
#' @param fcsts A single forecast object, or a list of forecast objects (see output of `create_forecast()`).
#' @param obs (Optional) An observations data frame.
#'  If provided, they will be overlaid over the forecast(s) as points.
#' @param raw (Optional) A boolean. Defaults to TRUE, in which case raw ensemble curves will be displayed.
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
graph_forecasts <- function(fcsts, obs=NULL, raw=TRUE, confs=NULL, score=NULL) {
    # TODO more rigorous error checking for this function
    # in case fcsts is a single forecast, wrap it in a list for consistency
    if("data" %in% names(fcsts)) {
        fcsts <- list(fcsts)
    }

    if(length(fcsts) == 0) {
        stop("0 forecasts provided")
    }

    # if obs provided, validate against every forecast
    if(!is.null(obs)) {
        fcsts |> purrr::map(\(fc) validate_fcst_obs_pair(fc, obs))
    } else {
        fcsts |> purrr::map(\(fc) validate_forecast(fc))
    }

    if(raw) {
        if(any(as.logical(purrr::map(fcsts, \(fc) ! "raw" %in% fc$data_types)))) {
            stop("`raw` parameter set to TRUE but not all forecasts contain raw data")
        }
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
    if(names |> purrr::discard(is.null) |> duplicated() |> any()) {
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
        fcsts[[i]]$data$name <- name
    }

    # score each data frame
    if(!is.null(obs) && !is.null(score)) {
        fcsts <- fcsts |> purrr::map(\(fc) score(fc, obs, summarize=FALSE))
    }

    # combine rows
    df <- do.call(dplyr::bind_rows, purrr::map(fcsts, \(fc) fc$data))
    fc <- create_forecast(df)

    graph <- ggplot2::ggplot(df)
    if(raw) {
        graph <- graph |> graph_ensemble(fc)
    }

    if(!is.null(confs)) {
        graph <- graph |> graph_confidence_intervals(fc, confs)
    }

    graph + ggplot2::facet_wrap(~name)
}

# TODO make long-form scenarios, provinces, etc. work with facets & everything else
# TODO <1 default alpha in every graphing function