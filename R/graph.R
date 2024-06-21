#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @template graph
#' @param fcst The forecast object (see the output of `create_forecast()`)
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_ensemble <- function(graph=NULL, fcst) {
    #TODO make the fit data different color
    #TODO opacity parameter
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    validate_forecast(fcst)

    if(! "raw" %in% fcst$data_types) {
        stop("raw data needed to graph ensemble")
    }

    # convert to long format for easy ggplot interfacing
    df <- wide2long(fcst$data)

    graph + ggplot2::geom_line(ggplot2::aes(x=time, y=raw, group=realization), df)
}


#' Intelligently graph observations
#'
#' Graph observation points. If provided with a forecast,
#'  the points can be color-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame. If it contains a `score` column,
#'  the observation points will be color-coded according to the scores.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_observations <- function(graph=NULL, obs) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) {return(
        graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs, color=score), obs)
    )} else {return(
        graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs), obs)
    )}
}


# helper and wrapper functions for graphing functionality

#' Convert raw forecast data to long format
#'
#' Given a data frame with raw data, convert it to long format.
#'  Raw forecast data is usually stored in wide format, where the `raw` column
#'  contains vectors of realization numbers.
#'  In long format, each row contains a single data point for a single realization,
#'  and a new column keeps track of the realization number for that row.
#'
#' @param df The data frame. It should contain raw forecast data (see `create_forecast()`).
#'
#' @returns The data frame in long format.
#' @autoglobal
#'
#' @examples
#' # TODO
wide2long <- function(df) {
    # go through every row of the data frame
    1:nrow(df) |>
        purrr::map(\(row) {
            # expand that row into its own data frame
            dplyr::tibble(
                # recycle the time
                time=df[[row,"time"]],
                # number the realizations
                realization=1:length(df[[row,"raw"]][[1]]),
                # and put the raw values in long form
                raw=df[[row,"raw"]][[1]]
            )
        }) |>
        # now combine them back into one data frame
        purrr::reduce(dplyr::bind_rows) |>
        # remove NA rows
        dplyr::filter(!is.na(raw))
}