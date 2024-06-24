#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @template graph
#' @template fcst
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' casteval:::graph_ensemble(
#'   NULL,
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))
#' ))
#' 
#' casteval:::graph_ensemble(
#'   NULL,
#'   create_forecast(data.frame(time=lubridate::as_datetime(c(0,20000,100000)), raw=c(20,30,40))
#' ))
graph_ensemble <- function(graph=NULL, fcst) {
    #TODO make the fit data different color
    #TODO opacity parameter
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

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
#' fc <- create_forecast(dplyr::tibble(
#'   time=1:3,
#'   raw=list(4:6, 7:9, 10:12)
#' ))
#' obs <- data.frame(time=1:3, obs=c(5,9,13))
#' 
#' # graph observations on their own
#' casteval:::graph_observations(NULL, obs)
#' 
#' # graph observations alongside forecast data
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), obs)
#' 
#' # graph observations alongside forecast data, and color-code by score
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), neglog(fc, obs))
graph_observations <- function(graph=NULL, obs) {
    # TODO figure out where to eliminate pre-forecast-date observations
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) { 
        # TODO print debugging reveals that this code is covered by tests, but covr::report() marks it as untested. This is likely a problem with either vdiffr or covr
        return(
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs, color=score), obs)
        )
    } else {
        return(
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs), obs)
        )
    }
}


#' Graph forecast quantiles
#'
#' Graph lines indicating quantiles of a forecast.
#'
#' @template graph
#' @template fcst
#' @param quant A numeric vector containing the quantiles to be graphed, as percentages.
#'  If `NULL`, all quantiles present in the forecast data frame will be graphed.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_quantiles <- function(graph=NULL, fcst, quant=NULL) {
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    
    if(!is.null(quant)) {
        
    }
}


#' Graph forecast confidence intervals
#'
#' Graph shaded bands indicating confidence intervals of a forecast.
#'
#' @template graph
#' @template fcst
#' @param conf A numeric vector containing the confidence intervals to be displayed, as percentages.
#'  If `NULL`, the confidence intervals will be inferred from the quantiles present in the forecast data frame.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_confidence_intervals <- function(graph=NULL, fcst, conf=NULL) {

}

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
#' # dplyr::tibble(
#' #   time=c(1,1,1,2,2,2,3,3,3),
#' #   realization=c(1,2,3,1,2,3,1,2,3),
#' #   raw=c(4,5,6,7,8,9,10,11,12)
#' # )
#' casteval:::wide2long(dplyr::tibble(
#'   time=1:3,
#'   raw=list(4:6, 7:9, 10:12)
#' ))
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