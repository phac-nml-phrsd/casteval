# TODO legends, axes, plot titles, scales. some graphs will have multiple legends so that has to be taken into consideration.
# TODO experiment with different color scales (viridis maybe?). Try to make the 2-12-50 example in the vignette more visible
# TODO redo format of forecasts with facets in mind
# TODO revisit graph_forecasts() after redoing formatting

# TODO somehow provide a score decorator which can convert accuracy() TRUE/FALSE to HIT/MISS or something else, and also modify the colormap

#' Graph a forecast
#'
#' Graph a forecast, along with corresponding observations, confidence intervals, etc.
#'
#' @template fcst
#' @param obs (Optional) An observations data frame.
#'  If provided, they will be overlaid over the forecast as points.
#' @param confs (Optional) A vector of numbers from 0 to 100.
#'  The corresponding confidence interval(s) will be displayed in the resulting graph.
#' @param score (Optional) A scoring function.
#'  The function will be used to score the `obs` against the forecast.
#'  A scoring function should accept a forecast object, an observations data frame, as well as a `summarize` argument.
#'  See `accuracy()`, `neglog()` for examples
#'
#' @returns A ggplot2 object
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(dplyr::tibble(
#'   time=1:3,
#'   raw=list(c(4,5,4), c(7,6,6), c(8,7,6))
#' ))
#' obs <- data.frame(time=1:3, obs=5:7)
#' 
#' # graph forecast
#' graph_forecast(fc)
#' 
#' # graph forecast and observations
#' graph_forecast(fc, obs)
#' 
#' # graph forecast and confidence interval(s)
#' graph_forecast(fc, confs=c(50,95))
#' 
#' # highlight the observations inside the 95% confidence interval
#' graph_forecast(fc, obs, confs=95, score=\(...) accuracy(..., interval=c(2.5, 97.5)))
#' 
#' # show the negative log score of each observation
#' graph_forecast(fc, obs, score=neglog)
graph_forecast <- function(fcst, obs=NULL, confs=NULL, score=NULL) {
    # validate forecast and/or observations
    if(is.null(obs)) {
        validate_forecast(fcst)
    } else {
        validate_fcst_obs_pair(fcst, obs)
    }

    # score if necessary
    if(!is.null(score)) {
        if(is.null(obs)) {
            # could be converted to warning
            stop("scoring function provided without observations")
        }

        # TODO: wrap in error handler. scoring functions which don't support the summarize flag should error when passed it (or maybe return NULL)
        obs <- score(fcst, obs, summarize=FALSE)
    }

    # graph everything according to the parameters
    graph <- NULL
    if("raw" %in% fcst$data_types) {
        graph <- graph |> graph_ensemble(fcst)
    }

    # TODO graph mean if present
    # TODO parameters/flags for keeping/discarding fit data, observations data that doesn't correpsond to forecast data, etc.

    if(!is.null(confs)) {
        graph <- graph |> graph_confidence_intervals(fcst, confs)
    }

    if(!is.null(obs)) {
        graph <- graph |> graph_observations(obs)
    }

    # error if we didn't end up graphing anything
    if(is.null(graph)) {
        # could be turned into a warning
        stop("nothing was graphed. Please specify raw data, confidence intervals, and/or observations to be graphed.")
    }

    ## set labels and secondary features

    # mark the forecast time if provided
    if(!is.null(fcst$forecast_time)) {
        graph <- graph + ggplot2::geom_vline(alpha=0.2, xintercept=fcst$forecast_time)
    }

    # make the title the name if provided
    if(!is.null(fcst$name)) {
        graph <- graph + ggplot2::labs(title=fcst$name)
    }

    # name the axes
    graph <- graph + ggplot2::xlab("time") + ggplot2::ylab("value")

    # make both axes integers only
    graph <- graph + ggplot2::scale_x_continuous(breaks=integer_breaks())
    graph <- graph + ggplot2::scale_y_continuous(breaks=integer_breaks())

    graph
}

#' Integer breaks on ggplot2 axes
#'
#' A modified version of scales::pretty_breaks() for only integer-valued ticks
#' 
#' @param n Number of breaks.
#' @param ... other arguments passed to `pretty()`
#' 
#' @source https://gist.github.com/jhrcook/eb7b63cc57c683a6eb4986c4107a88ec
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
    }
    return(fxn)
}

# TODO make long-form scenarios, provinces, etc. work with facets & everything else
# TODO <1 default alpha in every graphing function
# TODO if function provided data frame instead of forecast object, call create_forecast() on it (with warning/message)