# TODO experiment with different color scales (viridis maybe?). Try to make the 2-12-50 example in the vignette more visible
# TODO redo format of forecasts with facets in mind
# TODO revisit plot_forecasts() after redoing formatting

# TODO somehow provide a score decorator which can convert accuracy() TRUE/FALSE to HIT/MISS or something else, and also modify the colormap. use this to set viridis for neglog() too

#' Plot a forecast
#'
#' Plot a forecast, along with corresponding observations, quantile intervals, etc.
#'
#' @template fcst
#' @param obs (Optional) An observations data frame.
#'  If provided, they will be overlaid over the forecast as points.
#' @param quant_pairs (Optional) A list of pairs of numbers between 0 and 100,
#' or a single pair of numbers between 0 and 100.
#' If provided, the score for each corresponding pairs of quantiles will be calculated.
#' If not provided, it will default to every symmetrical pair of quantiles that can be found in `fcst`,
#' ordered from widest to narrowest (e.x. the 25% and 75% quantiles are symmetrical).
#' 
#' The corresponding quantile intervals, if present, will be displayed in the resulting plot.
#' 
#' `quant_pairs` can be set to `list()` in order to display no quantile intervals.
#' 
#' @param score (Optional) A scoring function.
#'  The function will be used to score `obs` against the forecast.
#'  A scoring function should accept a forecast object, an observations data frame, as well as a `summarize` argument.
#'  See `?accuracy`, `?neglog` for examples.
#'  See `vignette(topic='casteval', package='casteval')` for details.
#'
#' @returns A ggplot2 object
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=1:3,
#'   vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
#' ))
#' obs <- data.frame(time=1:3, val_obs=5:7)
#' 
#' # plot forecast
#' plot_forecast(fc)
#' 
#' # plot forecast and observations
#' plot_forecast(fc, obs)
#' 
#' # plot forecast and confidence interval(s)
#' plot_forecast(fc, confs=c(50,95))
#' 
#' # highlight the observations inside the 95% confidence interval
#' plot_forecast(fc, obs, confs=95, score=\(...) accuracy(..., interval=c(2.5, 97.5)))
#' 
#' # show the negative log score of each observation
#' plot_forecast(fc, obs, score=neglog)
plot_forecast <- function(fcst, obs=NULL, quant_pairs=NULL, score=NULL) {
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

    # plot everything according to the parameters
    plt <- NULL
    if("val" %in% colnames(fcst$data)) {
        plt <- plt |> plot_ensemble(fcst)
    }

    # TODO plot mean and/or quantiles if present
    # TODO parameters/flags for keeping/discarding fit data, observations data that doesn't correpsond to forecast data, etc.

    # pass allow_empty=TRUE so that missing quantile intervals is allowed
    quant_pairs <- parse_quant_pairs(quant_pairs, fcst$data, allow_empty=TRUE)

    # plot quant intervals if present
    if(length(quant_pairs) > 0) {
        plt <- plt |> plot_quant_intervals(fcst, quant_pairs)
    }

    # plot observatinos if present
    if(!is.null(obs)) {
        plt <- plt |> plot_observations(obs)
    }

    # error if we didn't end up plotting anything
    if(is.null(plt)) {
        # could be turned into a warning
        stop("nothing was plotted. Please specify raw data, quantiles, and/or observations to be plotted.")
    }

    ## set labels and secondary features

    # mark the forecast time if provided
    if(!is.null(fcst$forecast_time)) {
        plt <- plt + ggplot2::geom_vline(alpha=0.2, xintercept=fcst$forecast_time)
    }

    # make the title the name if provided
    if(!is.null(fcst$name)) {
        plt <- plt + ggplot2::labs(title=fcst$name)
    }

    # name the axes
    plt <- plt + ggplot2::xlab("time") + ggplot2::ylab("value")

    # make x axis integers only
    plt <- plt + ggplot2::scale_x_continuous(breaks=integer_breaks())
    #plt <- plt + ggplot2::scale_y_continuous(breaks=integer_breaks())

    plt
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
# TODO if function provided data frame instead of forecast object, call create_forecast() on it (with warning/message)