# TODO experiment with different color scales (viridis maybe?). Try to make the 2-12-50 example in the vignette more visible
# TODO redo format of forecasts with facets in mind
# TODO revisit plot_forecasts() after redoing formatting

# TODO somehow provide a score decorator which can convert accuracy() TRUE/FALSE to HIT/MISS or something else, and also modify the colormap. use this to set viridis for log_score() too

#' Plot a forecast
#'
#' Plot a forecast, along with corresponding observations, quantile intervals, etc.
#'
#' @template fcst
#' @param obs (Optional) An observations data frame.
#'  If provided, they will be overlaid over the forecast as points.
#' @param quant_intervals (Optional) A list of pairs of numbers between 0 and 100,
#' or a single pair of numbers between 0 and 100.
#' If provided, the score for each corresponding pairs of quantiles will be calculated.
#' If not provided, it will default to every symmetrical pair of quantiles that can be found in `fcst`,
#' ordered from widest to narrowest (e.x. the 25% and 75% quantiles are symmetrical).
#' 
#' The corresponding quantile intervals, if present, will be displayed in the resulting plot.
#' 
#' `quant_intervals` can be set to `list()` in order to display no quantile intervals.
#' 
#' @param invert_scale (Optional) (Optional) a boolean.
#' If `TRUE`, the color scale for scoring will be inverted.
#' This is useful for scores where smaller values are better, e.x. CRPS.
#' @template score
#' @param ... Additional parameters to be passed to `score`.
#' Note that `summarize` should not be one of them,
#' since `casteval` already passes that to `score`.
#' 
#' @returns A ggplot object
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
#' # plot forecast and quantile interval(s)
#' plot_forecast(fc, quant_intervals=list(c(25,75), c(2.5,97.5)))
#' 
#' # highlight the observations inside the quantile interval
#' plot_forecast(fc, obs, quant_intervals=c(2.5,97.5), score=make_accuracy(c(2.5,97.5)))
#' 
#' # show the log score of each observation
#' plot_forecast(fc, obs, score=log_score)
plot_forecast <- function(fcst, obs=NULL, quant_intervals=NULL, invert_scale=FALSE, score=NULL, ...) {
    # validate forecast and/or observations
    if(is.null(obs)) {
        validate_forecast(fcst)
    } else {
        validate_fcst_obs_pair(fcst, obs)
    }

    # score if necessary
    if(!is.null(score) && is.null(obs)) {
        # could be converted to warning
        stop("scoring function provided without observations")
    }

    # plot everything according to the parameters
    plt <- NULL
    if("val" %in% colnames(fcst$data)) {
        plt <- plt |> plot_ensemble(fcst)
    }

    # TODO plot mean and/or quantiles if present
    # TODO parameters/flags for keeping/discarding fit data, observations data that doesn't correpsond to forecast data, etc.

    # pass allow_empty=TRUE so that missing quantile intervals is allowed
    # the param is named "quant_intervals" to avoid conflicting with the ... params
    quant_intervals <- parse_quant_pairs(quant_intervals, fcst$data, allow_empty=TRUE)

    # plot quant intervals if present
    if(length(quant_intervals) > 0) {
        plt <- plt |> plot_quant_intervals(fcst, quant_intervals)
    }

    # plot observatinos if present
    if(!is.null(obs)) {
        if(is.null(score)) {
            plt <- plt |> plot_observations(obs)
        } else {
            plt <- plt |> plot_obs_score(fcst, obs, invert_scale=invert_scale, score=score, ...)
        }
    }

    # plot mean and median if present
    if("val_mean" %in% colnames(fcst$data)) {
        plt <- plt |> plot_mean(fcst)
    }

    if("val_q50" %in% colnames(fcst$data)) {
        plt <- plt |> plot_quantiles(fcst, quants=50)
    }

    # # error if we didn't end up plotting anything
    # if(is.null(plt)) {
    #     # could be turned into a warning
    #     stop("nothing was plotted. Please specify raw data, quantiles, and/or observations to be plotted.")
    # }

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
#' A modified version of `scales::pretty_breaks()` for only integer-valued ticks.
#' 
#' @param n Number of breaks.
#' @param ... other arguments passed to `pretty()`
#' 
#' @source <https://gist.github.com/jhrcook/eb7b63cc57c683a6eb4986c4107a88ec>
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