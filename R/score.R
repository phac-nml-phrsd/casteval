# function names in this file are tentative

#' Isolate projected values from fit values
#'
#' Removes rows from forecast data frame which should not be scored,
#' i.e. the data generated for dates/times prior to when the forecast was created.
#'
#' @param dat The forecast data frame. See `create_forecast()` for details
#' @param forecast_time Either NULL or a time of the same type as the values in `dat$time`.
#'  If non-NULL, all rows with time prior to `forecast_time` will be removed from `dat`.
#'
#' @returns The filtered data frame (no change if `forecast_time` NULL)
#' @autoglobal
#'
#' @examples
#' #TODO
filter_forecast_time(df, forecast_time) {
    if(is.null(forecast_time)) {
        return(df)
    } else {
        return(df |> dplyr::filter(time > forecast_time))
    }
}

#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# within confidence interval / # total) of the forecast.
#'
#' @param fcst The forecast (see `create_forecast()` output).
#'  If quantiles are provided, they will be used to compute the accuracy.
#'  If raw data is provided, quantiles will be calculated according to the `interval` parameter.
#' @param obs The observations data frame.
#' @param interval Either NULL or a vector of two numbers from 0 to 100.
#'  If `fcst` contains quantile data then the corresponding quantile columns will be
#'  used as the confidence interval.
#'  If `fcst` contains raw data then the corresponding quantiles will be calculated and
#'  used as a confidence interval.
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
accuracy <- function(fcst, obs, interval=NULL) {
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if("raw" %in% fcst$data_types) {

    } else if("quant" %in% fcst$data_types) {
        quants <- get_quantiles(df)

        # can't do anything with a single quantile
        if(length(quants) < 2) {
            stop("2 or more quantiles required to calculate accuracy")
        }

        if(is.null(interval)) {
            # if quantile columns are provided but `quants` is NULL,
            # we select the two outermost quantiles provided,
            # and require that they be equidistant from the median.
            # e.x. 25% to 75% is acceptable, but not 25% to 60%
            low <- min(quants)
            high <- max(quants)
            # use all.equal() to deal with floating point errors
            if(all.equal(50-low, high-50)) {
                stop("outermost quantiles must be equidistant from 50th percentile")
            }

            lowname <- quant_name(low)
            highname <- quant_name(high)
        } else {
            

            lowname <- quant_name(low)
            highname <- quant_name(high)
            # confirm that the corresponding columns exist
            if(! quant_name(low) %in% colnames(df)) {
                stop()
            }
        }

        df |>
            # isolate/rename the time and relevant quantile columns
            dplyr::select(time, low=quant_name(low), high=quant_name(high))
            # join observations by date
    } else {
        stop("`raw` or `quant_*` columns required to calculate accuracy")
    }
}

#' Validate quantile interval vector
#'
#' Helper function for accuracy(). Performs input validation on its `interval` parameter.
#'
#' @param interval Same as the `interval` parameter passed to accuracy()
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' #TBD
validate_interval <- function(interval) {
    if(!is.numeric(interval)) {
        stop("`interval` must be either NULL or vector of 2 numbers")
    }

    if(length(interval) != 2) {
        stop("`interval` vector must have length 2")
    }

    low <- interval[[1]]
    high <- interval[[2]]

    if(low >= high) {
        stop("`interval[[1]]` must be less than `interval[[2]]`")
    }

    if(low < 0 || low > 100 | high < 0 || high > 100) {
        stop("`interval[[1]]` and `interval[[2]]` must be between 0 and 100, inclusive")
    }

    NULL
}