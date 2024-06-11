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
#'  If raw data is provided, quantiles will be calculated according to the `quant` parameter.
#' @param obs The observations data frame.
#' @param quants Either NULL or a vector of two numbers from 0 to 100.
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
accuracy <- function(fcst, obs, quants=NULL) {
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if("quant" %in% fcst$data_types) {
        if(is.null(quants)) {
            # if quantile columns are provided but `quants` is NULL,
            # we select the two outermost quantiles provided,
            # and require that they be equidistant from the median.
            # e.x. 25% to 75% is acceptable, but not 25% to 60%
        } else {

        }
    } else {

    }
}