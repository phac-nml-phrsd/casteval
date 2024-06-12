# generic score-calculating helpers and wrappers

#' Isolate projected values from fit values
#'
#' Removes rows from forecast data frame which should not be scored,
#' i.e. the data generated for dates/times prior to when the forecast was created.
#'
#' @param df The forecast data frame. See `create_forecast()` for details
#' @param forecast_time Either NULL or a time of the same type as the values in `dat$time`.
#'  If non-NULL, all rows with time prior to `forecast_time` will be removed from `dat`.
#'
#' @returns The filtered data frame (no change if `forecast_time` NULL)
#' @autoglobal
#'
#' @examples
#' # data.frame(time=5:10, raw=15:20)
#' casteval:::filter_forecast_time(data.frame(time=1:10, raw=11:20), 5)
filter_forecast_time <- function(df, forecast_time) {
    if(is.null(forecast_time)) {
        return(df)
    } else {
        return(df |> dplyr::filter(time >= forecast_time))
    }
}

#' Validate a forecast-observations pair
#'
#' Given a forecast and observations, verify that:
#' - forecast is valid
#' - observations are valid
#' - forecast time type matches observations time type
#'
#' @param fcst The forecast object (a named list, such as the output of `create_forecast()`)
#' @param obs The observations (a data frame)
#'
#' @returns NULL if valid. Error otherwise
#' @export
#' @autoglobal
#'
#' @examples
#' # compatible time types
#' casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, raw=11:20)),
#'   data.frame(time=101:110, raw=111:120)
#' )
#' 
#' # incompatible time types
#' casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, raw=11:20)),
#'   data.frame(time=lubridate::ymd("2024-01-01"), raw=5)
#' )
validate_fcst_obs_pair <- function(fcst, obs) {
    validate_forecast(fcst)
    obs_time_type <- get_obs_format(obs)
    if(obs_time_type != fcst$time_type) {
        stop("observations time type must match forecast time type")
    }
    NULL
}