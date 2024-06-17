# generic score-calculating helpers and wrappers
# the actual scoring functions can be found in `accuracy.R`, `neglog.R`, etc.

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
#' 
#' # unchanged
#' casteval:::filter_forecast_time(data.frame(time=1:10, raw=11:20), NULL)
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
#' try(casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, raw=11:20)),
#'   data.frame(time=lubridate::ymd("2024-01-01"), raw=5)
#' ))
validate_fcst_obs_pair <- function(fcst, obs) {
    validate_forecast(fcst)
    obs_time_type <- get_obs_format(obs)
    if(obs_time_type != fcst$time_type) {
        stop("observations time type must match forecast time type")
    }
    NULL
}


#' Remove NA values from raw data
#'
#' Removes NA values from raw data in a forecast data frame.
#' Raises an error if this leaves any rows empty.
#'
#' @param df The forecast data frame.
#'  It should contain a `raw` column of numeric vectors.
#'
#' @returns The data frame with raw NA values removed from the vectors.
#' @autoglobal
#'
#' @examples
#' # TODO
remove_raw_NAs <- function(df) {
    if(! "raw" %in% colnames(df)) {
        stop("data frame does not contain `raw` column")
    }

    df$raw <- purrr::map(df$raw, ~ .x[!is.na(.x)])
    if(any(as.logical(purrr::map(df$raw, ~ length(.x) == 0)))) {
        stop("data frame contains row with no raw data")
    }
    df
}


#' Join a forecast and observations into a single data frame
#'
#' Add an observations column to the forecast data frame containing
#'  observations for each time point.
#'
#' @param df The forecast data frame.
#' @param obs The observations data frame.
#' @param na.rm A boolean. If FALSE, then an error will be raised when
#'  observations are missing for any forecast time points.
#'  If TRUE, any time points with missing observations will be removed from the data frame.
#'
#' @returns The forecast data frame with an additional `obs` column containing observations.
#' @autoglobal
#'
#' @examples
#' #TODO
join_fcst_obs <- function(df, obs, na.rm=FALSE) {
    # rename obs `raw` to `obs` and check that no collisions will occur
    obs <- dplyr::rename(obs, obs=raw)
    if("obs" %in% colnames(df)) {
        stop("`obs` column already present in forecast data frame")
    }
    
    # join, using NAs for wherever observations are missing
    df <- dplyr::left_join(df, obs, dplyr::join_by(time))

    # check for NAs in the `obs` column
    # (whether they are from NAs in the `obs` data frame or due to left_join() does not matter)
    if(any(as.logical(purrr::map(df$obs, is.na)))) {
        if(na.rm) { # remove the rows with NA obs
            df <- dplyr::filter(df, !is.na(obs))
        } else { # raise error
            stop("missing observations for some forecast time points")
        }
    }

    df
}