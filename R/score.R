# generic score-calculating helpers and wrappers
# the actual scoring functions can be found in `accuracy.R`, `neglog.R`, etc.

# TODO score() functional (accepts 1 or more forecasts and a scoring function)

# TODO for all additional required arguments in scoring functions, set sane defaults and give a warning if the defaults are used.
# TODO? move forecast_time filtering out of the scoring functions and into a wrapper

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
#' # data.frame(time=5:10, val=15:20)
#' casteval:::filter_forecast_time(data.frame(time=1:10, val=11:20), 5)
#' 
#' # unchanged
#' casteval:::filter_forecast_time(data.frame(time=1:10, val=11:20), NULL)
filter_forecast_time <- function(df, forecast_time) {
    if(is.null(forecast_time)) {
        return(df)
    } else {
        return(df |> dplyr::filter(time >= forecast_time))
    }
}


#' Join a forecast and observations into a single data frame
#'
#' Add an observations column to the forecast data frame containing
#'  observations for each time point.
#'
#' @param df The forecast data frame.
#' @param obs The observations data frame.
#'
#' @returns The forecast data frame with an additional `obs` column containing observations.
#' @autoglobal
#'
#' @examples
#' # data.frame(time=1:3, raw=4:6, obs=8:10)
#' casteval:::join_fcst_obs(data.frame(time=1:3, val=4:6), data.frame(time=0:4, val_obs=7:11))
#' 
#' # remove rows with missing observations
#' # data.frame(time=3, val_q50=6, val_obs=7)
#' casteval:::join_fcst_obs(
#'   data.frame(time=1:3, val_q50=4:6),
#'   data.frame(time=2:3, val_obs=c(NA,7))
#' )
#' 
#' # default behaviour is to error if observations are missing
#' try(casteval:::join_fcst_obs(
#'   data.frame(time=1:3, val_q50=4:6),
#'   data.frame(time=2:3, val_obs=c(NA,7))
#' ))
join_fcst_obs <- function(df, obs) {
    # this function does very little input validation because
    # it is meant to be used inside functions like `accuracy()` and `neglog()`,
    # where the forecast and observations are already validated.

    # check that no collisions will occur
    if("val_obs" %in% colnames(df)) {
        stop("`val_obs` column already present in forecast data frame")
    }

    # filter out NAs
    obs <- obs |> dplyr::filter(is.na(val_obs))

    df <- dplyr::inner_join(df, obs, dplyr::join_by(time))

    if(nrow(df) == 0) {
        stop("forecast and observations data do not share any time points")
    }

    df
}


#' Get a row of a date frame for a given time
#'
#' Given a forecast/observations data frame and a time,
#'  isolate the row with the given time.
#'
#' @param df A data frame with a `time` column.
#' @param t A time.
#'
#' @returns A data frame with one row, where the time equals `t`.
#'  Raises error if there isn't exactly one row which satisfies this.
#' @autoglobal
#'
#' @examples
#' # no rows with time==4
#' try(casteval:::get_time_point(data.frame(time=1:3,raw=4:6), 4))
#' 
#' # 2 rows with time==2
#' try(casteval:::get_time_point(data.frame(time=c(1,2,2,3), raw=4:7), 2))
#' 
#' # data.frame(time=2, raw=5, mean=8)
#' casteval:::get_time_point(data.frame(time=1:3, raw=4:6, mean=7:9), 2)
get_time_point <- function(df, t) {
    #TODO add a flag for allowing multiple matches
    df <- dplyr::filter(df, time == t)

    if(nrow(df) == 0) {
        stop("no rows in data frame with given time")
    } else if(nrow(df) > 1) {
        stop("multiple rows in data frame with given time")
    }

    df
}