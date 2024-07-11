# generic score-calculating helpers and wrappers
# the actual scoring functions can be found in `accuracy.R`, `neglog.R`, etc.

# TODO score() functional (accepts 1 or more forecasts and a scoring function)

# TODO for all additional required arguments in scoring functions, set sane defaults and give a warning if the defaults are used.
# TODO move forecast_time filtering out of the scoring functions and into a wrapper?

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
#' # dplyr::tibble(time=1:3, raw=list(1, 1:3, 4))
#' casteval:::remove_raw_NAs(dplyr::tibble(time=1:3, raw=list(c(NA, 1), c(1, 2, 3), c(NA, NA, 4))))
#' 
#' # error if we end up with any empty rows
#' try(casteval:::remove_raw_NAs(dplyr::tibble(time=4:5, raw=list(c(1,NA), c(NA,NA)))))
remove_raw_NAs <- function(df) {
    if(! "raw" %in% colnames(df)) {
        stop("forecast data frame does not contain `raw` column")
    }

    df$raw <- purrr::map(df$raw, ~ .x[!is.na(.x)])
    if(any(as.logical(purrr::map(df$raw, ~ length(.x) == 0)))) {
        stop("forecast data frame contains row with no raw data")
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
#' @param na.rm A boolean. Defaults to FALSE. If FALSE, then an error will be raised when
#'  observations are missing for any forecast time points.
#'  If TRUE, any time points with missing observations will be removed from the data frame.
#'
#' @returns The forecast data frame with an additional `obs` column containing observations.
#' @autoglobal
#'
#' @examples
#' # data.frame(time=1:3, raw=4:6, obs=8:10)
#' casteval:::join_fcst_obs(data.frame(time=1:3, raw=4:6), data.frame(time=0:4, obs=7:11))
#' 
#' # remove rows with missing observations 
#' # data.frame(time=3, quant_50=6, obs=7)
#' casteval:::join_fcst_obs(
#'   data.frame(time=1:3, quant_50=4:6),
#'   data.frame(time=2:3, obs=c(NA,7)),
#'   na.rm=TRUE
#' )
#' 
#' # default behaviour is to error if observations are missing
#' try(casteval:::join_fcst_obs(
#'   data.frame(time=1:3, quant_50=4:6),
#'   data.frame(time=2:3, obs=c(NA,7))
#' ))
join_fcst_obs <- function(df, obs, na.rm=FALSE) {
    # this function does very little input validation because
    # it is meant to be used inside functions like `accuracy()` and `neglog()`,
    # where the forecast and observations are already validated.

    # check that no collisions will occur
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
            if(nrow(df) == 0) {
                stop("no rows remain after removing NA observations")
            }
        } else { # raise error
            stop("missing observations for some forecast time points")
        }
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