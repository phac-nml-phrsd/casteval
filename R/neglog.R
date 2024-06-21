#' Get negative-log score for forecast
#'
#' Given a forecast and set of observations,
#'  compute the negative-log score for every time point.
#' Uses a Kernel Density Estimation (KDE) to interpolate the density
#'  at the observation point.
#'
#' @param fcst The forecast (see `create_forecast()` output).
#' @param obs The observations data frame.
#' @param at A time (compatible with `fcst` and `obs`).
#'  If specified, the score for this time point will be returned.
#'  Mutually exclusive with `after`.
#' @param after A number. If specified, the score at
#'  time `fcst$forecast_time + after` will be returned.
#'  Mutually exclusive with `at`.
#' @template summarize
#'
#' @returns If `at` and `after` are both unspecified,
#'  a data frame containing times, raw data, observations, and scores for those times.
#'  Otherwise, the score at the time speficied by either `at` or `after`.
#' @export
#' @autoglobal
#'
#' @examples
#' # in the absence of `at` and `after`, return a data frame with a `score` column
#' neglog(
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(1:5, 1:5, 1:5))),
#'   data.frame(time=1:3, obs=c(-1, 2.5, 5))
#' )
#' 
#' # use `at` parameter to specify absolute times
#' neglog(
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(1:5, 1:5, 1:5)), forecast_time=1),
#'   data.frame(time=1:3, obs=c(-1, 2.5, 5)),
#'   at=2
#' )
#' 
#' # use `after` parameter to specify times relative to `forecast_time`
#' neglog(
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(1:5, 1:5, 1:5)), forecast_time=1),
#'   data.frame(time=1:3, obs=c(-1, 2.5, 5)),
#'   after=1
#' )
neglog <- function(fcst, obs, at=NULL, after=NULL, summarize=TRUE) {
    # validate & filter
    validate_fcst_obs_pair(fcst, obs)
    
    # TODO revisit this if we end up adding together scores/whatever
    #df <- filter_forecast_time(fcst$data, fcst$forecast_time)
    df <- fcst$data

    df <- remove_raw_NAs(df)
    # KDE requires at least 2 data points, so check for that after removing NAs
    if(any(as.logical(purrr::map(df$raw, ~ length(.x) < 2)))) {
        stop("at least 2 raw data points for each time point required to calculate KDE")
    }

    # join
    df <- df |> dplyr::select(time, raw) |> join_fcst_obs(obs)

    # score using KDE
    df$score <- as.numeric(purrr::map2(df$obs, df$raw, scoringRules::logs_sample))
    #TODO if calculating a KDE becomes a bottleneck (unlikely but possible), then only calculate the score for the one time point specified by at/after.

    # TODO clean this up once default values for at/after set
    if(!summarize || (is.null(at) && is.null(after))) { # return the whole data frame with the score column
        return(df)
    }
    
    if(!is.null(at) && !is.null(after)) { # mutually exclusive
        stop("`at` and `after` parameters cannot both be provided")
    }
    
    # only one provided, calculate time
    if(!is.null(at)) {
        validate_time(at, fcst)
        t <- at
    }
    if(!is.null(after)) {
        if(!is.numeric(after)) {
            stop("`after` not numeric")
        }
        if(is.null(fcst$forecast_time)) {
            stop("`after` cannot be used if `fcst$forecast_time` is NULL")
        }
        t <- fcst$forecast_time + after
    }

    get_time_point(df, t)[["score"]]
}
