#' Get negative-log score for forecast
#'
#' Given a forecast and set of observations,
#'  compute the negative-log score for every time point.
#' Uses a Kernel Density Estimation (KDE) to interpolate the density
#'  at the observation point.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @param at (Optional) A time (compatible with `fcst` and `obs`).
#'  If specified, the score for this time point will be returned.
#'  Mutually exclusive with `after`.
#' @param after (Optional) A number. If specified, the score at
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
#' df <- data.frame(time=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), val=c(1:5, 1:5, 1:5))
#' # in the absence of `at` and `after`, return a data frame with a `score` column
#' neglog(
#'   create_forecast(df),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5))
#' )
#' 
#' # use `at` parameter to specify absolute times
#' neglog(
#'   create_forecast(df, forecast_time=1),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
#'   at=2
#' )
#' 
#' # use `after` parameter to specify times relative to `forecast_time`
#' neglog(
#'   create_forecast(df, forecast_time=1),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
#'   after=1
#' )
neglog <- function(fcst, obs, at=NULL, after=NULL, summarize=TRUE) {
    # validate & filter
    validate_fcst_obs_pair(fcst, obs)
    if(!"val" %in% colnames(fcst$data)) {
        stop("neglog() requires unsummarized forecast data (`val`)")
    }
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # join
    df <- df |> dplyr::select(time, val) |> join_fcst_obs(obs)

    # group by time
    df <- df |> dplyr::group_by(time)

    # check if any time points
    not_enough_points <- df |> filter(dplyr::n() < 2)
    if(nrow(not_enough_points) > 1) {
        tm <- not_enough_points$time[[1]]
        stop(glue::glue("not enough data points at time {tm} (at least 2 required to calculate KDE)"))
    }

    df <- df |> dplyr::summarize(score = scoringRules::logs_sample(dplyr::filter(obs, time=time)$val_obs[[1]], val))
    #TODO if calculating a KDE becomes a bottleneck (unlikely but possible), then only calculate the score for the one time point specified by at/after.

    # TODO clean this up once default values for at/after set
    if(!summarize || (is.null(at) && is.null(after))) { # return the whole data frame with the score column
        return(dplyr::select(df, time, obs, score))
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
