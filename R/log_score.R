#' Compute logarithmic score for forecast
#'
#' Given a forecast and set of observations,
#'  compute the log score for every time point.
#' Uses a Kernel Density Estimation (KDE) to interpolate the density
#'  at the observation point.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @template summarize
#' @template at_after
#' @param bw (Optional) The bandwidth for calculating the Kernel Density Estimation (see `?scoringRules::logs_sample`).
#' If not provided, a bandwidth will automatically be calculated by `scoringRules::logs_sample()`.
#'
#' @template at_after_returns
#' @export
#' @autoglobal
#'
#' @examples
#' df <- data.frame(time=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3), val=c(1:5, 1:5, 1:5))
#' # return a data frame
#' log_score(
#'   create_forecast(df),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
#'   summarize=FALSE
#' )
#' 
#' # use `at` parameter to specify absolute times
#' log_score(
#'   create_forecast(df, forecast_time=1),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
#'   at=2
#' )
#' 
#' # use `after` parameter to specify times relative to `forecast_time`
#' log_score(
#'   create_forecast(df, forecast_time=1),
#'   data.frame(time=1:3, val_obs=c(-1, 2.5, 5)),
#'   after=1
#' )
log_score <- function(fcst, obs, summarize=TRUE, at=NULL, after=NULL, bw=NULL) {
    # validate bw (mostly just make sure it isn't a vector with length >1)
    if(!(is.null(bw) || (is.numeric(bw) && length(bw)==1))) {
        stop("`bw` must be either NULL or a single number")
    }

    # validate & filter
    validate_fcst_obs_pair(fcst, obs)
    if(!"val" %in% colnames(fcst$data)) {
        stop("log_score() requires raw forecast data (`val`)")
    }
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # join
    df <- df |> dplyr::select(time, val) |> join_fcst_obs(obs)

    # group by time
    df <- df |> dplyr::group_by(time)

    # check if any time points
    not_enough_points <- df |> dplyr::filter(dplyr::n() < 2)
    if(nrow(not_enough_points) > 0) {
        tm <- not_enough_points$time[[1]]
        stop(glue::glue("not enough data points at time {tm} (at least 2 required to calculate KDE)"))
    }

    # scoringRules::logs_sample computes the negative log score. we negate it again to compute the log score
    df <- df |> dplyr::summarize(score = -scoringRules::logs_sample(val_obs[[1]], val, bw=bw), val_obs=val_obs[[1]])
    #TODO if calculating a KDE becomes a bottleneck (unlikely but possible), then only calculate the score for the one time point specified by at/after.

    if(!summarize) { # return the whole data frame with the score column
        return(dplyr::select(df, time, val_obs, score))
    }
    
    t <- calc_specified_time(fcst, at, after)

    df <- df |> dplyr::filter(time==t)

    if(nrow(df) == 0) {
        stop(glue::glue("score was not calculated for time {t}"))
    }

    df$score[[1]]
}


#' `log_score()` function factory
#'
#' Create a function wrapping `log_score()` with specified parameters.
#'
#' @param at See `?log_score`
#' @param after See `?log_score`
#' @param bw See `?log_score`
#'
#' @returns A function wrapping `log_score()` with the given parameters
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
make_log_score <- function(at=NULL, after=NULL, bw=NULL) {
    function(...) {
        log_score(..., at=at, after=after, bw=bw)
    }
}
