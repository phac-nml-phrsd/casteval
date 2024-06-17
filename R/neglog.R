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
#' @param after An integer. If specified, the score at
#'  time `fcst$forecast_time + after` will be returned.
#'  Mutually exclusive with `at`.
#'
#' @returns A data frame containing times, raw data, observations, and scores for those times.
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
neglog <- function(fcst, obs) {
    # TODO figure parameters to specify a single time point to return the score for.

    # validate & filter
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    df <- remove_raw_NAs(df)
    # KDE requires at least 2 data points, so check for that after removing NAs
    if(any(as.logical(purrr::map(df$raw, ~ length(.x) < 2)))) {
        stop("at least 2 raw data points for each time point required to calculate KDE")
    }

    # join
    df <- df |> dplyr::select(time, raw) |> join_fcst_obs(obs)

    # score using KDE
    df$score <- purrr::map2(df$obs, df$raw, scoringRules::logs_sample)
    
    df
}
