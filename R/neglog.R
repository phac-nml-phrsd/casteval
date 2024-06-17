#' Get negative-log score for forecast
#'
#' Given a forecast and set of observations,
#'  compute the negative-log score for every time point.
#' Uses a Kernel Density Estimation (KDE) to interpolate the density
#'  at the observation point.
#'
#' @param fcst The forecast (see `create_forecast()` output).
#' @param obs The observations data frame.
#'
#' @returns A data frame containing times, raw data, observations, and scores for those times.
#' @export
#' @autoglobal
#'
#' @examples
#' 
neglog <- function(fcst, obs) {
    # validate & filter
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # demand raw values
    if(! "raw" %in% fcst$data_types) {
        stop("raw data needed to calculate log score")
    }

    # remove NAs
    df$raw <- purrr::map(df$raw, ~ .x[!is.na(.x)])
    
    # prepare for join
    df |> 
      dplyr::select(time, raw) |>
      # remove rows with no data
      dplyr::filter(as.logical(purrr::map(raw, ~ length(.x) >= 2)))
    obs <- dplyr::filter(obs, !is.na(raw)) |> dplyr::rename(obs=raw)

    # join & check for overlap
    df <- dplyr::inner_join(df, obs, dplyr::join_by(time))
    if(nrow(df) == 0) {
        stop("observations don't overlap with forecast data at all")
    }

    df$score <- purrr::map2(df$obs, df$raw, scoringRules::logs_sample)
    
    df
}
