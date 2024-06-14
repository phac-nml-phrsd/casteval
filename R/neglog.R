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
#' @returns The sum of scores across all time points for which there is sufficient data.
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

    # remove NAs & prepare for join
    df$raw <- purrr::map(df$raw, ~ .x[!is.na(.x)]) |> dplyr::select(time, raw) |>
      # remove rows with no data
      dplyr::filter(as.logical(purrr::map(raw, ~ length(.x) >= 2)))
    obs <- dplyr::filter(obs, !is.na(raw)) |> dplyr::rename(obs=raw)

    # join & check for overlap
    df <- dplyr::inner_join(df, obs, dplyr::join_by(time))
    if(nrow(df) == 0) {
        stop("observations don't overlap with forecast data at all")
    }

    # TODO catch cases where not enough data points in sample for KDE
    df$score <- purrr::map2(df$raw, df$obs, neglog_point)
    sum(df$score)
}

#' Get negative-log-frequency score for single time point
#'
#' Helper for neglog(). Given a set of predictions and a single observation,
#'  compute the negative-log score.
#'
#' @param samp A numeric vector. The values predicted by the model.
#' @param x The actual value observed.
#'
#' @returns A number representing the score.
#' @autoglobal
#'
#' @examples
#' #TODO
neglog_point <- function(samp, x) {
    dens <- density(samp, bw="nrd", from=x, to=x, n=1)$y[[1]]
    -log(dens)
}