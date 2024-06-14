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
#' @returns A data frame with a `time` column and a `score` column,
#'  containing the scores for every corresponding time point.
#' @export
#' @autoglobal
#'
#' @examples
#' 
neglog <- function(fcst, obs) {

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
    density(samp, bw="nrd", from=x, to=x, n=1)$y[[1]]
}