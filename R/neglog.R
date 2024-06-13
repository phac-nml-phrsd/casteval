#' Get negative-log-frequency score for forecast
#'
#' Given a forecast and set of observations,
#'  compute the negative-log score for every time point.
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
#' @param pred A numeric vector. The values predicted by the model.
#' @param actual The actual value observed.
#'
#' @returns A number representing the score.
#' @autoglobal
#'
#' @examples
#' #TODO
neglog_point <- function(pred, actual) {

}