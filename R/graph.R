# TODO legends, axes, plot titles, scales. some graphs will have multiple legends so that has to be taken into consideration.

#' Graph one or more forecasts
#'
#' Graph one or more forecasts, along with corresponding observations, quantiles, etc.
#'
#' @param fcsts A single forecast object, or a list of forecast objects (see output of `create_forecast()`).
#' @param obs An observations data frame.
#'  If provided, they will be overlaid over the forecast(s) as points.
#' @param confs NULL, or a vector of numbers from 0 to 100.
#'  The corresponding confidence intervals will be displayed in the resulting graph(s)
#' @param score NULL, or a scoring function.
#'  The function will be used to score the `obs` against the forecast(s).
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' 
graph_forecasts <- function(fcsts, obs=NULL, confs=NULL, score=NULL) {

}