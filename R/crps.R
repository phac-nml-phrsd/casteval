# Continuous Ranked Probability Score


#' Compute Continuous Ranked Probability Score for forecast
#'
#' Given a forecast and set of observations, compute the
#' Continuous Ranked Probability Score (CRPS) for every time point.
#' The CRPS for a given distribution `f` and observation `y`,
#' let `F` be the CDF of `f`. Then the CRPS is the integral of the square
#' of `F(x) - H(x - y)`, where `H` is the Heaviside function.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @template at_after
#' @template summarize
#'
#' @template at_after_returns
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
crps <- function(fcst, obs, at=NULL, after=NULL, summarize=TRUE) {

}