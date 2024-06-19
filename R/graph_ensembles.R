#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @param fcst The forecast object (see the output of `create_forecast()`)
#'
#' @returns A ggplot graph
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
graph_ensembles <- function(fcst) {
    validate_forecast(fcst)
}