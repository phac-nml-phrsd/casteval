#' Intelligently graph observations
#'
#' Graph observation points. If provided with a forecast,
#'  the points can be color-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame.
#' @param fcst A forecast (see output of `create_forecast()`), or NULL.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_observations <- function(graph=NULL, obs, fcst=NULL) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    if(!is.null(fcst)) {

    }
}