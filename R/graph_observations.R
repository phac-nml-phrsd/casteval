#' Intelligently graph observations
#'
#' Graph observation points. If provided with a forecast,
#'  the points can be color-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame.
#' @param scores A data frame containing a `time` column and a `score` column.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_observations <- function(graph=NULL, obs, scores=NULL) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    #if()

}