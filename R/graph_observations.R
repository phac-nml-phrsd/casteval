#' Intelligently graph observations
#'
#' Graph observation points. If provided with a forecast,
#'  the points can be color-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame. If it contains a `score` column,
#'  the observation points will be color-coded according to the scores.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_observations <- function(graph=NULL, obs) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) {return(
        graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs, color=score), obs)
    )} else {return(
        graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs), obs)
    )}
}