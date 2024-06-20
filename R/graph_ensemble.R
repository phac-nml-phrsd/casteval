#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @template graph
#' @param fcst The forecast object (see the output of `create_forecast()`)
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' #TODO
graph_ensemble <- function(graph=NULL, fcst) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    validate_forecast(fcst)

    if(! "raw" %in% fcst$data_types) {
        stop("raw data needed to graph ensemble")
    }

    # convert to long format for easy ggplot interfacing
    df <- wide2long(fcst$data)

    graph + ggplot2::geom_line(ggplot2::aes(x=time, y=raw, group=realization), df)
}
