#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @template graph
#' @template fcst
#' @param alpha (Optional) The alpha value to be passed to `ggplot2::geom_line()`.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' casteval:::graph_ensemble(
#'   NULL,
#'   create_forecast(dplyr::tibble(time=1:3, val=c(4:12))
#' ))
#' 
#' NULL |> casteval:::graph_ensemble(create_forecast(data.frame(
#'   time=lubridate::as_datetime(c(0,20000,100000)),
#'   val=c(20,30,40)
#' )))
graph_ensemble <- function(graph=NULL, fcst, alpha=0.3) {
    #TODO? make the fit data points instead of lines, or just don't plot the fit data
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    cols <- colnames(fcst$data)
    if(! "val" %in% cols) {
        stop("raw data needed to graph ensemble")
    }
    if(! "sim" %in% cols) {
        stop("simulation numbers (`sim` column) required to graph ensemble")
    }

    graph + ggplot2::geom_line(ggplot2::aes(x=time, y=val, group=sim), alpha=alpha, fcst$data)
}
