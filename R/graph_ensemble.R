#' Graph ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a graph displaying all of them.
#'
#' @param fcst The forecast object (see the output of `create_forecast()`)
#'
#' @returns A ggplot object.
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
graph_ensemble <- function(fcst) {
    validate_forecast(fcst)

    if(! "raw" %in% fcst$data_types) {
        stop("raw data needed to graph ensemble")
    }

    # convert to long format for easy ggplot interfacing
    df <- wide2long(fcst$data)

    ggplot2::ggplot(df, ggplot2::aes(x=time, y=raw, group=realization)) + ggplot2::geom_line()
}