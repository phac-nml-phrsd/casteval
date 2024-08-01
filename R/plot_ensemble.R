#' Plot ensemble of forecast realizations
#'
#' Given a forecast with raw realizations, generate a plot displaying all of them.
#' If the forecast provides simulation numbers (`sim` column), the ensemble of curves will be plotted.
#' Otherwise, the data will be plotted as points.
#'
#' @template plt
#' @template fcst
#' @template alpha
#' @template colour
#'
#' @returns A ggplot object.
#' @autoglobal
#' @export
#'
#' @examples
#' plot_ensemble(
#'   NULL,
#'   create_forecast(dplyr::tibble(time=rep(1:3,each=3), sim=rep(1:3,3), val=c(4:12))
#' ))
#' 
#' NULL |> plot_ensemble(create_forecast(data.frame(
#'   time=lubridate::as_datetime(c(0,20000,100000)),
#'   val=c(20,30,40)
#' )))
plot_ensemble <- function(plt=NULL, fcst, alpha=0.3, colour="black") {
    #TODO? make the fit data points instead of lines, or just don't plot the fit data
    validate_forecast(fcst)
    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    cols <- colnames(fcst$data)
    if(! "val" %in% cols) {
        stop("raw data needed to plot ensemble")
    }

    # if sim numbers present, plot ensemble curves
    if("sim" %in% cols) {
        return(plt + ggplot2::geom_line(ggplot2::aes(x=time, y=val, group=sim), colour=colour, alpha=alpha, data=fcst$data))
    }
    
    # if sim numbers absent, just plot points
    else {
        return(plt + ggplot2::geom_point(ggplot2::aes(x=time, y=val), alpha=alpha, colour=colour, data=fcst$data))
    }
}
