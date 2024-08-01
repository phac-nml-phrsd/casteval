#' Plot forecast mean
#'
#' Given forecast data, plot the mean
#'
#' @template plt
#' @template fcst
#' @template alpha
#' @template colour
#'
#' @returns A ggplot object
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(
#'   data.frame(time=1:5, val_mean=6:10)
#' )
#' 
#' NULL |> plot_mean(fc, colour="red", alpha=0.5)
#' 
#' fc2 <- create_forecast(
#'   data.frame(time=rep(1:5, each=3), val=c(1,2,3, 4,7,5, 10,11,10, 0,0,0, 1,2,2))
#' )
#' 
#' NULL |> plot_mean(fc2)
plot_mean <- function(plt=NULL, fcst, alpha=1, colour="green") {
    validate_forecast(fcst)
    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    if(!"val_mean" %in% colnames(fcst$data)) {
        if(!"val" %in% colnames(fcst$data)) {
            stop("mean or raw data required to plot mean")
        }
        dat <- fcst$data |> dplyr::group_by(time) |> dplyr::summarize(val_mean=mean(val))
    }
    else {
        dat <- fcst$data
    }
    plt + ggplot2::geom_line(ggplot2::aes(x=time, y=val_mean), data=dat, alpha=alpha, colour=colour)
}