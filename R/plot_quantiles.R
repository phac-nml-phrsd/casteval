#' Plot forecast quantiles
#'
#' Given forecast data, plot the requested quantiles as lines.
#'
#' @template plt
#' @template fcst
#' @param quants (Optional) A vector of numbers between 0 and 100, the quantiles to be graphed.
#' If not provided, the quantile columns in `fcst$data` will be used.
#' @template alpha
#' @template colour
#'
#' @returns A ggplot object
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=1:10,
#'   vals=list(
#'     c(1,2,3,5,4,5,4,6,6,5),
#'     c(1,3,5,4,6,5,7,9,8,8),
#'     c(1,4,3,4,5,6,5,3,2,2),
#'     c(1,2,4,5,7,8,7,9,10,9)
#'   )
#' ))
#' 
#' NULL |> plot_ensemble(fc) |> plot_quantiles(fc, quants=c(2.5, 50, 97.5))
#' 
#' fc2 <- create_forecast(data.frame(
#'   time=1:10,
#'   val_q2.5=1:10,
#'   val_q25=2:11,
#'   val_q50=3:12,
#'   val_q75=4:13,
#'   val_q97.5=5:14
#' ))
#' 
#' NULL |> plot_quantiles(fc2)
#' NULL |> plot_quantiles(fc2, quants=c(2.5,50,97.5))
plot_quantiles <- function(plt=NULL, fcst, quants=NULL, alpha=1, colour="orange") {
    # validate
    validate_forecast(fcst)

    # get quant defaults or validate provided quants
    if(is.null(quants)) {
        quants <- get_quant_percentages(fcst$data)
        if(length(quants) == 0) {
            stop("no quantile columns in forecast data and `quants` not provided")
        }
    } else {
        quants |> purrr::walk(\(quant) validate_quant(quant))
    }
    
    if(any(duplicated(quants))) {
        stop("`quants` contains duplicate quantiles")
    }

    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    # get the quantiles in long form in a data frame
    # with `time`, `quantile`, and `value` columns
    quant_data <- quants |>
        purrr::imap(\(q, i) get_quantile(fcst$data, q) |> dplyr::mutate(value=quant, quant=NULL, linetype=glue::glue("{q}% quantile"))) |>
        dplyr::bind_rows() |>
        dplyr::mutate(quantile=as.factor(linetype))

    # plot
    plt + ggplot2::geom_line(ggplot2::aes(x=time, y=value, linetype=linetype), data=quant_data, alpha=alpha, colour=colour)

    # using `{ggnewscale}` to make a second color scale is probably a bad idea because it impedes
    # customization by the user, and can be visually confusing
    # so instead we just use linetype above
    # plt + ggnewscale::new_scale_color() +
    #     ggplot2::geom_line(ggplot2::aes(x=time, y=value, color=quantile), data=quant_data, alpha=alpha)
}