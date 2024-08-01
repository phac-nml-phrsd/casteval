# #' Plot forecast quantiles
# #'
# #' Plot lines indicating quantiles of a forecast.
# #' Deprecated until I can find a way to make the legends behave.
# #'
# #' @template plt
# #' @template fcst
# #' @param quants A numeric vector containing the quantiles to be plotted, as percentages.
# #'  If `NULL`, all quantiles present in the forecast data frame will be plotted.
# #'
# #' @returns A ggplot object.
# #' @autoglobal
# #'
# #' @examples
# #' fc1 <- create_forecast(dplyr::tibble(
# #' time=1:3,
# #' raw=list(0:5, 5:10, 10:15)
# #' ))
# #' 
# #' NULL |> casteval:::plot_ensemble(fc1) |> casteval:::plot_quantiles(fc1, c(2.5, 25,51,75))
# plot_quantiles <- function(plt=NULL, fcst, quants=NULL) {
#     # TODO make the quantile line colors better. maybe even symmetrical
#     validate_forecast(fcst)
#     if(is.null(plt)) {
#         plt <- ggplot2::ggplot()
#     }
    
#     # if not specified, use all present quantile columns
#     if(is.null(quants)) {
#         quants <- get_quant_percentages(fcst$data)
#     }

#     if(length(quants) == 0) {
#         # could possibly be made a warning
#         # TODO? set defaults for quantiles
#         stop("no quantiles specified and none found in data frame")
#     }

#     # TODO maybe make this map->reduce instead of map->wide->long
#     # compile quantile data into a data frame
#     quant_data <- quants |>
#         # get all specified quantiles from forecast as a list of vectors
#         purrr::map(\(x) get_quantile(fcst$data, x)) |>
#         # name according to quantile
#         stats::setNames(quants) |>
#         # convert to data frame
#         dplyr::as_tibble() |>
#         # append time column
#         dplyr::mutate(time=fcst$data$time) |>
#         # convert to long format for ggplot2
#         tidyr::pivot_longer(cols=as.character(quants)) |>
#         dplyr::mutate(quantile=name)

#     # plot it
#     return(
#         plot + ggplot2::geom_line(ggplot2::aes(x=time, y=value, color=quantile), data=quant_data, alpha=0.4)
#     )
# }


#' Plot forecast quantiles
#'
#' Given forecast data, plot the requested quantiles as lines
#'
#' @template plt
#' @template fcst
#' @param quants A vector of numbers between 0 and 100, the quantiles to be graphed
#' @template alpha
#' @template colour
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
plot_quantiles <- function(plt=NULL, fcst, quants, alpha=1, colour="orange") {
    # validate
    validate_forecast(fcst)
    quants |> purrr::walk(\(quant) validate_quant(quant))
    if(any(duplicated(quants))) {
        stop("`quants` contains duplicate quantiles")
    }

    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    # get the quantiles in long form in a data frame
    # with `time`, `quantile`, and `value` columns
    quant_data <- quants |>
        purrr::imap(\(q, i) get_quantile(fcst$data, q) |> dplyr::mutate(value=quant, quant=NULL, quantile=q)) |>
        dplyr::bind_rows() |>
        dplyr::mutate(quantile=as.factor(quantile))

    # plot
    plt + ggplot2::geom_line(ggplot2::aes(x=time, y=value, linetype=quantile), data=quant_data, alpha=alpha, colour=colour)

    # using `{ggnewscale}` to make a second color scale is probably a bad idea because it impedes
    # customization by the user, and can be visually confusing
    # so instead we just use linetype above
    # plt + ggnewscale::new_scale_color() +
    #     ggplot2::geom_line(ggplot2::aes(x=time, y=value, color=quantile), data=quant_data, alpha=alpha)
}