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


#' Plot a forecast quantile
#'
#' Given forecast data, plot one of its quantiles as a line
#'
#' @template plt
#' @template fcst
#' @quant 
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' 
plot_quantile <- function(plt=NULL, fcst, quant, alpha=0.5, colour="orange") {

}


#' Plot quantile intervals
#'
#' Given forecast data, highlight the specified quantile intervals.
#'
#' @template plt
#' @template fcst
#' @param quant_pairs (Optional) A list of pairs of numbers between 0 and 100.
#'  If not provided, the quantile intervals will be inferred from the forecast.
#'  Otherwise, each pair will be used as a quantile intervals.
#' @param alpha (Optional) The alpha parameter to be passed to `ggplot2`
#' @param palette (Optional) The colour palette to use (see `?ggplot2::scale_fill_brewer`)
#'
#' @returns A ggplot object.
#' @autoglobal
#' @export
#'
#' @examples
#' # forecast with quantile data
#' fc1 <- create_forecast(dplyr::tibble(
#'   time=1:3,
#'   val_q5=8:6, val_q95=22:20,
#'   val_q10=10:8, val_q90=20:18,
#'   val_q25=14:12, val_q75=16:14
#' ))
#' 
#' # forecast with raw data
#' fc2 <- create_forecast(dplyr::tibble(
#'   time=rep(1:3, each=5),
#'   sim=rep(1:5, 3),
#'   val=c(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
#' ))
#' 
#' # infer quantile pairs
#' plot_quant_intervals(NULL, fc1)
#' 
#' # manually specify quantile pairs
#' plot_quant_intervals(NULL, fc1, list(c(5,95), c(10,90)))
#' 
#' # quantile pairs must be specified for raw data
#' plot_quant_intervals(NULL, fc2, list(c(5, 95), c(25,75)))
plot_quant_intervals <- function(plt=NULL, fcst, quant_pairs=NULL, alpha=NULL, palette=1) {
    ## validate/process parameters

    validate_forecast(fcst)
    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    # get quantile pairs
    quant_pairs <- parse_quant_pairs(quant_pairs, fcst$data)
    # sort quantile pairs
    quant_pairs <- quant_pairs |>
        purrr::map(\(pair) pair[[1]]) |>
        as.numeric() |>
        order() %>%
        quant_pairs[.] 

    if(is.null(alpha)) {
        alpha <- 0.5 / length(quant_pairs)
    }

    ## calculate the high and low quantiles for each interval

    # make a factor containing names of the intervals
    names <- quant_pairs |>
        purrr::map(\(pair) glue::glue("{pair[[1]]}%-{pair[[2]]}%")) |>
        as.character() %>%
        factor(., levels=.)

    intervals <- quant_pairs |>
        # for every pair,
        purrr::imap(\(pair, i)
            # get the low quantile in a data frame
            get_quantile(fcst$data, pair[[1]]) |>
                # append the high quantile and assign a name (which is a factor)
                dplyr::mutate(
                    lo=quant,
                    hi=get_quantile(fcst$data, pair[[2]])$quant,
                    interval=names[[i]]
                )
        ) |>
        # combine into one data frame
        dplyr::bind_rows()

    ## plot the intervals

    plt +
        # plot ribbons, with fill varying by interval number
        ggplot2::geom_ribbon(
            ggplot2::aes(x=time, ymin=lo, ymax=hi, fill=interval),
            alpha=alpha,
            data=intervals
        ) +
        # adjust the color palette
        ggplot2::scale_fill_brewer(palette=palette)
}