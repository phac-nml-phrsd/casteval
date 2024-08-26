#' Plot quantile intervals
#'
#' Given forecast data, highlight the specified quantile intervals.
#'
#' @template plt
#' @template fcst
#' @param quant_intervals (Optional) A list of pairs of numbers between 0 and 100,
#' or a single pair of such numbers.
#'  If not provided, the quantile intervals will be inferred from the forecast.
#'  Otherwise, each pair will be used as a quantile intervals.
#' @template alpha
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
plot_quant_intervals <- function(plt=NULL, fcst, quant_intervals=NULL, alpha=NULL, palette=1) {
    ## validate/process parameters

    validate_forecast(fcst)
    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    # get quantile pairs
    quant_intervals <- parse_quant_pairs(quant_intervals, fcst$data)
    # sort quantile pairs
    quant_intervals <- quant_intervals |>
        purrr::map(\(pair) pair[[1]]) |>
        as.numeric() |>
        order() %>%
        quant_intervals[.] 

    if(is.null(alpha)) {
        alpha <- 0.5 / length(quant_intervals)
    }

    ## calculate the high and low quantiles for each interval

    # make a factor containing names of the intervals
    names <- quant_intervals |>
        purrr::map(\(pair) glue::glue("{pair[[1]]}%-{pair[[2]]}%")) |>
        as.character() %>%
        factor(., levels=.)

    intervals <- quant_intervals |>
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

    plt <- plt +
        # plot ribbons, with fill varying by interval number
        ggplot2::geom_ribbon(
            ggplot2::aes(x=time, ymin=lo, ymax=hi, fill=interval),
            alpha=alpha,
            data=intervals
        ) +
        # adjust the color palette
        ggplot2::scale_fill_brewer(palette=palette)

    validate_plotting_groups(fcst$data)
    plt |> apply_facets(get_plotting_groups(fcst$data))
}