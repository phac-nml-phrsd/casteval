# graph_confidence_intervals() is here too because it's closely related to quantiles

#' Graph forecast quantiles
#'
#' Graph lines indicating quantiles of a forecast.
#'
#' @template graph
#' @template fcst
#' @param quants A numeric vector containing the quantiles to be graphed, as percentages.
#'  If `NULL`, all quantiles present in the forecast data frame will be graphed.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' fc1 <- create_forecast(dplyr::tibble(
#' time=1:3,
#' raw=list(0:5, 5:10, 10:15)
#' ))
#' 
#' NULL |> casteval:::graph_ensemble(fc1) |> casteval:::graph_quantiles(fc1, c(2.5, 25,51,75))
graph_quantiles <- function(graph=NULL, fcst, quants=NULL) {
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    
    # if not specified, use all present quantile columns
    if(is.null(quants)) {
        quants <- get_quant_percentages(fcst$data)
    }

    if(length(quants) == 0) {
        # could possibly be made a warning
        # TODO: set defaults for quantiles?
        stop("no quantiles specified and none found in data frame")
    }

    # TODO maybe make this map->reduce instead of map->wide->long
    # compile quantile data into a data frame
    quant_data <- quants |>
        # get all specified quantiles from forecast as a list of vectors
        purrr::map(\(x) get_quantile(fcst$data, x)) |>
        # name according to quantile
        stats::setNames(quants) |>
        # convert to data frame
        dplyr::as_tibble() |>
        # append time column
        dplyr::mutate(time=fcst$data$time) |>
        # convert to long format for ggplot2
        tidyr::pivot_longer(cols=as.character(quants))

    # graph it
    return(
        graph + ggplot2::geom_line(ggplot2::aes(x=time, y=value, color=name), quant_data)
    )
}


#' Graph forecast confidence intervals
#'
#' Graph shaded bands indicating confidence intervals of a forecast.
#'
#' @template graph
#' @template fcst
#' @param confs (Optional) A numeric vector containing the confidence intervals to be displayed, as percentages.
#'  If `NULL`, the confidence intervals will be inferred from the quantiles present in the forecast data frame.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(dplyr::tibble(
#'   time=1:3,
#'   raw=list(c(3,5,6,7,3), c(6,8,7,8,7), c(11,15,13,14,17))
#' ))
#' NULL |> casteval:::graph_ensemble(fc) |> casteval:::graph_confidence_intervals(fc, c(90,50))
graph_confidence_intervals <- function(graph=NULL, fcst, confs=NULL) {
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }
    
    if(is.null(confs)) {
        # get sorted quantiles
        quants <- get_quant_percentages(fcst$data)
        # convert to confidence intervals
        confs <- quants2confs(quants)
    }

    if(length(confs) == 0) {
        stop("no confidence intervals specified and none inferrable from data frame")
    }

    # for each confidence interval...
    conf_data <- confs |>
        # make a data frame containing its high and low quantiles
        purrr::map(\(x) dplyr::tibble(
            time=fcst$data$time,
            lo=get_quantile(fcst$data, 50-x/2),
            hi=get_quantile(fcst$data, 50+x/2),
            conf=x # prepare for the reduction below
        )) |>
        # bind together in long form
        purrr::reduce(dplyr::bind_rows) |>
        # make `conf` into factor, largest first
        dplyr::mutate(conf = as.factor(conf) |> forcats::fct_rev())

    # graph it
    graph + 
        ggplot2::geom_ribbon(
            ggplot2::aes(x=time, ymin=lo, ymax=hi, fill=conf),
            alpha=0.5/length(confs), conf_data
        ) +
        ggplot2::scale_fill_brewer()
}


#' Get confidence intervals from quantiles
#'
#' Helper function for graph_confidence_intervals().
#' Errors if the given quantiles are not sufficient to infer confidence intervals.
#'
#' @param quants Numeric vector containing quantile percentages.
#'
#' @returns Numeric vector containing confidence intervals.
#' @export
#' @autoglobal
#'
#' @examples
#' # c(50,80,90,95,100)
#' casteval:::quants2confs(c(0,2.5,5,10,25,75,90,95,97.5,100))
#' 
#' # c(50,80)
#' casteval:::quants2confs(c(25,10,90,75))
quants2confs <- function(quants) {
    # sort quantiles in case they aren't already sorted
    quants <- sort(quants)

    # we need an even amount
    len <- length(quants)
    if(len %% 2 != 0) {
        stop("even number of quantiles needed to infer confidence intervals")
    }

    # they must be symmetric around the median
    if(any(quants != rev(100-quants))) {
        stop("quantiles must be symmetric around median to infer confidence intervals")
    }

    # calculate confidence intervals
    sort(2 * (50 - quants[1:(len/2)]))
}