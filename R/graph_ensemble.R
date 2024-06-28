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
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))
#' ))
#' 
#' casteval:::graph_ensemble(
#'   NULL,
#'   create_forecast(data.frame(time=lubridate::as_datetime(c(0,20000,100000)), raw=c(20,30,40))
#' ))
graph_ensemble <- function(graph=NULL, fcst, alpha=NULL) {
    #TODO make the fit data points instead of lines
    #TODO opacity parameter
    validate_forecast(fcst)
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if(! "raw" %in% fcst$data_types) {
        stop("raw data needed to graph ensemble")
    }

    if(is.null(alpha)) {
        if(length(fcst$data$raw[[1]]) > 10) {
            alpha <- 0.2
        } else {
            alpha <- 0.5
        }

    }

    # convert to long format for easy ggplot interfacing
    df <- wide2long(fcst$data)

    graph + ggplot2::geom_line(ggplot2::aes(x=time, y=raw, group=realization), alpha=alpha, df)
}


#' Convert raw forecast data to long format
#'
#' Given a data frame with raw data, convert it to long format.
#'  Raw forecast data is usually stored in wide format, where the `raw` column
#'  contains vectors of realization numbers.
#'  In long format, each row contains a single data point for a single realization,
#'  and a new column keeps track of the realization number for that row.
#'
#' @param df The data frame. It should contain raw forecast data (see `create_forecast()`).
#'
#' @returns The data frame in long format.
#' @autoglobal
#'
#' @examples
#' # dplyr::tibble(
#' #   time=c(1,1,1,2,2,2,3,3,3),
#' #   realization=c(1,2,3,1,2,3,1,2,3),
#' #   raw=c(4,5,6,7,8,9,10,11,12)
#' # )
#' casteval:::wide2long(dplyr::tibble(
#'   time=1:3,
#'   raw=list(4:6, 7:9, 10:12)
#' ))
wide2long <- function(df) {
    # go through every row of the data frame
    1:nrow(df) |>
        purrr::map(\(row) {
            # expand that row into its own data frame
            dplyr::tibble(
                # recycle the time
                time=df[[row,"time"]],
                # number the realizations
                realization=1:length(df[[row,"raw"]][[1]]),
                # and put the raw values in long form
                raw=df[[row,"raw"]][[1]]
            )
        }) |>
        # now combine them back into one data frame
        purrr::reduce(dplyr::bind_rows) |>
        # remove NA rows
        dplyr::filter(!is.na(raw))
}
