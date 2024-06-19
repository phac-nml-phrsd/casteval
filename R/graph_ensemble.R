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

    num_realizations <- length(fcst$data$raw)
}

# TODO move to another file
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
#' @export
#' @autoglobal
#'
#' @examples
#' # TODO
wide2long <- function(df) {
    # go through every row of the data frame
    1:nrow(df) |>
        purrr::map(\(row) {
            # expand that row into its own data frame
            dplyr::tibble(
                # recycle the time
                time=df[[row,"time"]],
                # number the realizations
                realization=1:length(df[[row,"raw"]]),
                # and put the raw values in long form
                raw=df[[row,"raw"]]
            )
        }) |>
        # now combine them back into one data frame
        purrr::reduce(dplyr::bind_rows)
}