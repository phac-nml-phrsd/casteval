#' Combine a group of forecasts into one
#'
#' User-facing wrapper for `combine_data_frames()`.
#' Converts multiple forecasts into a single forecast.
#'
#' @param ... The forecasts (individual realizations) to be combined.
#'  They should have the same time type and all contain raw values only.
#'  Forecasts should be formatted as named lists, such as the ones `create_forecast()` returns.
#'
#' @returns A named list representing the resulting forecast.
#' @export
#'
#' @examples
#' #TBD
combine_ensemble <- function(...) {
    # ensure that time types are identical
    # call combine_data_frames()
}

#' Combine a group of data frames into one
#'
#' `combine_data_frames()` converts a list of forecast data frames
#' into a single data frame containing all the aggregated data points in one column.
#'
#' @param dfs A list of forecast data frames, with raw data points and the same time types.
#'
#' @returns A data frame containing the aggregated data.
#'
#' @examples
#' # TBD
combine_data_frames <- function(dfs) {

}