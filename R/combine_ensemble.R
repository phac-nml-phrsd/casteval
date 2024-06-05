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

#' Combine a group of forecast data frames into one
#'
#' `combine_data_frames()` converts a list of forecast data frames
#' into a single data frame containing all the aggregated data points in one column.
#' It performs no input validation so it is not user-facing.
#' 
#' @param dfs A list of forecast data frames, with raw data points and the same time types.
#'
#' @returns A data frame containing the aggregated data.
#'
#' @examples
#' # TBD
combine_data_frames <- function(dfs) {
    if(length(dfs) == 0) {
        stop("dfs has length 0")
    }

    # accumulate
    out <- dfs[[1]]
}

#' Combine two forecast data frames into one
#'
#' Helper for `combine_data_frames()`.
#' See `?combine_data_frames()` for details.
#'
#' @param df1 The first data frame.
#' @param df2 The second data frame.
#'
#' @returns The combined data frame.
combine_two_data_frames <- function(df1, df2) {

}