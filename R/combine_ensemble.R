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
    # this is not the most efficient way to do this but it is clean
    # if it becomes a performance bottleneck it can be rewritten
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
    # when dplyr::left_join() encounters identical (non-keyed) column names, it automatically renames them
    # however we do this renaming manually to avoid confusion and possible bugs

    # get the current lengths of the vectors that df1 and df2 contain
    # this is important because of how dplyr::full_join() works
    stopif(nrows(df1 == 0))
    stopif(nrows(df2 == 0))
    df1_len <- length(df1$raw[[1]])
    df2_len <- length(df2$raw[[1]])

    df1 |>
        # rename raw -> raw1
        dplyr::rename(raw1=raw) |>
        # join by time column
        dplyr::full_join(df2, dplyr::join_by(time)) |>
        # full_join() introduces NULLs when a key is in one data frame but not the other,
        # so we have to replace them with NA's to preserve the order & number of ensembles
        dplyr::mutate(raw1=widen_NULL(raw1,df1_len), raw=widen_NULL(raw, df2_len)) |>
        # merge the two raw columns with c() and discard `raw1`
        dplyr::mutate(raw=purrr::map2(raw1, raw, c), raw1=NULL)
}

#' Replace NULLs with vectors of NAs
#'
#' Helper for `combine_two_data_frames()`.
#' Replaces NULLs in a list with vectors of NAs of the correct length
#'
#' @param lst A list of vectors and/or NULLs.
#'  All NULLs in the list will be replaced with a vector of `len` NAs 
#' @param len A positive integer.
#'
#' @returns The modified `lst`.
#'
#' @examples
#' # TBD
widen_NULL <- function(lst, len) {
    purrr::map(len, ~ dplyr::if_else(is.null(.x), rep(NA, len), .x))
}