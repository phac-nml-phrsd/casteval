## functions for dealing with group columns


#' Get group names of forecast data
#'
#' Gets the names of groups in a forecast by inspecting its columns.
#'
#' @param df A forecast data frame
#'
#' @returns A character vector of names
#' @autoglobal
#'
#' @examples
#' #TODO
get_group_names <- function(df) {
    colnames(df) |>
        stringr::str_subset("^grp_") |>
        purrr::map(\(col) regmatches(col, regexpr("_", col), invert = TRUE)[[1]][[2]]) |>
        as.character()
}