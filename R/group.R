## functions for dealing with group columns


#' Get group names of forecast data
#'
#' Gets the names of groups in a forecast by inspecting its columns.
#' Wrapper for `get_group_cols()`
#'
#' @param df A forecast data frame
#'
#' @returns A character vector of names
#' @autoglobal
#'
#' @examples
#' # character(0)
#' casteval:::get_group_names(data.frame(time=1))
#' 
#' # c("variable", "scenario")
#' casteval:::get_group_names(data.frame(time=1, grp_variable=2, grp_scenario=3))
get_group_names <- function(df) {
    get_group_cols(df) |>
        purrr::map(\(col) regmatches(col, regexpr("_", col), invert = TRUE)[[1]][[2]]) |>
        as.character()
}


#' Get group column names of forecast data
#'
#' desc
#'
#' @param df A forecast data frame
#'
#' @returns A character vector of column names
#' @autoglobal
#'
#' @examples
#' casteval:::get_group_cols(data.frame(time=1,val=2,grp_scenario=3,grp_variable=4))
get_group_cols <- function(df) {
    colnames(df) |> stringr::str_subset("^grp_")
}


#' Group forecast data frame
#'
#' Given a forecast data frame, group it by all the `grp_*` columns
#'
#' @param df A forecast data frame
#' @param ... Additional arguments to be passed to `dplyr::group_by()` (e.x. `.add`, `.drop`)
#'
#' @returns The grouped forecast data frame
#' @autoglobal
#'
#' @examples
#' # `{casteval}` exports an example data frame for grouping
#' casteval:::group_all(groupex)
#' 
#' dplyr::group_by(groupex, time) |> casteval:::group_all(.add=TRUE)
group_all <- function(df, ...) {
    group_cols <- get_group_cols(df)
    df |> dplyr::group_by(dplyr::pick(dplyr::all_of(group_cols)), ...)
}


#' Does the forecast data have groups?
#' 
#' Predicate function which determines whether `df` contains `grp_*` columns
#'
#' @param df A forecast data frame
#'
#' @returns `TRUE` if there are groups, `FALSE` otherwise
#' @autoglobal
#'
#' @examples
#' casteval:::has_groups(data.frame(time=1,val=2))
#' 
#' casteval:::has_groups(data.frame(time=1, val=2, grp_scenario=3))
has_groups <- function(df) {
    length(get_group_names(df)) > 0
}