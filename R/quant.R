# various functions for inspecting/accessing the "quant_*" columns in data frames

#' Get quantile numbers from column names
#'
#' Given a (forecast) data frame,
#'  inspects the column names and returns the quantile numbers.
#'  This function does no input validation.
#'
#' @param df The data frame,
#'  presumably containing columns named "quant_" followed by a number from 0 to 100.
#'
#' @returns A numeric vector containing the quantiles, sorted in increasing order.
#' @autoglobal
#'
#' @examples
#' #TBD
get_quantiles <- function(df) {
    cols <- colnames(df)
    quant_cols <- str_subset(cols, "^quant_")
    
    quant_cols |>
        purrr::map(~ as.numeric(strsplit(.x, "_")[[1]][[2]])) |>
        as.numeric() |>
        sort()
}