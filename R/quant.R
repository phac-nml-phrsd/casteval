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
#' # returns c(2.5, 50, 97.5)
#' casteval:::get_quant_percentages(
#'   data.frame(time=1:3, quant_2.5=4:6, quant_50=7:9, quant_97.5=10:12)
#' )
get_quant_percentages <- function(df) {
    cols <- colnames(df)
    quant_cols <- stringr::str_subset(cols, "^val_q")
    
    quant_cols |>
        purrr::map(~ as.numeric(strsplit(.x, "q")[[1]][[2]])) |>
        as.numeric() |>
        sort()
}


#' Get quantile column from data frame
#'
#' Given a data frame and a quantile number, returns the corresponding
#'  column from the data frame.
#'
#' @param df The data frame. Should contain a "quant_*" column corresponding to `num`
#' @param num A number between 0 and 100 inclusive.
#'
#' @returns The `num` quantile column in `df`.
#' @autoglobal
#'
#' @examples
#' # returns 4:6
#' casteval:::get_quant_col(data.frame(time=1:3, quant_50=4:6), 50)
get_quant_col <- function(df, num) {
    as.numeric(df[[quant_name(num)]])
}


#' Turn quantile number into column name
#'
#' Given a number, returns the column name of a corresponding quantile column
#'
#' @param num The quantile number.
#'
#' @returns The quantile column name.
#' @autoglobal
#'
#' @examples
#' # "quant_50"
#' casteval:::quant_name(50)
#' 
#' # "quant_2.5"
#' casteval:::quant_name(2.5)
quant_name <- function(num) {
    paste0("val_q", as.character(num))
}


#' Compute quantile data from raw data
#'
#' Given a column of raw data, calculate a given quantile.
#'
#' @param raw A numeric vector, or a list of non-empty numeric vectors. The vectors should not contain NA values.
#'  Functions that use `raw2quant()` should make sure not to provide it NA values,
#'  for example by using `remove_raw_NAs()`.
#' @param perc A percentage, from 0 to 100.
#'
#' @returns A numeric vector with the same length as `raw`.
#' @autoglobal
#'
#' @examples
#' # c(2, 2, 4.5, 9)
#' casteval:::raw2quant(list(1:3, 2, 3:6, 7:11), 50)
#' # 26
#' casteval:::raw2quant(list(0:100), 26)
raw2quant <- function(raw, perc) {
    if(anyNA(raw, recursive=TRUE)) {
        stop("raw data contains NA values")
    }
    raw |> purrr::map(\(x) stats::quantile(x, perc/100)[[1]]) |> as.numeric()
}


#' Obtain quantiles from data frame
#'
#' Given a forecast data frame, compute quantiles from raw data or existing quantile columns,
#'  depending on what is present.
#'
#' @param df A forecast data frame. It should contain raw and/or quantile data.
#' @param perc A percentage, from 0 to 100.
#'
#' @returns A numeric vector with one entry for each row in `df`.
#' @autoglobal
#'
#' @examples
#' df1 <- dplyr::tibble(time=1:3, quant_50=c(1000,2000,3000), raw=list(4:6, 7:9, 10:12))
#' df2 <- dplyr::select(df1, time, quant_50)
#' 
#' # c(5, 8, 11)
#' casteval:::get_quantile(df1, 50)
#' 
#' # c(1000, 3000, 3000)
#' casteval:::get_quantile(df2, 50)
get_quantile <- function(df, perc) {
    # if raw present, compute quantile from raw, regardless of whether quantile columns present
    if("raw" %in% colnames(df)) {
        df <- remove_raw_NAs(df)
        return(raw2quant(df$raw, perc))
    }

    else if(perc %in% get_quant_percentages(df)) {
        return(get_quant_col(df, perc))
    }

    else {
        stop(paste0("could not compute/obtain ", perc, "% quantile from data frame"))
    }
}