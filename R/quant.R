# various functions for inspecting/accessing the "val_q*" columns in data frames

#' Get quantile numbers from column names
#'
#' Given a (forecast) data frame,
#'  inspects the column names and returns the quantile numbers.
#'  This function does no input validation.
#'
#' @param df The data frame,
#'  presumably containing columns named "val_q" followed by a number from 0 to 100.
#'
#' @returns A numeric vector containing the quantiles, sorted in increasing order.
#' @autoglobal
#'
#' @examples
#' # returns c(2.5, 50, 97.5)
#' casteval:::get_quant_percentages(
#'   data.frame(time=1:3, val_q2.5=4:6, val_q50=7:9, val_q97.5=10:12)
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
#' @param df The data frame. Should contain a "val_q*" column corresponding to `num`
#' @param num A number between 0 and 100 inclusive.
#'
#' @returns The `num` quantile column in `df`.
#' @autoglobal
#'
#' @examples
#' # returns 4:6
#' casteval:::get_quant_col(data.frame(time=1:3, val_q50=4:6), 50)
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
#' # "val_q50"
#' casteval:::quant_name(50)
#' 
#' # "val_q2.5"
#' casteval:::quant_name(2.5)
quant_name <- function(num) {
    paste0("val_q", as.character(num))
}


#' Obtain quantiles from data frame
#'
#' Given a forecast data frame, compute quantiles from raw data or existing quantile columns,
#'  depending on what is present.
#'
#' @param df A forecast data frame. It should contain raw or quantile data.
#' @param perc A percentage, from 0 to 100.
#'
#' @returns A data frame with a `time` column and a `quant` column, containing the requested quantile data
#' @autoglobal
#'
#' @examples
#' df1 <- dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))
#' df2 <- dplyr::select(df1, time, quant_50)
#' 
#' # c(5, 8, 11)
#' casteval:::get_quantile(df1, 50)
#' 
#' # c(1000, 3000, 3000)
#' casteval:::get_quantile(df2, 50)
get_quantile <- function(df, perc) {
    qcol <- glue::glue("val_q{perc}")
    # if raw values present, compute quantile from them, regardless of whether quantile columns present
    if("val" %in% colnames(df)) {
        # group by time
        quants <- df |> dplyr::group_by(time) |>
            dplyr::summarize(quant=stats::quantile(val, perc/100)[[1]])
        return(quants)
    }

    else if(qcol %in% colnames(df)) {
        return(dplyr::select(df, time, quant=dplyr::all_of(qcol)))
    }

    else {
        stop(glue::glue("could not compute/obtain {perc}% quantile from data frame"))
    }
}


#' Pair up matching quantiles
#'
#' Given a list of quantiles, pair together the ones symmetrical around the median.
#' For example, the 2.5% quantile and 97.5% quantile go together.
#' The 50% quantile will never be paired.
#'
#' @param quants A vector of distinct numbers between 0 and 100.
#'
#' @returns A named list with two elements: `paired`, a list of pairs of quantiles,
#'  and `unpaired`, a vector of all the leftover quantiles.
#' @autoglobal
#'
#' @examples
#' # list(paired=list(c(15,85), c(30,70)), unpaired=c(0,10,20,50,51,60,75))
#' casteval:::pair_quantiles(c(0, 10, 15, 20, 30, 50, 51, 60, 70, 75, 85))
pair_quantiles <- function(quants) {
    paired <- list()
    unpaired <- numeric(0)

    quants <- quants |> sort() |> unique()
    while(length(quants) > 1) {
        first <- quants[[1]]
        last <- tail(quants, n=1)

        # equidistant pair from 50%
        if(last - 50 == 50 - first) {
            paired[[length(paired) + 1]] <- c(first, last)
            quants <- quants[c(-1, -length(quants))]
        }

        # last is more distant from 50%
        else if(last - 50 > 50 - first) {
            unpaired[[length(unpaired) + 1]] <- last
            quants <- quants[-length(quants)]
        }

        # first is more distanct from 50%
        else {
            unpaired[[length(unpaired) + 1]] <- first
            quants <- quants[-1]
        }
    }

    if(length(quants) == 1) {
        unpaired[[length(unpaired) + 1]] <- quants[[1]]
    }

    list(paired=paired, unpaired=sort(unpaired))
}


#' Pairs quantile pair(s)
#'
#' Helper function for funtions that accept a `quant_pairs` argument.
#'
#' @param name desc
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' 
parse_quant_pairs <- function(quant_pairs) {

}