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
#' Given a number, returns the column name of a corresponding quantile column.
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
#' df1 <- data.frame(time=rep(1:3,each=3), sim=rep(1:3, 3), val=c(4:6, 7:9, 10:12))
#' df2 <- data.frame(time=1:3, val_q50=c(1000, 2000, 3000))
#' 
#' casteval:::get_quantile(df1, 50)
#' casteval:::get_quantile(df2, 50)
get_quantile <- function(df, perc) {
    qcol <- glue::glue("val_q{perc}")
    # if raw values present, compute quantile from them, regardless of whether quantile columns present
    if("val" %in% colnames(df)) {
        # group by time
        quants <- df |> dplyr::group_by(time) |> group_all(.add=TRUE) |>
            dplyr::summarize(quant=stats::quantile(val, perc/100)[[1]], .groups="drop")
        return(quants)
    }

    else if(qcol %in% colnames(df)) {
        groupcols <- get_group_cols(df)
        return(dplyr::select(df, time, dplyr::all_of(groupcols), quant=dplyr::all_of(qcol)))
    }

    else {
        stop(glue::glue("could not compute/obtain {perc}% quantile from data frame"))
    }
}


#' Pair up matching quantiles
#'
#' Given a list of quantiles, pair together the ones symmetrical around the median.
#' For example, the 2.5% quantile and 97.5% quantile form a pair.
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
        last <- utils::tail(quants, n=1)

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


#' Parse quantile pair(s)
#'
#' Helper function for funtions that accept a `quant_pairs` argument.
#' Validates/formats the given pairs, or infers the quantile pairs from forecast data.
#'
#' @param quant_pairs A list of pairs, a single pair, or NULL
#' @param df A forecast data frame
#' @param allow_empty If `FALSE`, an error will be raised when no quantile pairs can be inferred.
#' If `TRUE`, an empty list will be returned silently. Defaults to `FALSE`.
#'
#' @returns A list of pairs, either taken from `quant_pairs` or inferred from `df`
#' @autoglobal
#'
#' @examples
#' # infer from forecast
#' casteval:::parse_quant_pairs(
#'   NULL,
#'   data.frame(time=1, val_q10=2, val_q25=3, val_50=4, val_q75=5, val_q90=6)
#' )
#' 
#' # single pair
#' casteval:::parse_quant_pairs(
#'   c(2.5,97.5),
#'   data.frame(time=1, val=1)
#' )
#' 
#' # multiple pairs
#' casteval:::parse_quant_pairs(
#'   list(c(2.5,97.5), c(25,75)),
#'   data.frame(time=1, val=1)
#' )
#' 
#' # allow empty
#' casteval:::parse_quant_pairs(
#'   NULL,
#'   data.frame(time=1:3, val_q50=4:6),
#'   allow_empty=TRUE
#' )
#' 
#' casteval:::parse_quant_pairs(
#'   list(),
#'   data.frame(time=1, val=1),
#'   allow_empty=TRUE
#' )
parse_quant_pairs <- function(quant_pairs, df, allow_empty=FALSE) {
    if(is.null(quant_pairs)) { # default quant_pairs -> infer from forecast
        quant_pairs <- pair_quantiles(get_quant_percentages(df))$paired
        if(length(quant_pairs) == 0) {
            if(allow_empty) {
                return(list())
            } else {
                stop("could not infer quantile pairs from forecast data")
            }
        }
    }
    else if(is.numeric(quant_pairs)) { # provided a single pair
        validate_quant_pair(quant_pairs)
        quant_pairs <- list(quant_pairs)
    }
    else if(is.list(quant_pairs)) { # provided list of pairs
        if(length(quant_pairs) == 0) {
            if(allow_empty) {
                return(list())
            } else {
                stop("`quant_pairs` is empty")
            }
        }
        # validate 
        quant_pairs |> purrr::walk(validate_quant_pair)
    }
    else {
        stop("`quant_pairs` must be either NULL, pair of quantiles, or list of pairs of quantiles")
    }

    quant_pairs
}