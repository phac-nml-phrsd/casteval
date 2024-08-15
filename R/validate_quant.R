## validate quantiles/quantile columns


#' Make sure quantile values are logically possible
#'
#' Checks that values in quantile columns are in increasing order for each time point.
#' That is, if `x < y`, then `df$quant_x[[i]] <= df$quant_y[[i]]` must hold true for all `1 <= i <= nrow(df)`
#'
#' @param df A forecast data frame, with a `time` column and 0 or more `quant_*` columns
#'
#' @returns NULL if valid, error otherwise.
#' @autoglobal
#'
#' @examples
#' try(
#'   casteval:::validate_quant_order(data.frame(time=1:3, quant_25=4:6, quant_75=c(4,4,4)))
#' )
#' casteval:::validate_quant_order(data.frame(time=1:3, quant_2.5=4:6, quant_50=c(4,6,8)))
validate_quant_order <- function(df) {
    # get quantile percentages in increasing order
    # we convert column name -> number -> back to name so that it gets sorted
    quant_names <- get_quant_percentages(df) |>
        purrr::map(\(x) quant_name(x)) |>
        as.character()

    # get the quantile columns in order
    rows <- df |> dplyr::select(dplyr::all_of(quant_names)) |>
        unname() |>
        # then turn it into list of rows, corresponding to time points
        purrr::transpose()

    # check that each row is nonstrictly increasing
    unsorted <- rows |> purrr::map(\(row) is.unsorted(as.numeric(row), na.rm=TRUE)) |> as.logical()
    if(any(unsorted)) {
        stop(paste("quantiles have impossible values in row", which(unsorted)[[1]]))
    }

    invisible(NULL)
}


#' Validate quantile column name
#'
#' Check that a quantile column name starts with `val_q` followed by a number between 0 and 100.
#'
#' @param name A string containing the quantile column name
#'
#' @returns NULL if valid. Error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_name("val_q97.5")
#' casteval:::validate_quant_name("val_q50")
#' casteval:::validate_quant_name("val_q100")
#' casteval:::validate_quant_name("val_q0")
#' 
#' # invalid
#' try(casteval:::validate_quant_name("val_q50abc"))
#' try(casteval:::validate_quant_name("val_q101"))
#' try(casteval:::validate_quant_name("val_q-1"))
#' try(casteval:::validate_quant_name("hello"))
validate_quant_name <- function(name) {
    parts <- strsplit(name, "q")[[1]]
    if(length(parts) != 2) {
        stop(glue::glue("invalid quantile column name {name}"))
    }

    quant <- suppressWarnings(as.numeric(parts[[2]]))
    if(is.na(quant)) {
        stop(glue::glue("invalid quantile percentage {quant}"))
    }

    quant <- as.numeric(quant)
    if(quant < 0 || quant > 100) {
        stop(glue::glue("quantile percentage {quant} out of range"))
    }

    invisible(NULL)
}


#' Validate quantile interval vector
#'
#' Given a quantile pair, check that it is valid.
#' A valid quantile pair is a numeric vector of length 2
#' where both values are between 0 and 100,
#' and the first number is smaller than the second.
#'
#' @param pair An element of `quant_pairs`
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_pair(c(50, 70))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(70, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(-1, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(50,60,70)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair("50, 60"))
validate_quant_pair <- function(pair) {
    if(!is.numeric(pair)) {
        stop("quantile pair must be vector of 2 numbers")
    }

    if(length(pair) != 2) {
        stop("quantile pair must have length 2")
    }

    low <- pair[[1]]
    high <- pair[[2]]

    if(low >= high) {
        stop("first quantile in pair must be less than second quantile in pair")
    }

    if(low < 0 || low > 100 || high < 0 || high > 100) {
        stop("quantiles in pair must be between 0 and 100, inclusive")
    }

    invisible(NULL)
}


#' Validate a single quantile
#'
#' Given a quantile, check that it:
#' - is a single number
#' - is between 0 and 100, inclusive
#'
#' @param quant The quantile to validate
#'
#' @returns `NULL` if valid, error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant(50)
#' casteval:::validate_quant(0)
#' casteval:::validate_quant(100)
#' casteval:::validate_quant(2.5)
#' 
#' # invalid
#' try(casteval:::validate_quant("50"))
#' try(casteval:::validate_quant(-1))
#' try(casteval:::validate_quant(c(25,75)))
validate_quant <- function(quant) {
    if(!is.numeric(quant)) {
        stop("`quant` must be numeric")
    }

    if(length(quant) != 1) {
        stop(glue::glue("`quant` must be exactly 1 number, received numeric vector of length {length(quant)}"))
    }

    if(quant < 0 || quant > 100) {
        stop("`quant` must be between 0 and 100")
    }

    invisible(NULL)
}