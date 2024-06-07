#' Get data frame format
#'
#' Helper function for `create_forecast()`
#' `get_format()` inspects the columns of a data frame to determine how it's formatted.
#' Also performs input validation on the contents of the data frame.
#'
#' @param df A data frame. It should have a valid time column and a valid combination of data columns.
#'
#' @returns A named list containing a `time_type` (a string containing the type of the values in the time column)
#'  and `data_types` (a character vector containing the types of data columns present)
#'
#' @examples
#' # TBD
get_format <- function(df) {
    # empty data frames trip up some of the checks below
    if(nrow(df) == 0) {
        stop("data frame is empty")
    }

    cols <- colnames(df)
    # check existence of time column
    if(! "time" %in% cols) {
        stop("data frame does not contain `time` column")
    }

    # validate and identify time types (date, date-time, or numeric)
    time_type <- get_time_type(df$time)

    # check for existence of various data columns
    raw_exists <- ("raw" %in% cols)
    quant_cols <- stringr::str_subset(cols, "^quant_") # all columns whose names start with "quant_" contain quantiles
    quant_exists <- (length(quant_cols) > 0)
    mean_exists <- ("mean" %in% cols)

    data_types <- c()

    # raw overrides everything else
    if(raw_exists) {
        if(mean_exists) {
            stop("both raw and mean values provided. Only provide one or the other")
        }
        if(quant_exists) {
            # this could possibly be changed into a warning where the quantiles are simply discarded (and recomputed later if necessary)
            # however it would require extra error checking elsewhere
            stop("both raw and quantile values provided. For mean-and-quantiles format, use column name `mean`, not `raw`")
        }

        # check all raw values numeric (vector or otherwise)
        if(!column_all(df$raw, is.numeric)) {
            stop("raw column not all numeric")
        }

        # get lengths of raw vectors
        raw_lens <- purrr::map(df$raw, length)

        if(length(unique(raw_lens)) > 1) {
            # this could be changed into a warning if necessary
            # however it would make plotting trajectories difficult down the line
            stop("raw ensemble vectors have inconsistent lengths")
        }

        data_types <- c(data_types, "raw")
    }

    # if no raw, then check for other data columns (usually summaries)
    else {
        if(mean_exists) {
            if(!column_all(df$mean, is.numeric)) {
                stop("mean column not all numeric")
            }
            data_types <- c(data_types, "mean")
        }

        # TODO move all quantile column processing into a new file & functions
        if(quant_exists) {
            for(col in quant_cols) {
                # check numeric
                if(!column_all(df[[col]], is.numeric)) {
                    stop(paste(col, "column not all numeric"))
                }

                # check valid quantile number provided
                parts <- strsplit(col)[[1]]
                if(length(parts) != 2) {
                    stop(paste("quantile column name", col, "badly formatted"))
                }
                perc <- as.numeric(parts[[2]])
                if(is.na(perc)) {
                    stop(paste("quantile column name", col, "does not specify percentage"))
                }
                if(perc < 0 || perc > 100) {
                    stop(paste("quantile percentage must be between 0 and 100, not", parts[[2]]))
                }
            }
            data_types <- c(data_types, "quant")
        }
    }

    # if no data columns were found above, there is a problem
    if(is.null(data_types)) {
        stop("data frame contains no data columns")
    }

    list(time_type=time_type, data_types=data_types)
}


#' Get type of time column
#'
#' Helper function for `get_format()`.
#' Inspects a time column from a data frame to determine the type of its contents.
#' Also checks that the contents are all of the same supported type.
#'
#' @param timecol A vector or list, presumably a column in a data frame.
#'  Its contents should all be of the same suppotred type (date, date-time, or numeric)
#'
#' @returns A string describing the type of the time column
#'
#' @examples
#' # "numeric"
#' casteval:::get_time_type(c(1,2,3))
#' # "date"
#' casteval:::get_time_type(c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02")))
#' # "date-time"
#' casteval:::get_time_type(list(lubridate::ymd_hms("2024-01-01_12:34:56")))
#' # inconsistent types
#' try(casteval:::get_time_type(list(1, lubridate::ymd("2024-01-01"))))
#' # unsupported types
#' try(casteval:::get_time_type(list("January 1", "January 2")))
get_time_type <- function(timecol) {
    if(length(timecol) == 0) {
        stop("no times present")
    }

    # validate time column & get type
    if(column_all(timecol, lubridate::is.Date)) { # all dates
        time_type <- "date"
    } else if(column_all(timecol, lubridate::is.POSIXt)) { # all date-times
        time_type <- "date-time"
    } else if(column_all(timecol, is.numeric)) { # all numbers
        time_type <- "numeric"
    } else {
        stop("time column has inconsistent/unsupported types")
    }
    time_type
}


#' Check everything in a column
#'
#' `column_all()` checks that all elements of a given column pass a given predicate function.
#' Useful for checking types.
#'
#' @param col A list or vector, presumably a column in a data frame.
#' @param f A predicate function (accepts one argument and returns a boolean).
#'
#' @returns TRUE if everything in col passes `f()`, FALSE otherwise
#'
#' @examples
#' # returns TRUE
#' casteval:::column_all(list(1, 2.5, 1.4e7, -10, 5L, Inf, NaN), is.numeric)
#' 
#' # returns FALSE
#' casteval:::column_all(list(1, 2, 3), function(x) {x < 3})
#' 
#' # NA is by default a logical value, not a numeric one. Returns FALSE
#' casteval:::column_all(list(1, NA), is.numeric)
#' 
#' # NA gets coerced to NA_real_, because it's in a numeric vector. Returns TRUE
#' casteval:::column_all(c(1,NA), is.numeric)
column_all <- function(col, f) {
    all(as.logical(purrr::map(col, f)))
}