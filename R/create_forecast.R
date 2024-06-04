#' Create a forecast
#'
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standardized format.
#'
#' @param data Forecast data. Either a data frame or list of data frames.
#'  An ensemble of forecasts can be provided as a list of data frames, in which case it will be aggregated into a single data frame.
#'  The data frame(s) should contain a column named `time`, which may be of integers, dates, or date-times.
#'  No mixing of types is allowed (e.g. `time` may not contain both integers and date-times).
#' @param name A string specifying the name of the forecast/model.
#' @param forecast_time An integer, date, or date-time specifying when the forecast was created.
#'  Its type should match the type of values in the `time` column(s) of `data`
#'  If provided, this forecast will be scored only using data corresponding to dates/times greater than or equal to `forecast_time`.
#'  Additionally, graphs of this forecast will highlight the `forecast_time` using a vertical line.
#' 
#' @returns A named list containing the forecast and its metadata
#' @export
#'
#' @examples
#' # TBD
create_forecast <- function(data, name=NULL, forecast_time=NULL) {
    forecast <- list(name=name, forecast_time=forecast_time)
    # we check for data frame first since data frames are also lists
    if(is.data.frame(data)) {
        if(! "time" %in% colnames(data))
            stop("`data` does not contain `time` column")
        
    } else if (is.list(data)) {

    } else {
        stop("`data` has invalid type. Must be data frame or list of data frames")
    }
}


# takes in a data frame
# inspects the columns & column names to determine how it's formatted
# also does input validation
# returns the type as a string
# all input data frames pass through this function to ensure they are well-formatted
get_format <- function(data) {
    cols <- colnames(data)
    # check existence of time column
    if(! "time" %in% cols) {
        stop("data frame does not contain `time` column")
    }

    # validate and identify time types (date, datetime, or numeric)
    time_type <- get_time_type(data["time"])

    # check for existence of various data columns
    raw_exists <- ("raw" %in% cols)
    quant_cols <- stringr::str_subset(cols, "^quant_") # all columns whose names start with "quant_" contain quantiles
    quant_exists <- (length(quant_cols) > 0)
    mean_exists <- ("mean" %in% cols)

    if(raw_exists) {
        if(mean_exists) {
            stop("both raw and mean values provided")
        }
        if(quant_exists) {
            # this could possibly be changed into a warning where the quantiles are simply discarded (and recomputed later if necessary)
            stop("both raw and quantile values provided. For mean-and-quantiles format, use column name `mean`, not `raw`")
        }

        # check all raw values numeric (vector or otherwise)
        if(!column_all(data["raw"], is.numeric)) {
            stop("raw column not all numeric")
        }

    }


}

# receives the time column (list or vector) and returns a string indicating the type
# raises error if not valid
get_time_type <- function(timecol) {
    # validate time column & get type
    if(column_all(timecol, lubridate::is.Date)) { # all dates
        time_type <- "date"
    } else if(column_all(timecol, lubridate::is.POSIXt)) { # all date-times
        time_type <- "datetime"
    } else if(column_all(timecol, is.numeric)) { # all numbers
        time_type <- "numeric"
    } else {
        stop(paste("time column has inconsistent/unsupported types. Supported types are", paste(supported_time_types, sep=", ")))
    }
    return time_type
}

# helper function for validating column types
# f is a predicate
column_all <- function(col, f) {
    all(purrr::map(col, f))
}