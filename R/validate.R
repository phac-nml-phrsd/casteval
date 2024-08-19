## miscellaneous validation functions
## a validation function returns invisible(NULL) on success and errors otherwise


#' Check that time compatible with forecast
#'
#' Check that the type of a given time matches the time type of a given forecast.
#'
#' @param t A time (e.x. a number, date, or date-time).
#' @template fcst
#'
#' @returns NULL if `t` is compatible with `fcst`. Error otherwise
#' @autoglobal
#'
#' @examples
#' # both numeric (compatible)
#' casteval:::validate_time(5, create_forecast(data.frame(time=6,val=7)))
#' 
#' # one date, one date-time (incompatible)
#' try(casteval:::validate_time(
#'   lubridate::ymd("2024-01-01"),
#'   create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_00:00:00"),val=6))
#' ))
validate_time <- function(t, fcst) {
    time_type <- get_time_type(fcst$data)
    if((lubridate::is.Date(t) && time_type == "date") ||
        (lubridate::is.POSIXt(t) && time_type == "date-time") ||
        (is.numeric(t) && time_type == "numeric")) {
        return(invisible(NULL))
    } else {
        stop("type of `t` does not match `fcst$time_type`")
    }
}


#' Check that column is in data frame
#'
#' Errors if `col` is not a column in data frame.
#'
#' @param df A data frame.
#' @param col A string containing the column name.
#'
#' @returns NULL if valid, error otherwise.
#' @autoglobal
#'
#' @examples
#' casteval:::validate_column(
#'   data.frame(time=1, val=4, val_q25=6, val_q75=8),
#'   "val_q25"
#' )
#' try(casteval:::validate_column(
#'   data.frame(time=1, val=4),
#'   "val_mean"
#' ))
validate_column <- function(df, col) {
    if(! col %in% colnames(df)) {
        stop(paste0("column `", col, "` not in data frame"))
    }
    invisible(NULL)
}


# TODO make error messages more informative


#' Validate time column
#'
#' Check that the type of a time column is valid.
#'
#' @param times A vector, presumably a time column
#'
#' @returns NULL if valid, error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_time_column(c(1,2,3))
#' casteval:::validate_time_column(lubridate::as_date(1000:1010))
#' casteval:::validate_time_column(lubridate::as_datetime(10000:10100))
#' 
#' # invalid
#' try(casteval:::validate_time_column(list(1,2,3)))
#' try(casteval:::validate_time_column(c("monday", "tuesday")))
validate_time_column <- function(times) {
    if(lubridate::is.Date(times) ||
        lubridate::is.POSIXt(times) ||
        is.numeric(times)) {
            return(invisible(NULL))
    } else {
        stop("time column must be either numeric, Date, or date-time (POSIXt) vector")
    }
}


#' Validate forecast group names
#'
#' Check that the forecast groups don't contain any invalid names (in particular, the empty string)
#'
#' @param names A character vector of the group names (without the leading "grp_")
#'
#' @returns NULL if valid, error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_group_names(character(0))
#' casteval:::validate_group_names(c("variable", "scenario", "___12345"))
#' 
#' # invalid
#' try(
#'   casteval:::validate_group_names(c("variable", ""))
#' )
validate_group_names <- function(names) {
    # check for empty string
    if("" %in% names) {
        stop("provided empty group name ('grp_')")
    }

    invisible(NULL)
}