#' Validate forecast in named-list format
#'
#' `validate_forecast()` validates a forecast by checking that it:
#' - is a named list
#' - contains a `time_type` field
#' - contains a `data_types` field
#' - contains a valid data frame
#' - data frame format matches `time_type` and `data_type`
#' If any of these conditions fail, it stops with an appropriate an error message
#'
#' @param fcst The object to be validated
#'
#' @returns NULL if valid (error if invalid)
#'
#' @examples
#' #TODO
validate_forecast <- function(fcst) {
    # must be list
    if(!is.list(fcst)) {
        stop("forecast must be named list")
    }

    # data frames pass is.list() but are probably a sign of a mistake
    if(is.data.frame(fcst)) {
        stop("forecast must be named list containing data, not just a data frame")
    }

    # `time_type` present
    if(! "time_type" %in% names(fcst)) {
        stop("forecast must specify `time_type`")
    }

    # `data_types` present
    if(! "data_types" %in% names(fcst)) {
        stop("forecast must specify `data_types`")
    }

    # `data` present
    if(! "data" %in% names(fcst)) {
        stop("forecast must contain `data`")
    }

    # get_format() both validates the data frame and returns its format to us
    fmt <- get_format(fcst$data)
    
    # check that the formats are consistent
    if(fcst$time_type != fmt$time_type) {
        stop("stated time type does not match data frame time type")
    }
    if(!setequal(fcst$data_types, fmt$data_types)) {
        stop("stated data types do not match data frame data types")
    }

    NULL
}

#' Check that time compatible with forecast
#'
#' Check that the type of a given time matches the time type of a given forecast.
#'
#' @param t A time (e.x. a number, date, or date-time).
#' @param fcst A forecast (as returned by `create_forecast()`).
#'  No input validation is done on `fcst` itself.
#'
#' @returns NULL if `t` is compatible with `fcst`. Error otherwise
#' @autoglobal
#'
#' @examples
#' # both numeric (compatible)
#' validate_time(5, create_forecast(data.frame(time=6,raw=7)))
#' 
#' # one date, one date-time (incompatible)
#' try(validate_time(
#'   lubridate::ymd("2024-01-01"),
#'   create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_00:00:00"),raw=6))
#' ))
validate_time <- function(t, fcst) {
    if(lubridate::is.Date(t) && fcst$time_type == "date" ||
        lubridate::is.POSIXt(t) && fcst$time_type == "date-time" ||
        is.numeric(t) && fcst$time_type == "numeric") {
        return(NULL)
    } else {
        stop("type of `t` does not match `fcst$time_type`")
    }
}