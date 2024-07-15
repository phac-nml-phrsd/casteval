#' Get type of data frame time column
#'
#' Inspects a data frame's time column and returns its type.
#'
#' @param df A data frame with a `time` column.
#' 
#' @returns A string, one of "numeric", "date", "date-time"
#'
#' @examples
#' # "numeric"
#' casteval:::get_time_type(data.frame(time=c(1,2,3)))
#' 
#' # "date"
#' casteval:::get_time_type(data.frame(time=lubridate::as.Date(1:3)))
#' 
#' # "date-time"
#' casteval:::get_time_type(data.frame(time=lubridate::ymd_hms("2024-01-01_12:34:56")))
#' 
#' # unsupported type
#' try(casteval:::get_time_type(list("January 1", "January 2")))
get_time_type <- function(df) {
    if(!"time" %in% colnames(df)) {
        stop("data frame does not contain time column")
    }
    timecol <- df$time

    # validate time column & get type
    if(lubridate::is.Date(timecol)) { # all dates
        return("date")
    } else if(lubridate::is.POSIXt(timecol) || lubridate::is.POSIXct(timecol)) { # all date-times
        return("date-time")
    } else if(is.numeric(timecol)) { # all numbers
        return("numeric")
    } else {
        stop("time column has unsupported type")
    }
}