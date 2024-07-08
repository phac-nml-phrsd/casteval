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
#' # incorrect time type
#' try(casteval:::validate_forecast(list(
#'   time_type="date", data_types="raw", data=data.frame(time=1:3, raw=4:6)
#' )))
#' 
#' # invalid `forecast_time` type
#' try(casteval:::validate_forecast(list(
#'   time_type="numeric",
#'   data_types="raw",
#'   data=data.frame(time=1,raw=4),
#'   forecast_time=lubridate::ymd("2024-01-01")
#' )))
#' 
#' # a valid forecast
#' casteval:::validate_forecast(list(
#'   time_type="numeric",
#'   data_types=c("mean", "quant"),
#'   data=data.frame(time=1:3, mean=4:6, quant_50=7:9),
#'   forecast_time=2
#' ))
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

    # check `forecast_time` type valid
    if(!is.null(fcst$forecast_time)) {
        validate_time(fcst$forecast_time, fcst)
    }
    invisible(NULL)
}

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
#' casteval:::validate_time(5, create_forecast(data.frame(time=6,raw=7)))
#' 
#' # one date, one date-time (incompatible)
#' try(casteval:::validate_time(
#'   lubridate::ymd("2024-01-01"),
#'   create_forecast(data.frame(time=lubridate::ymd_hms("2024-01-01_00:00:00"),raw=6))
#' ))
validate_time <- function(t, fcst) {
    if((lubridate::is.Date(t) && fcst$time_type == "date") ||
        (lubridate::is.POSIXt(t) && fcst$time_type == "date-time") ||
        (is.numeric(t) && fcst$time_type == "numeric")) {
        return(NULL)
    } else {
        stop("type of `t` does not match `fcst$time_type`")
    }
}


#' Check that column is in data frame
#'
#' Errors of `col` is not a column in data frame.
#'
#' @param df A data frame.
#' @param col A string containing the column name.
#'
#' @returns NULL if valid, error otherwise.
#' @autoglobal
#'
#' @examples
#' casteval:::validate_column(
#'   data.frame(time=1, raw=4, quant_25=6, quant_75=8),
#'   "quant_25"
#' )
#' try(casteval:::validate_column(
#'   data.frame(time=1, raw=4),
#'   "mean"
#' ))
validate_column <- function(df, col) {
    if(! col %in% colnames(df)) {
        stop(paste0("column `", col, "` not in data frame"))
    }
    invisible(NULL)
}


#' Validate a forecast-observations pair
#'
#' Given a forecast and observations, verify that:
#' - forecast is valid
#' - observations are valid
#' - forecast time type matches observations time type
#'
#' @template fcst
#' @param obs An observations data frame.
#'
#' @returns NULL if valid. Error otherwise
#' @autoglobal
#'
#' @examples
#' # compatible time types
#' casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, raw=11:20)),
#'   data.frame(time=101:110, obs=111:120)
#' )
#' 
#' # incompatible time types
#' try(casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, raw=11:20)),
#'   data.frame(time=lubridate::ymd("2024-01-01"), obs=5)
#' ))
validate_fcst_obs_pair <- function(fcst, obs) {
    validate_forecast(fcst)
    obs_time_type <- get_obs_format(obs)
    if(obs_time_type != fcst$time_type) {
        stop("observations time type must match forecast time type")
    }
    invisible(NULL)
}


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
    # we convert column name -> number -> back to name so that it can be sorted
    quant_names <- get_quant_percentages(df) |>
        purrr::map(\(x) paste0("quant_", x)) |>
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

# TODO make error messages more informative