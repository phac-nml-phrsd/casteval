#' Create a forecast object
#' 
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standard format.
#'
#' @param dat Forecast data. It can be in one of the following formats:
#' - A single data frame containing forecast data
#' - A list of data frames each containing unsummarized forecast data
#' - A named list with a `time` field (vector of times) and a `vals` field (list of realization vectors).
#'  Each of the vectors in `vals` must have the same length as `time`
#' 
#' Forecast data frames should contain:
#' - A `time` column
#' - (Optional) a `sim` column, containing simulation numbers for unsummarized data
#' - (Optional) a `val` column, containing unsummarized data.
#'  If `sim` is present then `val` must be present as well
#' - (Optional) columns starting with `val_q` followed by a number from 0 to 100, containing quantile data
#' - (Optional) a `val_mean` column, containing mean data
#'
#' Times can be either numeric, dates, or date-times. All data must be numeric.
#' `sim` must also be numeric if present.
#' 
#' @param name (Optional) A string specifying the name of the forecast
#' @param forecast_time (Optional) An integer, date, or date-time specifying when the forecast was created.
#'  Its type should match the type of values in the `time` column(s) of `data`
#'  If provided, this forecast will be scored only using data corresponding to dates/times greater than or equal to `forecast_time`.
#'  Additionally, plots of this forecast may graphically distinguish between values to the left and right of `forecast_time`.
#' 
#' @returns A named list with fields `data`, `name`, `forecast_time`.
#' `data` is a data frame containing forecast data.
#' `name` and `forecast_time` are the same as the parameters passed to `create_forecast()`
#' @export
#' @autoglobal
#'
#' @examples
#' # forecast with numeric times and raw data
#' create_forecast(data.frame(time=1:3, val=10:12), name="a forecast", forecast_time=2)
#' 
#' # forecast with dates and mean-and-quantiles data
#' create_forecast(
#'   data.frame(
#'     time=c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02")),
#'     val_mean=10:11, val_q2.5=5:6, val_q97.5=15:16
#'   ),
#'   name="another forecast"
#' )
#'
#' # an ensemble of 3 realizations, each represented by a data frame
#' create_forecast(list(
#'   dplyr::tibble(time=1:5,val=6:10),
#'   dplyr::tibble(time=2:6,val=7:11),
#'   dplyr::tibble(time=3:7,val=8:12)
#' ))
#' 
#' # an already-combined ensemble
#' create_forecast(dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2), val=c(20,21,22,23,24,10,11,12,13,14)))
#' 
#' # an already-combined ensemble with simulation numbers
#' create_forecast(dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2), sim=c(1,2,3,4,5,1,2,3,4,5), val=c(20,21,22,23,24,10,11,12,13,14)))
#' 
#' # an ensemble of 4 realizations, each represented by a vector
#' create_forecast(list(
#'   time=1:3,
#'   vals=list(4:6, 7:9, 10:12, 13:15)
#' ))
create_forecast <- function(dat, name=NULL, forecast_time=NULL) {
    # TODO grouping & corresponding input formats
    # TODO warn about extra columns

    #TODO messages & quiet mode

    #check quant symmetry
    if(is.data.frame(dat)) {
        df <- create_forecast_single(dat)
    }
    
    else if(is.list(dat)) {
        if("time" %in% names(dat) && "vals" %in% names(dat)) {
            df <- create_forecast_ensemble(dat$time, dat$vals)
        } else {
            df <- create_forecast_multiple(dat)
        }
    }

    # check time type compatibility
    fcst <- list(name=name, forecast_time=forecast_time, data=df)
    if(!is.null(fcst$forecast_time)) {
        validate_time(fcst$forecast_time, forecast)
    }

    fcst
}


#' Create forecast from single data frame
#'
#' Helper for `create_forecast()`.
#'
#' @param df A data frame
#' @returns A forecast object
#' @autoglobal
#'
#' @examples
#' # See `create_forecast()`
create_forecast_single <- function(df) {
    forecast <- list(name=name, forecast_time=forecast_time)

    # validate data frame
    validate_data_frame(df)
    forecast$data <- df

    forecast
}

#' Create forecast from multiple data frames
#'
#' Helper for `create_forecast()`.
#'
#' @param dfs A list of data frames
#' @param name A string
#' @param forecast_time A number, date, or date-time
#'
#' @returns A forecast object
#' @autoglobal
#'
#' @examples
#' # See `create_forecast()`
create_forecast_multiple <- function(dfs, name, forecast_time) {
    forecast <- list(name=name, forecast_time=forecast_time)

    if(length(dfs) == 0) {
        stop("list of data frames is empty")
    }

    if(!all(as.logical(purrr::map(dfs, is.data.frame)))) {
        stop("received list containing non-data-frames")
    }

    # validate data frames & get their formats
    fmts <- dfs |> purrr::map(~ get_format(.x))

    if((fmts |> purrr::map(~ .x$time_type) |> unique() |> length()) > 1) {
        stop("all data frames must have same time type")
    }

    if(!all(as.logical(purrr::map(fmts, ~ "raw" %in% .x$data_types)))) {
        stop("all data frames must contain raw data")
    }

    forecast$time_type <- fmts[[1]]$time_type
    forecast$data_types <- "raw"
    forecast$data <- combine_data_frames(dfs)
    forecast
}


#' Create forecast from time vector and ensemble of realizations
#'
#' Helper for `create_forecast()`.
#'
#' @param dat A named list containing names `time` and `ensemble`
#' @param name A string
#' @param forecast_time A number, date, or date-time
#'
#' @returns A forecast object
#' @autoglobal
#'
#' @examples
#' # See `create_forecast()`
create_forecast_ensemble <- function(dat, name, forecast_time) {
    forecast <- list(name=name, forecast_time=forecast_time)
    
    ## do input validation
    tm <- dat$time
    ens <- dat$ensemble

    if(!is.numeric(tm)) {
        stop("`dat$time` must be numeric vector")
    }

    if(!is.list(ens)) {
        stop("`dat$ensemble` must be list")
    }

    if(length(tm) == 0) {
        stop("`dat$time` is empty")
    }

    if(length(ens) == 0) {
        stop("`dat$ensemble` is empty")
    }

    if(!all(as.logical(purrr::map(ens, is.numeric)))) {
        stop("`dat$ensemble` must be list of numeric vectors")
    }

    lens <- ens |> purrr::map(length) |> as.numeric()
    if(any(length(tm) != lens)) {
        stop("all vectors in `dat$ensemble` must have the same length as `dat$time`")
    }

    # transpose list of vectors
    raw <- ens |> purrr::list_transpose() |> purrr::map(as.numeric)
    # build data frame
    df <- dplyr::tibble(time=tm, raw=raw)
    forecast$time_type <- get_time_type(tm)
    forecast$data_types <- "raw"
    forecast$data <- df
    forecast
}