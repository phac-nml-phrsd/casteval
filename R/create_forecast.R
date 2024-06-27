#' Create a forecast
#'
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standardized format.
#'
#' @param dat Forecast data. It can be one of the following:
#' - A single data frame containing raw or summary data.
#' - A list of data frames each containing raw data.
#' - A named list containing a numeric vector `time` and a list of vectors `data`.
#' Each vector corresponds to a realization in a forecast ensemble.
#'  The `time` vector and `data` vectors must all have the same length.
#' 
#' All data frames in `dat` must additionally have a `time` column containing either integers, dates, or date-times.
#' 
#' Raw data columns should be named `raw`.
#' Summary data columns may be named `mean`, or `quant_` followed by a number from 0 to 100 (e.x. `quant_2.5`, `quant_50`, etc.).
#' All raw or summary data must be numeric.
#' 
#' See below for examples.
#' 
#' See [get_format()] for further details on data frame formatting.
#' 
#' @param name (Optional) A string specifying the name of the forecast/model.
#' @param forecast_time (Optional) An integer, date, or date-time specifying when the forecast was created.
#'  Its type should match the type of values in the `time` column(s) of `data`
#'  If provided, this forecast will be scored only using data corresponding to dates/times greater than or equal to `forecast_time`.
#'  Additionally, plots of this forecast may graphically distinguish between values to the left and right of `forecast_time`.
#' 
#' @returns A named list containing the forecast and its metadata.
#'  The (possibly processed) data frame is stored in `$data`.
#'  The name and forecast time are stored in `$name` and `$forecast_time`.
#'  The type of the time column (one of "date", "date-time", or "numeric") is stored in `$time_type`
#'  The types of the data columns (a character vector containing "mean", "quant", and/or "raw") are stored in `$data_types`
#' @export
#'
#' @examples
#' # forecast with numeric times and raw data
#' create_forecast(data.frame(time=1:3, raw=10:12), name="a forecast", forecast_time=2)
#' 
#' # forecast with dates and mean-and-quantiles data
#' create_forecast(
#'   data.frame(
#'     time=c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02")),
#'     mean=10:11, quant_2.5=5:6, quant_97.5=15:16
#'   ),
#'   name="another forecast"
#' )
#'
#' # combining an ensemble of realizations into one
#' create_forecast(list(
#'   dplyr::tibble(time=1:5,raw=6:10),
#'   dplyr::tibble(time=2:6,raw=7:11),
#'   dplyr::tibble(time=3:7,raw=8:12)
#' ))
#' 
#' # an already-combined ensemble
#' create_forecast(dplyr::tibble(time=1:2, raw=list(10:15, 20:25)))
create_forecast <- function(dat, name=NULL, forecast_time=NULL) {
    # TODO third option where you provide a list of vertical vectors instead of list of data frames)
    # TODO support even more input data formats
    # TODO consider changing the format so that each realization is its own column

    # TODO update vignette
    # we check for data frame first since data frames are also lists

    # TODO detect lists that should be vectors and convert them to vectors

    # A single data frame
    if(is.data.frame(dat)) {
        forecast <- create_forecast_single(dat, name, forecast_time)
    }
    
    # A list of data frames, to be combined.
    else if(is.list(dat)) {
        forecast <- create_forecast_multiple(dat, name, forecast_time)
    } else {
        stop("`dat` has invalid type. Must be data frame or list of data frames")
    }

    # TODO sort the rows by time?

    # check forecast_time type consistency
    if(!is.null(forecast$forecast_time)) {
        validate_time(forecast$forecast_time, forecast)
    }

    forecast
}


#' Create forecast from single data frame
#'
#' Helper for `create_forecast()`.
#'
#' @param df A data frame
#' @param name A string
#' @param forecast_time A number, date, or date-time
#'
#' @returns A forecast object
#' @autoglobal
#'
#' @examples
#' # See `create_forecast()`
create_forecast_single <- function(df, name, forecast_time) {
    forecast <- list(name=name, forecast_time=forecast_time)

    # validate data frame & get its format
    fmt <- get_format(df)
    # store the format as metadata
    forecast$time_type <- fmt$time_type
    forecast$data_types <- fmt$data_types
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