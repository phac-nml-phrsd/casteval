#' Create a forecast object
#' 
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standard format.
#'
#' @param dat Forecast data. It can be in one of the following formats:
#' - A data frame containing forecast data
#' - A named list with a `time` field (vector of times) and a `vals` field (list of realization vectors).
#'  Each of the vectors in `vals` must have the same length as `time`
#' 
#' In the first option above, the data frame should contain:
#' - A `time` column
#' - (Optional) a `sim` column, containing simulation numbers for unsummarized data
#' - (Optional) a `val` column, containing unsummarized data.
#'  If `sim` is present then `val` must be present as well.
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
#' # an already-combined ensemble
#' create_forecast(dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2), val=c(20,21,22,23,24,10,11,12,13,14)))
#' 
#' # an already-combined ensemble with simulation numbers
#' create_forecast(dplyr::tibble(time=c(1,1,1,1,1,2,2,2,2,2), sim=c(1,2,3,4,5,1,2,3,4,5), val=c(20,21,22,23,24,10,11,12,13,14)))
#' 
#' # an ensemble of 4 realizations, each represented by a vector in `vals`
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
        validate_data_frame(dat)
        df <- dat
    }
    
    else if(is.list(dat)) {
        if("time" %in% names(dat) && "vals" %in% names(dat)) {
            df <- create_forecast_ensemble(dat$time, dat$vals)
        } else {
            stop("`dat` list must contain `time` and `vals` fields")
        }
    }

    else {
        stop("`dat` has invalid type. See `?create_forecast` or `vignette(topic='casteval', package='casteval')` for proper usage")
    }

    # TODO? sort rows by time, maybe it will improve performance

    # check time type compatibility
    fcst <- list(name=name, forecast_time=forecast_time, data=df)
    if(!is.null(fcst$forecast_time)) {
        validate_time(fcst$forecast_time, fcst)
    }

    fcst
}


# TODO support a format with a list of vectors for each time point


#' Create forecast from time vector and ensemble of realizations
#'
#' Helper for `create_forecast()`.
#'
#' @param time A vector of times
#' @param vals A list of vectors of values.
#'  Each vector corresponds to a realization which will be assigned a simulation number.
#'
#' @returns A forecast data frame
#' @autoglobal
#'
#' @examples
#' # See `create_forecast()`
create_forecast_ensemble <- function(time, vals) {
    ## validate `time` and `vals`

    validate_time_column(time)

    if(length(time) == 0) {
        stop("`dat$time` is empty")
    }
    
    if(!is.list(vals)) {
        stop("`dat$vals` must be a list")
    }

    if(length(vals) == 0) {
        stop("`dat$vals` is empty")
    }

    if(!all(as.logical(purrr::map(vals, is.numeric)))) {
        stop("`dat$vals` must be list of numeric vectors")
    }

    lens <- vals |> purrr::map(length) |> as.numeric()
    if(any(length(time) != lens)) {
        stop("all vectors in `dat$vals` must have the same length as `dat$time`")
    }

    # create a data frame from each vector in vals, give them each a unique sim number, then bind them together
    vals |> purrr::imap(\(val, i) data.frame(time=time, sim=i, val=val)) |>
        dplyr::bind_rows()
}

# TODO filter out NAs in forecast data and observations