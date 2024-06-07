#' Create a forecast
#'
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standardized format.
#'
#' @param dat Forecast data. Either a data frame or list of data frames.
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
#' # forecast with numeric times and raw data
#' create_forecast(data.frame(time=1:3, raw=10:12), name="a forecast", forecast_time=2)
#' 
#' # forecast with dates and mean-and-quantiles data
#' create_forecast(
#'   data.frame(
#'     time=c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02")),
#'     mean=10:11, quant_2.5=5:6, quant_97.5=15:16), name="another forecast")
#'
#' # combining an ensemble of realizations into one
#' create_forecast(list(
#'   dplyr::tibble(time=1:5,raw=6:10),
#'   dplyr::tibble(time=2:6,raw=7:11),
#'   dplyr::tibble(time=3:7,raw=8:12)))
#' 
#' # an already-combined ensemble
#' create_forecast(dplyr::tibble(time=1:2, raw=list(10:11, 12:13)))
create_forecast <- function(dat, name=NULL, forecast_time=NULL) {
    forecast <- list(name=name, forecast_time=forecast_time)
    # we check for data frame first since data frames are also lists

    # TODO detect lists that should be vectors and convert them to vectors

    # A single data frame 
    if(is.data.frame(dat)) {
        # validate data frame & get its format
        fmt <- get_format(dat)
        # store the format as metadata
        forecast$time_type <- fmt$time_type
        forecast$data_types <- fmt$data_types
        forecast$data <- dat
    }
    
    # A list of data frames, to be combined.
    else if(is.list(dat)) {
        if(length(dat) == 0) {
            stop("list of data frames is empty")
        }

        if(!all(as.logical(purrr::map(dat, is.data.frame)))) {
            stop("received list containing non-data-frames")
        }

        # validate data frames & get their formats
        fmts <- dat |> purrr::map(~ get_format(.x))

        if((fmts |> purrr::map(~ .x$time_type) |> unique() |> length()) > 1) {
            stop("all data frames must have same time type")
        }

        if(!all(as.logical(purrr::map(fmts, ~ "raw" %in% .x$data_types)))) {
            stop("all data frames must contain raw data")
        }

        forecast$time_type <- fmts[[1]]$time_type
        forecast$data_types <- "raw"
        forecast$data <- combine_data_frames(dat)
    } else {
        stop("`dat` has invalid type. Must be data frame or list of data frames")
    }

    # TODO sort the rows by time?
    # TODO verify that forecast_time type same as time_type, and do the same in validate_forecast()
    # TODO check that quantile values are in order (increasing order from lower quantiles to higher)
    forecast
}