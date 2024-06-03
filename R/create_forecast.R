#' Create a forecast
#'
#' `create_forecast()` creates a forecast object given data and optional metadata.
#' It accepts a variety of forecast formats as input and intelligently converts them into a standardized format.
#'
#' @param data Forecast data. Either a data frame or list of data frames.
#'  An ensemble of forecasts can be provided as a list of data frames, in which case it will be aggregated into a single data frame.
#' @param name A string containing the name of the forecast/model.
#' @param forecast_date A string, integer, date, or date-time containing the day the forecast was created. Its type should match the type of values in the `time` column(s) of `data`
#' 
#' @returns A named list containing the forecast and its metadata
#' @export
#'
#' @examples
#' # TBD
create_forecast <- function(data, name=NULL, forecast_date=NULL) {
    # we check for data frame first since data frames are also lists
    if(is.data.frame(data)) {

    } else if (is.list(data)) {

    }
}