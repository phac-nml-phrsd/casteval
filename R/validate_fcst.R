## validate forecast objects, forecast data frames, and observations data frames


#' Validate forecast in named-list format
#'
#' `validate_forecast()` validates a forecast by checking that:
#' - it is a named list
#' - it contains a valid data frame
#' - its forecast time is compatible with its time type
#' If any of these conditions fail, it stops with an appropriate an error message
#'
#' @param fcst The object to be validated
#'
#' @returns NULL if valid (error if invalid)
#'
#' @examples
#' # valid forecast
#' casteval:::validate_forecast(list(
#'   name="hello",
#'   forecast_time=5,
#'   data=data.frame(time=1:3, val=4:6)
#' ))
#' 
#' # not a forecast
#' try(casteval:::validate_forecast(data.frame(time=1:3, val=4:6)))
#' 
#' # bad forecast_time type
#' try(casteval:::validate_forecast(list(
#'   forecast_time=lubridate::as_date(1000),
#'   data=data.frame(time=1:3, val=4:6)
#' )))
validate_forecast <- function(fcst) {
    # must be list
    if(!is.list(fcst)) {
        stop("forecast must be named list")
    }

    # data frames pass is.list() but are probably a sign of a mistake
    if(is.data.frame(fcst)) {
        stop("forecast must be named list containing data frame, not just a data frame")
    }

    # `data` present
    if(! "data" %in% names(fcst)) {
        stop("forecast must contain `data`")
    }

    validate_data_frame(fcst$data)

    # check `forecast_time` type valid
    if(!is.null(fcst$forecast_time)) {
        validate_time(fcst$forecast_time, fcst)
    }
    invisible(NULL)
}


#' Validate observations data frame
#'
#' Check that a given object:
#' - is a data frame
#' - is not empty
#' - contains a valid `time` column
#' - contains a numeric `val_obs` column
#'
#' @param obs An observations object.
#'
#' @returns NULL if valid, error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_obs(
#'   data.frame(time=1:3, val_obs=4:6)
#' )
#' 
#' # valid (contains `score` column)
#' casteval:::validate_obs(
#'   data.frame(time=1:3, val_obs=4:6, score=c(1, 0, -1))
#' )
#' 
#' # invalid
#' try(casteval:::validate_obs(
#'   data.frame(time=1:3, val_obs=c("a", "b", "c"))
#' ))
validate_obs <- function(obs) {
    if(!is.data.frame(obs)) {
        stop("obs must be data frame")
    }

    if(nrow(obs) == 0) {
        stop("obs data frame has no rows")
    }

    cols <- colnames(obs)

    if(!"time" %in% cols) {
        stop("obs data frame requires time column")
    }

    validate_time_column(obs$time)

    if(!"val_obs" %in% cols) {
        stop("obs data frame requires val_obs column")
    }

    if(!is.numeric(obs$val_obs)) {
        stop("obs$val_obs must be numeric")
    }

    # check for duplicates
    dups <- obs |> dplyr::group_by(time) |> dplyr::filter(dplyr::n() > 1)
    if(nrow(dups) > 0) {
        tm <- dups$time[[1]]
        stop(glue::glue("obs contains duplicate observations at time {tm}"))
    }

    obs |> get_group_names() |> validate_group_names()

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
#'   create_forecast(data.frame(time=1:10, val=11:20)),
#'   data.frame(time=101:110, val_obs=111:120)
#' )
#' 
#' # incompatible time types
#' try(casteval:::validate_fcst_obs_pair(
#'   create_forecast(data.frame(time=1:10, val=11:20)),
#'   data.frame(time=lubridate::ymd("2024-01-01"), val_obs=5)
#' ))
validate_fcst_obs_pair <- function(fcst, obs) {
    validate_forecast(fcst)
    validate_obs(obs)
    if(get_time_type(obs) != get_time_type(fcst$data)) {
        stop("observations time type must match forecast time type")
    }
    invisible(NULL)
}


#' Validate forecast data frame
#'
#' Given a data frame, checks that:
#' 
#' - it isn't empty
#' - it has a valid time column
#' - its quantile columns are properly named and their values are in order
#' - it has at least one data column
#' - its data columns are numeric
#'
#' @param df A data frame
#'
#' @returns NULL on success. Error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_data_frame(data.frame(
#'   time=1:3,
#'   val=4:6
#' ))
#' 
#' # invalid (summary and unsummary data)
#' try(casteval:::validate_data_frame(data.frame(
#'   time=1:3,
#'   val=4:6,
#'   val_mean=7:9
#' )))
#' 
#' # invalid (no data columns)
#' try(casteval:::validate_data_frame(data.frame(
#'   time=1:3
#' )))
validate_data_frame <- function(df) {
    if(nrow(df) == 0) {
        stop("data frame has no rows")
    }

    cols <- colnames(df)

    # check time column
    if(! "time" %in% cols) {
        stop("data frame must contain `time` column")
    }
    validate_time_column(df$time)

    # check quantile column names
    quant_cols <- stringr::str_subset(cols, "^val_q")
    quant_cols |> purrr::walk(validate_quant_name)

    # check group column names
    df |> get_group_names() |> validate_group_names()

    # check contents of numeric columns
    numeric_columns <- c("sim", "val", "val_mean", quant_cols)
    for(col in numeric_columns) {
        if(col %in% cols) {
            if(!is.numeric(df[[col]])) {
                stop(glue::glue("{col} column must be numeric"))
            }
        }
    }

    # if sim exists, val must exist
    if("sim" %in% cols && (!"val" %in% cols)) {
        stop("sim column present but val column missing")
    }

    summary_present <- "val_mean" %in% cols || length(quant_cols) > 0

    if("val" %in% cols && summary_present) {
        stop("both summarized and unsummarized (`val`) data provided")
    }

    # check for duplicate entries
    if(("val" %in% cols && "sim" %in% cols) || summary_present) {
        if(summary_present) {
            grouped <- df |> dplyr::group_by(time)
        } else {
            grouped <- df |> dplyr::group_by(time, sim)
        }
        dups <- grouped |> dplyr::filter(dplyr::n() > 1)
        if(nrow(dups) > 0) {
            stop("data frame contains duplicate entries")
        }
    }

    if((!"val" %in% cols) && !summary_present) {
        stop("data frame contains no data columns")
    }

    validate_quant_order(df)

    invisible(NULL)
}