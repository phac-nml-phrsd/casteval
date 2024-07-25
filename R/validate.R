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
    # we convert column name -> number -> back to name so that it gets sorted
    quant_names <- get_quant_percentages(df) |>
        purrr::map(\(x) quant_name(x)) |>
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


#' Validate quantile column name
#'
#' Check that a quantile column name starts with `val_q` followed by a number between 0 and 100.
#'
#' @param name A string containing the quantile column name
#'
#' @returns NULL if valid. Error otherwise
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_name("val_q97.5")
#' casteval:::validate_quant_name("val_q50")
#' casteval:::validate_quant_name("val_q100")
#' casteval:::validate_quant_name("val_q0")
#' 
#' # invalid
#' try(casteval:::validate_quant_name("val_q50abc"))
#' try(casteval:::validate_quant_name("val_q101"))
#' try(casteval:::validate_quant_name("val_q-1"))
#' try(casteval:::validate_quant_name("hello"))
validate_quant_name <- function(name) {
    parts <- strsplit(name, "q")[[1]]
    if(length(parts) != 2) {
        stop(glue::glue("invalid quantile column name {name}"))
    }

    quant <- suppressWarnings(as.numeric(parts[[2]]))
    if(is.na(quant)) {
        stop(glue::glue("invalid quantile percentage {quant}"))
    }

    quant <- as.numeric(quant)
    if(quant < 0 || quant > 100) {
        stop(glue::glue("quantile percentage {quant} out of range"))
    }

    invisible(NULL)
}


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


#' Validate quantile interval vector
#'
#' Given a quantile pair, check that it is valid.
#' A valid quantile pair is a numeric vector of length 2
#' where both values are between 0 and 100,
#' and the first number is smaller than the second.
#'
#' @param pair An element of `quant_pairs`
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_pair(c(50, 70))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(70, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(-1, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair(c(50,60,70)))
#' 
#' # invalid
#' try(casteval:::validate_quant_pair("50, 60"))
validate_quant_pair <- function(pair) {
    if(!is.numeric(pair)) {
        stop("quantile pair must be vector of 2 numbers")
    }

    if(length(pair) != 2) {
        stop("quantile pair must have length 2")
    }

    low <- pair[[1]]
    high <- pair[[2]]

    if(low >= high) {
        stop("first quantile in pair must be less than second quantile in pair")
    }

    if(low < 0 || low > 100 || high < 0 || high > 100) {
        stop("quantiles in pair must be between 0 and 100, inclusive")
    }

    invisible(NULL)
}