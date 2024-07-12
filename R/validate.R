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


#' Validate forecast data frame
#'
#' Given a data frame, checks that:
#' 
#' - it has a valid time column
#' - its quantile columns are properly named
#' - its data columns are numeric
#'
#' @param df A data frame
#'
#' @returns NULL on success. Error otherwise
#' @autoglobal
#'
#' @examples
#' # TODO
validate_data_frame <- function(df) {
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
#' #TODO
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
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
validate_time_column <- function(times) {
    if(lubridate::is.Date(times) ||
        lubridate::is.POSIXt(times) ||
        is.numeric(times)) {
            return(invisible(NULL))
    } else {
        stop("time column must be either numeric, Date, or date-time (POSIXt)")
    }
    invisible(NULL)
}
