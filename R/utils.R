# miscellaneous utilities and helper functions


#' Is it a forecast?
#'
#' Determines whether a given R object is a forecast object or not.
#' 
#' @details
#' An R object is a forecast object if:
#' 
#' - It is a list (and also not a data frame)
#' - It has a `name` field (may be `NULL`)
#' - It has a `forecast_time` field (may be `NULL`)
#' - It has a `data` field
#' 
#' `is_forecast()` does NO further input validation beyond this.
#' For a version of the function that does input validation, see `is_valid_forecast()`.
#'
#' @param fcst An R object
#'
#' @returns `TRUE` if `fcst` is a forecast object, `FALSE` otherwise
#' @autoglobal
#'
#' @examples
#' # FALSE
#' casteval:::is_forecast(5)
#' casteval:::is_forecast(data.frame(time=1, val=2))
#' 
#' # TRUE
#' casteval:::is_forecast(list(name=NULL, forecast_time=NULL, data=NULL))
is_forecast <- function(fcst) {
    if(
        is.list(fcst) &&
        !is.data.frame(fcst) &&
        "name" %in% names(fcst) &&
        "forecast_time" %in% names(fcst) &&
        "data" %in% names(fcst)
    ) {
        TRUE
    } else {
        FALSE
    }
}


#' Is it a valid forecast?
#'
#' A stricter version of `is_forecast()` which validates
#' its input using `validate_forecast()`.
#'
#' @param fcst An R object
#'
#' @returns `TRUE` if `fcst` passes `validate_forecast()` without any errors,
#' `FALSE` otherwise
#' @autoglobal
#'
#' @examples
#' # FALSE
#' casteval:::is_valid_forecast(5)
#' casteval:::is_valid_forecast(list(name=NULL, forecast_time=NULL, data=data.frame(a=1,b=2)))
#' 
#' # TRUE
#' casteval:::is_valid_forecast(list(name=NULL, forecast_time=NULL, data=data.frame(time=1, val=2)))
is_valid_forecast <- function(fcst) {
    tryCatch(
        {
            validate_forecast(fcst)
            TRUE    
        },
        error=\(cnd) FALSE
    )
}

#' Isolate projected values from fit values
#'
#' Removes rows from forecast data frame which should not be scored,
#' i.e. the data generated for dates/times prior to when the forecast was created.
#'
#' @param df The forecast data frame. See `?create_forecast` for details
#' @param forecast_time Either NULL or a time of the same type as the values in `dat$time`.
#'  If non-NULL, all rows with time prior to `forecast_time` will be removed from `dat`.
#'
#' @returns The filtered data frame (no change if `forecast_time` NULL)
#' @autoglobal
#'
#' @examples
#' # data.frame(time=5:10, val=15:20)
#' casteval:::filter_forecast_time(data.frame(time=1:10, val=11:20), 5)
#' 
#' # unchanged
#' casteval:::filter_forecast_time(data.frame(time=1:10, val=11:20), NULL)
filter_forecast_time <- function(df, forecast_time) {
    if(is.null(forecast_time)) {
        return(df)
    } else {
        return(df |> dplyr::filter(time >= forecast_time))
    }
}


#' Join a forecast and observations into a single data frame
#'
#' Add an observations column to the forecast data frame containing
#'  observations for each time point.
#'
#' @param df The forecast data frame.
#' @param obs The observations data frame.
#'
#' @returns The forecast data frame with an additional `obs` column containing observations.
#' @autoglobal
#'
#' @examples
#' # data.frame(time=1:3, raw=4:6, obs=8:10)
#' casteval:::join_fcst_obs(data.frame(time=1:3, val=4:6), data.frame(time=0:4, val_obs=7:11))
#' 
#' # remove rows with missing observations
#' # data.frame(time=3, val_q50=6, val_obs=7)
#' casteval:::join_fcst_obs(
#'   data.frame(time=1:3, val_q50=4:6),
#'   data.frame(time=2:3, val_obs=c(NA,7))
#' )
#' 
#' # default behaviour is to error if observations are missing
#' try(casteval:::join_fcst_obs(
#'   data.frame(time=1:3, val_q50=4:6),
#'   data.frame(time=2:3, val_obs=c(NA,7))
#' ))
join_fcst_obs <- function(df, obs) {
    # this function does very little input validation because
    # it is meant to be used inside functions like `accuracy()` and `log_score()`,
    # where the forecast and observations are already validated.

    # check that no collisions will occur
    if("val_obs" %in% colnames(df)) {
        stop("`val_obs` column already present in forecast data frame")
    }

    # filter out NAs
    obs <- obs |> dplyr::filter(!is.na(val_obs))

    df <- join_data(df, obs)

    if(nrow(df) == 0) {
        stop("forecast and observations data do not share any time points")
    }

    df
}


#' Join two data frames
#'
#' Join two data frames by time and grouping columns.
#'
#' @param df1 The first data frame
#' @param df2 The second data frame
#'
#' @details
#' `df1` and `df2` should both have a `time` column and the exact same grouping columns.
#' 
#' @returns The inner join of the two data frames
#' @autoglobal
#'
#' @examples
#' #TODO
join_data <- function(df1, df2) {
    joinby <- c("time", get_group_cols(df1))
    dplyr::inner_join(df1, df2, by=joinby)
}


#' Get a row of a date frame for a given time
#'
#' Given a forecast/observations data frame and a time,
#'  isolate the row with the given time.
#'
#' @param df A data frame with a `time` column.
#' @param t A time.
#'
#' @returns A data frame with one row, where the time equals `t`.
#'  Raises error if there isn't exactly one row which satisfies this.
#' @autoglobal
#'
#' @examples
#' # no rows with time==4
#' try(casteval:::get_time_point(data.frame(time=1:3,val=4:6), 4))
#' 
#' # 2 rows with time==2
#' try(casteval:::get_time_point(data.frame(time=c(1,2,2,3), val=4:7), 2))
#' 
#' # data.frame(time=2, val=5, val_mean=8)
#' casteval:::get_time_point(data.frame(time=1:3, val=4:6, val_mean=7:9), 2)
get_time_point <- function(df, t) {
    #TODO add a flag for allowing multiple matches
    df <- dplyr::filter(df, time == t)

    if(nrow(df) == 0) {
        stop("no rows in data frame with given time")
    }

    df
}

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
#' casteval:::get_time_type(data.frame(time=lubridate::as_date(1:3)))
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


#' Calcuate relative/absolute time specified by user
#'
#' Calculate the time according to the `at`/`after` params passed by the user.
#' Helper for `log_score()` and `plot_KDE()`.
#'
#' @template fcst
#' @param at (Optional) See `?log_score`
#' @param after (Optional) See `?log_score`
#'
#' @returns A time of the same type as those in `fcst`
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(data.frame(time=1:3, val=4:6), forecast_time=2)
#' # 1
#' casteval:::calc_specified_time(fc, at=1)
#' 
#' # 2+1=3
#' casteval:::calc_specified_time(fc, after=1)
calc_specified_time <- function(fcst, at=NULL, after=NULL) {
    if(!is.null(at) && !is.null(after)) { # mutually exclusive
        stop("`at` and `after` parameters cannot both be provided")
    }
    
    # only one provided, calculate time
    if(!is.null(at)) {
        tryCatch(
            validate_time(at, fcst),
            error = \(e) stop("type of `at` must match type of forecast times")
        ) 
        t <- at
    }
    else if(!is.null(after)) {
        if(!is.numeric(after)) {
            stop("`after` not numeric")
        }
        if(is.null(fcst$forecast_time)) {
            stop("`after` cannot be used if `fcst$forecast_time` is NULL")
        }
        t <- fcst$forecast_time + after
    }
    # neither provided, error
    else {
        stop("either `at` or `after` must be provided")
    }

    t
}


#' Apply facets to plot
#'
#' Apply the given facets to the given plot
#'
#' @param plot A `ggplot` object
#' @param plotting_groups A character vector of up to 2 group column names
#'
#' @details
#' 
#' - If `plotting_groups` is empty, `plot` is returned unchanged
#' - If `plotting_groups` has one element, `ggplot2::facet_wrap()` is used
#' - If `plotting_groups` has two elements, `ggplot2::facet_grid()` is used
#' 
#' @returns desc
#' @autoglobal
#'
#' @examples
#' obs <- groups_obs |> dplyr::filter(grp_scenario==1)
#' NULL |> plot_observations(obs) |> casteval:::apply_facets(c("grp_variable"))
#' NULL |> plot_observations(obs) |> casteval:::apply_facets(c("grp_variable", "grp_province"))
apply_facets <- function(plot, plotting_groups) {
    len <- length(plotting_groups)
    if(len == 0) {
       return(plot)
    } else if(len == 1) {
        # turn a string into `ggplot2`'s requested facet syntax
        # https://stackoverflow.com/questions/11028353/passing-string-variable-facet-wrap-in-ggplot-using-r
        fac <- as.formula(glue::glue("~{plotting_groups[[1]]}"))
        return(plot + ggplot2::facet_wrap(fac))
    } else if(len == 2) {
        # see above
        a <- plotting_groups[[1]]
        b <- plotting_groups[[2]]
        fac <- as.formula(glue::glue("{a} ~ {b}"))
        return(plot + ggplot2::facet_grid(fac))
    } else {
        stop("more than 2 plotting groups provided to `apply_facets()`")
    }
}