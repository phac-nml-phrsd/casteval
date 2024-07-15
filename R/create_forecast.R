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


create_forecast <- function(dat, name=NULL, forecast_time=NULL) {
    # TODO grouping & corresponding input formats
    # TODO warn about extra columns

    #check empty
    #check quant symmetry
    #check quant order
    message("Validating input data...")
    if(is.data.frame(dat)) {
        
    }
}