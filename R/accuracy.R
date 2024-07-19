#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# inside quantile range / # total) of the forecast.
#'  Raw data and/or provided quantiles will be used to compute the quantile range.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @param quants (Optional) A vector of two percentages from 0 to 100,
#'  the high and low end of the quantile range.
#'  Defaults to `c(2.5, 97.5)`.
#' @template summarize
#'
#' @returns A number from 0 to 1,
#'  the rate at which the observations were inside the specified quantile range
#' @export
#' @autoglobal
#'
#' @examples
#' # calculate quantiles and accuracy from raw data
#' # returns 2/3
#' accuracy(
#'   create_forecast(dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=4:12)),
#'   data.frame(time=1:3, val_obs=c(5, 7.4, 11.6)),
#'   quants=c(25, 75)
#' )
#' 
#' # provide two quantiles and specify them in `quants`
#' # returns 1/3
#' accuracy(
#'   create_forecast(dplyr::tibble(
#'     time=1:3, val_q25=4:6, val_q50=7:9, val_mean=100:102, val_q75=200:202
#'   )),
#'   data.frame(time=1:3, val_obs=c(4, 201, 1000)),
#'   quants=c(25,50)
#' )
#'
#' # return a data frame with a logical `score` column
#' accuracy(
#'   create_forecast(dplyr::tibble(
#'     time=1:3, val_q2.5=4:6, val_q50=7:9, val_mean=100:102, val_q97.5=200:202
#'   )),
#'   data.frame(time=1:3, val_obs=c(4, 201, 1000)),
#'   summarize=FALSE
#' )
accuracy <- function(fcst, obs, quants=c(2.5, 97.5), summarize=TRUE) {
    if(is.null(quants)) {
        stop("TODO")
    }
    validate_quant_range(quants)

    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # get the low and high quantiles and name the columns correspondingly
    low <- get_quantile(df, quants[[1]]) |> dplyr::rename(low=quant)
    high <- get_quantile(df, quants[[2]]) |> dplyr::rename(high=quant)

    # attach quant columns to obs data frame
    obs <- obs |> dplyr::inner_join(low, dplyr::join_by(time)) |> dplyr::inner_join(high, dplyr::join_by(time))
    if(nrow(obs) == 0) {
        stop("observations and forecast data share no time points")
    }

    # calculate accuracy
    obs <- obs |> mutate(score=dplyr::between(val_obs, low, high))

    if(!summarize) {
        return(obs |> dplyr::select(time, val_obs, score))
    }

    # calculate success rate (aka accuracy)
    mean(obs$score)
}

#' Validate quantile range vector
#'
#' Helper function for accuracy(). Performs input validation on its `quants` parameter.
#'
#' @param quants Same as the `quants` parameter passed to accuracy()
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_range(c(50, 70))
#' 
#' # invalid
#' try(casteval:::validate_quant_range(c(70, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_range(c(-1, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_range(c(50,60,70)))
#' 
#' # invalid
#' try(casteval:::validate_quant_range("50, 60"))
validate_quant_range <- function(quants) {
    if(!is.numeric(quants)) {
        stop("`quants` must be either NULL or vector of 2 numbers")
    }

    if(length(quants) != 2) {
        stop("`quants` vector must have length 2")
    }

    low <- quants[[1]]
    high <- quants[[2]]

    if(low >= high) {
        stop("`quants[[1]]` must be less than `quants[[2]]`")
    }

    if(low < 0 || low > 100 || high < 0 || high > 100) {
        stop("`quants[[1]]` and `quants[[2]]` must be between 0 and 100, inclusive")
    }

    invisible(NULL)
}