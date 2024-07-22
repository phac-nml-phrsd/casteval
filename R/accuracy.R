#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# inside quantile interval / # total) of the forecast.
#'  Raw data and/or provided quantiles will be used to compute the quantile interval.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @param quant_pairs (Optional) A list of pairs of numbers between 0 and 100.
#' If provided, the score for each corresponding pair of quantiles will be calculated.
#' If not provided, it will default to every symmetrical pair of quantiles that can be found in `fcst`,
#' ordered from widest to narrowest (e.x. the 25% and 75% quantiles are symmetrical).
#' 
#' If `summarize` is `FALSE`, an additional column named `pair` will indicate which pair of quantiles each row represents.
#' If `summarize` is `TRUE`, the output will be a vector with the same length as `quant_pairs`,
#' containing the respective score for each pair.
#' @template summarize
#'
#' @returns A number from 0 to 1,
#'  the rate at which the observations were inside the specified quantile interval
#' @export
#' @autoglobal
#'
#' @examples
#' # forecast with raw data
#' fc1 <- create_forecast(dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=4:12))
#' obs1 <- data.frame(time=1:3, val_obs=c(5, 7.4, 11.6))
#' 
#' # calculate quantiles and accuracy from raw data
#' # returns 2/3
#' accuracy(fc1, obs1, quant_pairs=list(c(25, 75)))
#' 
#' # forecast with quantile data
#' fc2 <- create_forecast(dplyr::tibble(
#'   time=1:3, val_q5=1:3, val_q25=4:6, val_q50=100:102, val_q75=200:202, val_q95=203:205
#' ))
#' obs2 <- data.frame(time=1:3, val_obs=c(4, 202, 1000))
#' 
#' # infer quantile pairs from forecast data (`c(5,95)` and `c(25, 75)`)
#' # returns c(2/3, 1/3)
#' accuracy(fc2, obs2)
#'
#' # return a data frame with a `time`, `pair`, and `score` columns
#' accuracy(fc2, obs2, summarize=FALSE)
accuracy <- function(fcst, obs, quant_pairs=c(2.5, 97.5), summarize=TRUE) {
    if(is.null(quants)) {
        stop("TODO")
    }

    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if(is.null(quants)) { # default quants -> infer from forecast
        quants <- pair_quantiles(get_quant_percentages(fcst$data))$paired
        if(length(quants) == 0) {
            stop("`quants` is NULL and `fcst` does not contain usable quantile data for scoring accuracy")
        }
    }
    else { # provided quants
        # check for numeric(0) shenanigans
        if(length(quants) == 0) {
            stop("no quantile pairs provided")
        }
        # validate
        quants |> purrr::walk(validate_quant_interval)
    }

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

#' Validate quantile interval vector
#'
#' Helper function for accuracy(). Performs input validation on its `quant_pairs` parameter.
#'
#' @param pair An element of `quant_pairs`
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_quant_interval(c(50, 70))
#' 
#' # invalid
#' try(casteval:::validate_quant_interval(c(70, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_interval(c(-1, 50)))
#' 
#' # invalid
#' try(casteval:::validate_quant_interval(c(50,60,70)))
#' 
#' # invalid
#' try(casteval:::validate_quant_interval("50, 60"))
validate_quant_interval <- function(pair) {
    if(!is.numeric(pair)) {
        stop("`pair` must be either NULL or vector of 2 numbers")
    }

    if(length(pairs) != 2) {
        stop("quantile pair must have length 2")
    }

    low <- pairs[[1]]
    high <- pairs[[2]]

    if(low >= high) {
        stop("`first quantile in pair must be less than second quantile")
    }

    if(low < 0 || low > 100 || high < 0 || high > 100) {
        stop("`quantiles in pair must be between 0 and 100, inclusive")
    }

    invisible(NULL)
}