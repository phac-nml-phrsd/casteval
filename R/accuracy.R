#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# inside quantile interval / # total) of the forecast.
#'  Raw data and/or provided quantiles will be used to compute the quantile interval.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @param quant_pairs (Optional) A list of pairs of numbers between 0 and 100,
#' or a single pair of numbers between 0 and 100.
#' If provided, the score for each corresponding pairs of quantiles will be calculated.
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
#' # return a data frame with a `time`, `pair`, `val_obs`, and `score` columns
#' accuracy(fc2, obs2, summarize=FALSE)
accuracy <- function(fcst, obs, quant_pairs=NULL, summarize=TRUE) {
    validate_fcst_obs_pair(fcst, obs)
    fcst$data <- filter_forecast_time(fcst$data, fcst$forecast_time)

    quant_pairs <- parse_quant_pairs(quant_pairs, fcst$data)
    message(glue::glue("Scoring accuracy using quantile pairs {toString(quant_pairs)}"))

    scores <- quant_pairs |>
        # get the score data frame for each pair and give it a `pair` numbering
        purrr::imap(\(pair, i)
            accuracy_help(fcst, obs, pair) |> dplyr::mutate(pair=i)
        ) |>
        # combine them into one data frame
        dplyr::bind_rows()

    if(!summarize) {
        return(scores)
    }

    # calculate accuracy
    scores <- scores |> dplyr::group_by(pair) |> dplyr::summarize(n=dplyr::n(), acc=mean(score))
    # print how many points used
    message(glue::glue("Used {scores$n[[1]]} time points to calculate accuracy"))
    scores$acc
}


#' Calculate accuracy given a single quantile pair
#'
#' Helper for `accuracy()`.
#' Calculates accuracy for a single quantile pair and returns the unsummarized result.
#'
#' @param fcst A forecast object.
#' @param obs An observations data frame.
#' @param pair A valid quantile pair
#'
#' @returns A data frame with `time`, `val_obs`, and `score` columns
#' @autoglobal
#'
#' @examples
#' # See `?accuracy`
accuracy_help <- function(fcst, obs, pair) {
    low <- get_quantile(fcst$data, pair[[1]]) |> dplyr::rename(low=quant)
    high <- get_quantile(fcst$data, pair[[2]]) |> dplyr::rename(high=quant)

    # attach quant columns to obs data frame
    obs <- obs |> dplyr::inner_join(low, dplyr::join_by(time)) |> dplyr::inner_join(high, dplyr::join_by(time))
    if(nrow(obs) == 0) {
        stop("observations and forecast data share no time points")
    }

    # calculate accuracy
    obs <- obs |> dplyr::mutate(score=dplyr::between(val_obs, low, high))

    return(obs |> dplyr::select(time, val_obs, score))
}


#' `accuracy()` function factory
#'
#' Given the desired quantile pairs,
#' create a function wrapping `accuracy()` which passes those quantile pairs to it.
#'
#' @param quant_pairs See `?accuracy`
#'
#' @returns desc
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=1:3,
#'   vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
#' ))
#' obs <- data.frame(time=1:3, val_obs=5:7)
#' 
#' acc <- make_accuracy(c(5,95))
#' acc(fc, obs)
#' 
#' plot_forecast(fc, obs, score=make_accuracy(c(25,75)))
make_accuracy <- function(quant_pairs) {
    function(...) {
        accuracy(..., quant_pairs=quant_pairs)
    }
}