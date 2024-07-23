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
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if(is.null(quant_pairs)) { # default quant_pairs -> infer from forecast
        quant_pairs <- pair_quantiles(get_quant_percentages(fcst$data))$paired
        if(length(quant_pairs) == 0) {
            stop("could not infer quantile pairs from forecast data")
        }
    }
    else if(is.numeric(quant_pairs)) { # provided a single pair
        validate_quant_pair(quant_pairs)
        quant_pairs <- list(quant_pairs)
    }
    else if(is.list(quant_pairs)) { # provided list of pairs
        if(length(quant_pairs) == 0) {
            stop("`quant_pairs` is empty")
        }
        # validate 
        quant_pairs |> purrr::walk(validate_quant_pair)
    }
    else {
        stop("`quant_pairs` must be either NULL, pair of quantiles, or list of pairs of quantiles")
    }

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

    # calculate success rate (aka accuracy)
    scores |> dplyr::group_by(pair) |> dplyr::summarize(acc=mean(score)) %>% .$acc
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