#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# within confidence interval / # total) of the forecast.
#'  If quantiles are provided, they will be used to compute the accuracy.
#'  If raw data is provided, quantiles will be calculated according to the `interval` parameter.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @param interval (Optional) A vector of two numbers from 0 to 100.
#'  If `fcst` contains quantile data then the corresponding quantile columns will be
#'  used as the confidence interval.
#'  If `fcst` contains raw data then the corresponding quantiles will be calculated and
#'  used as a confidence interval.
#' @template summarize
#'
#' @returns A number from 0 to 1,
#'  the rate at which the observations were inside the specified confidence interval
#' @export
#' @autoglobal
#'
#' @examples
#' # calculate quantiles and accuracy from raw data
#' # returns 2/3
#' accuracy(
#'   create_forecast(dplyr::tibble(time=1:3, raw=list(4:6, 7:9, 10:12))),
#'   data.frame(time=1:3, obs=c(5, 7.4, 11.6)),
#'   interval=c(25, 75)
#' )
#' 
#' # provide two quantiles and specify them in `interval`
#' # returns 1/3
#' accuracy(
#'   create_forecast(dplyr::tibble(
#'     time=1:3, quant_25=4:6, quant_50=7:9, mean=100:102, quant_75=200:202
#'   )),
#'   data.frame(time=1:3, obs=c(4, 201, 1000)),
#'   interval=c(25,50)
#' )
#' 
#' # implicitly use the outermost quantiles (must be symmetric around the median)
#' accuracy(
#'   create_forecast(dplyr::tibble(
#'     time=1:3, quant_25=4:6, quant_50=7:9, mean=100:102, quant_75=200:202
#'   )),
#'   data.frame(time=1:3, obs=c(4, 201, 1000)),
#' )
accuracy <- function(fcst, obs, interval=NULL, summarize=TRUE) {
    # TODO document `summarize` parameter in vignette
    # TODO redo using get_quantile()
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if("raw" %in% fcst$data_types) {
        # perform input validation
        if(is.null(interval)) {
            interval <- c(2.5, 97.5)
            message("interval not provided. defaulting to `c(2.5, 97.5)`")
        }
        validate_interval(interval)

        df <- remove_raw_NAs(df)

        # compute quantiles using raw & interval
        lows <- raw2quant(df$raw, interval[[1]])
        highs <- raw2quant(df$raw, interval[[2]])
        df <- df |> dplyr::mutate(time, low=lows, high=highs, .keep="none")

        lowname <- "low"
        highname <- "high"
    } else if("quant" %in% fcst$data_types) {
        quants <- get_quant_percentages(df)

        # can't do anything with a single quantile
        if(length(quants) < 2) {
            stop("2 or more quantiles required to calculate accuracy")
        }

        if(is.null(interval)) {
            # if quantile columns are provided but `quants` is NULL,
            # we select the two outermost quantiles provided,
            # and require that they be equidistant from the median.
            # e.x. 25% to 75% is acceptable, but not 25% to 60%
            low <- min(quants)
            high <- max(quants)
            # use all.equal() to deal with floating point errors
            if(all.equal(50-low, high-50) != TRUE) {
                stop("outermost quantiles must be equidistant from 50th percentile")
            }

            lowname <- quant_name(low)
            highname <- quant_name(high)
        } else {
            validate_interval(interval)

            lowname <- quant_name(interval[[1]])
            highname <- quant_name(interval[[2]])

            # confirm that the corresponding columns exist
            if(! lowname %in% colnames(df)) {
                stop(paste0("column named `", lowname, "` not in data frame"))
            }
            if(! highname %in% colnames(df)) {
                stop(paste0("column named `", highname, "` not in data frame"))
            }
        }
    } else {
        stop("`raw` or `quant_*` columns required to calculate accuracy")
    }

    # isolate/rename the time and relevant quantile columns
    temp <- df |> dplyr::select(time, low=dplyr::all_of(lowname), high=dplyr::all_of(highname))
    
    # check for any NAs that make proceeding impossible without removing rows
    if(NA %in% temp$low || NA %in% temp$high) {
        stop("some forecast quantiles are NA")
    }

    # join & calculate accuracy
    df <- join_fcst_obs(df, obs) |>
        dplyr::mutate(score=dplyr::between(obs, temp$low, temp$high))
    if(!summarize) {
        return(df)
    }

    # calculate success rate (aka accuracy)
    mean(df$score)
}

#' Validate quantile interval vector
#'
#' Helper function for accuracy(). Performs input validation on its `interval` parameter.
#'
#' @param interval Same as the `interval` parameter passed to accuracy()
#'
#' @returns NULL if valid. Error otherwise.
#' @autoglobal
#'
#' @examples
#' # valid
#' casteval:::validate_interval(c(50, 70))
#' 
#' # invalid
#' try(casteval:::validate_interval(c(70, 50)))
#' 
#' # invalid
#' try(casteval:::validate_interval(c(-1, 50)))
#' 
#' # invalid
#' try(casteval:::validate_interval(c(50,60,70)))
#' 
#' # invalid
#' try(casteval:::validate_interval("50, 60"))
validate_interval <- function(interval) {
    if(!is.numeric(interval)) {
        stop("`interval` must be either NULL or vector of 2 numbers")
    }

    if(length(interval) != 2) {
        stop("`interval` vector must have length 2")
    }

    low <- interval[[1]]
    high <- interval[[2]]

    if(low >= high) {
        stop("`interval[[1]]` must be less than `interval[[2]]`")
    }

    if(low < 0 || low > 100 || high < 0 || high > 100) {
        stop("`interval[[1]]` and `interval[[2]]` must be between 0 and 100, inclusive")
    }

    NULL
}