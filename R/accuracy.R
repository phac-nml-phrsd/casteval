#' Get accuracy from quantiles
#'
#' Given a forecast and set of observations,
#'  compute the accuracy (# within confidence interval / # total) of the forecast.
#'
#' @param fcst The forecast (see `create_forecast()` output).
#'  If quantiles are provided, they will be used to compute the accuracy.
#'  If raw data is provided, quantiles will be calculated according to the `interval` parameter.
#' @param obs The observations data frame.
#' @param interval Either NULL or a vector of two numbers from 0 to 100.
#'  If `fcst` contains quantile data then the corresponding quantile columns will be
#'  used as the confidence interval.
#'  If `fcst` contains raw data then the corresponding quantiles will be calculated and
#'  used as a confidence interval.
#'
#' @returns A number from 0 to 1,
#'  the rate at which the observations were inside the specified confidence interval
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
accuracy <- function(fcst, obs, interval=NULL) {
    #TODO deal with NA values
    validate_fcst_obs_pair(fcst, obs)

    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    if("raw" %in% fcst$data_types) {
        # perform input validation
        if(is.null(interval)) {
            stop("`interval` parameter required for computing accuracy from raw data")
        }
        validate_interval(interval)

        # compute quantiles using raw & interval
        df |> dplyr::mutate(low = quantile(raw, interval[[1]])[[1]], high = quantile(raw, interval[[2]])[[1]])
        
        lowname <- "low"
        highname <- "high"
    } else if("quant" %in% fcst$data_types) {
        quants <- get_quantiles(df)

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
            if(all.equal(50-low, high-50)) {
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

    # compare observations against specified confidence interval
    df <- df |>
        # isolate/rename the time and relevant quantile columns
        dplyr::select(time, low=lowname, high=highname) |>
        # join observations by time into `obs` column
        dplyr::inner_join(obs |> dplyr::rename(obs=raw), dplyr::join_by(time)) |>
        # flag the rows where the observations are within the confidence interval
        dplyr::mutate(success=dplyr::between(obs, low, high)) |>

    # calculate success rate (aka accuracy)
    mean(df$success)
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

    if(low < 0 || low > 100 | high < 0 || high > 100) {
        stop("`interval[[1]]` and `interval[[2]]` must be between 0 and 100, inclusive")
    }

    NULL
}