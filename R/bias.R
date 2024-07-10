#' Calculate forecast bias
#'
#' 
#'
#' @template fcst
#' @param obs An observations data frame.
#' @template summarize
#'
#' @returns A number between -1 and 1, inclusive. -1 means 100% underprediction and 1 means 100% overprediction.
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
bias <- function(fcst, obs, summarize=TRUE) {
    # TODO fix summarize=FALSE output to make it work with graphing (must not be long form)
    # validate and filter
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # if raw present, compute the bias using all data points
    # TODO verify if this is sound
    if("raw" %in% fcst$data_types) {
        # convert to long form (has to be done before join_fcst_obs() in the current state of the package)
        # TODO fix this once raw format changed
        df <- df |> remove_raw_NAs() |> wide2long()

    } else if("mean" %in% fcst$data_types) { # next look for mean data
        df <- df |> dplyr::mutate(raw=mean)
    } else if("quant_50" %in% colnames(fcst$data)) {
        df <- df |> dplyr::mutate(raw=quant_50)
    } else {
        stop("raw data, mean, or median required to compute bias")
    }

    # map overpredict to +1 and underpredict to -1 (and equality to 0)
    # it would be more appropriate to call the column `prediction` instead of `raw`
    # but we call it `raw` to make it compatible with join_fcst_obs()
    df <- df |> join_fcst_obs(obs) |>
        dplyr::mutate(score=sign(raw - obs))

    if(!summarize) {
        return(df |> dplyr::select(time, obs, score))
    }

    mean(df$score)
}
