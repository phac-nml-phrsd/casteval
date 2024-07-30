# Continuous Ranked Probability Score


#' Compute Continuous Ranked Probability Score for forecast
#'
#' Given a forecast and set of observations, compute the
#' 
#' Continuous Ranked Probability Score (CRPS) for every time point.
#' The CRPS for a given distribution `f` and observation `y`,
#' let `F` be the CDF of `f`. Then the CRPS is the integral of the square
#' of `F(x) - H(x - y)`, where `H` is the Heaviside function.
#' 
#' The CDF is obtained using an emperical distribution function on the forecast data.
#'
#' @template fcst
#' @param obs An observations data frame.
#' @template at_after
#' @template summarize
#'
#' @template at_after_returns
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
crps <- function(fcst, obs, at=NULL, after=NULL, summarize=TRUE) {
    # validate
    validate_fcst_obs_pair(fcst, obs)
    if(!"val" %in% colnames(fcst$data)) {
        stop("crps() requires raw forecast data (`val`)")
    }

    # filter
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # join & group
    df <- df |>
        dplyr::select(time, val) |>
        join_fcst_obs(obs) |>
        dplyr::group_by(time)

    # compute the CRPS score
    df <- df |> dplyr::summarize(score=scoringRules::crps_sample(val_obs[[1]], val), val_obs=val_obs[[1]])

    if(!summarize) {
        return(dplyr::select(df, time, val_obs, score))
    }

    t <- calc_specified_time(fcst, at, after)
    df <- df |> dplyr::filter(time==t)
    if(nrow(df) == 0) {
        stop(glue::glue("score was not calculated for time {t}"))
    }

    df$score[[1]]
}