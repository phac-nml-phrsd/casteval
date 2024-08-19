# Continuous Ranked Probability Score


#' Compute Continuous Ranked Probability Score for forecast
#'
#' Given a forecast and set of observations, compute the
#' Continuous Ranked Probability Score (CRPS) for every time point.
#' 
#' @details
#' For a given distribution `f` and observation `y`,
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
#' @template grouping
#' @export
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=c(1,2,3),
#'   vals=list(c(1,1,1), c(2,2,2), c(3,3,3), c(4,4,4), c(5,5,5)), forecast_time=1)
#' )
#' 
#' crps(fc, data.frame(time=1:3, val_obs=c(3,4,5)), summarize=FALSE)
#' 
#' crps(fc, data.frame(time=1:3, val_obs=c(3,4,5)), at=2)
crps <- function(fcst, obs, summarize=TRUE, at=NULL, after=NULL) {
    # validate
    validate_fcst_obs_pair(fcst, obs)
    if(!"val" %in% colnames(fcst$data)) {
        stop("crps() requires raw forecast data (`val`)")
    }

    # filter
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # join & group
    df <- df |>
        join_fcst_obs(obs) |>
        dplyr::group_by(time) |>
        group_all(.add=TRUE)

    # compute the CRPS score
    df <- df |> dplyr::summarize(score=scoringRules::crps_sample(val_obs[[1]], val), val_obs=val_obs[[1]], .groups="drop")

    if(!summarize) {
        return(df)
    }

    t <- calc_specified_time(fcst, at, after)
    df <- df |> dplyr::filter(time==t)
    if(nrow(df) == 0) {
        stop(glue::glue("score was not calculated for time {t}"))
    }

    if(has_groups(df)) {
        return(df)
    }
    else {
        return(df$score[[1]])
    }
}