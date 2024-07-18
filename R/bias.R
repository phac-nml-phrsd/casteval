#' Calculate forecast bias
#'
#' Given a forecast and a set of observations, compute the bias of the forecast's predictions.
#' `bias()` looks for forecast data in the following order:
#' 1. raw unsummarized data (`val`)
#' 2. mean (`val_mean`)
#' 3. median (`val_q50`)
#' 
#' It uses the first that it finds to calculate bias,
#' by assigning 1 for overprediction, 0 for equality, and -1 for underprediction.
#' It then summarizes the score by taking the mean of the assigned values.
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
    # validate and filter
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    # place the data to be scored in `prediction` column
    if("val" %in% fcst$data_types) { # try raw data
        df <- df |> dplyr::rename(prediction=val)
    } else if("val_mean" %in% fcst$data_types) { # then try mean data
        df <- df |> dplyr::rename(prediction=val_mean)
    } else if("val_q50" %in% colnames(fcst$data)) { # then try median data
        df <- df |> dplyr::rename(prediction=val_q50)
    } else { # then error
        stop("unsummarized, mean, or median forecast values required to compute bias")
    }

    # join observations
    df <- df |> join_fcst_obs(obs) |>
        dplyr::mutate(score=sign(prediction - val_obs))

    if(!summarize) {
        df <- df |> dplyr::group_by(time) |> dplyr::sumarize(val_obs=val_obs[[1]], score=mean(score))
        return(df)
    }

    mean(df$score)
}
