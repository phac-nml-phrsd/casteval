#' Calculate forecast bias
#'
#' Given a forecast and a set of observations, compute the bias of the forecast's predictions.
#' `bias()` looks for forecast data in the following order:
#' 1. raw data (`val`)
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
#' obs <- data.frame(time=1:5, val_obs=rep(10,5))
#' 
#' # # a forecast with bias on individual days, but no overall bias
#' fc1 <- create_forecast(dplyr::tibble(
#'   time=c(1,1,2,2,3,3,4,4,5,5),
#'   val=c(9, 9, 9, 10, 10, 10, 10, 11, 11, 11)
#' ))
#' 
#' bias(fc1, obs, summarize=FALSE)
#' 
#' bias(fc1, obs)
#' 
#' # A forecast with an underprediction bias
#' fc2 <- create_forecast(data.frame(
#'   time=c(1,1,1,2,2,2,3,3,3),
#'   val=c(9,9,9,10,10,10,11,9,9)
#' ))
#' 
#' bias(fc2, obs, summarize=FALSE)
#' 
#' bias(fc2, obs)
bias <- function(fcst, obs, summarize=TRUE) {
    # validate and filter
    validate_fcst_obs_pair(fcst, obs)
    df <- filter_forecast_time(fcst$data, fcst$forecast_time)

    cols <- colnames(fcst$data)

    # place the data to be scored in `prediction` column
    if("val" %in% cols) { # try raw data
        df <- df |> dplyr::rename(prediction=val)
    } else if("val_mean" %in% cols) { # then try mean data
        df <- df |> dplyr::rename(prediction=val_mean)
    } else if("val_q50" %in% cols) { # then try median data
        df <- df |> dplyr::rename(prediction=val_q50)
    } else { # then error
        stop("raw, mean, or median forecast values required to compute bias")
    }

    # join observations
    df <- df |> join_fcst_obs(obs) |>
        dplyr::mutate(score=sign(prediction - val_obs))

    if(!summarize) {
        df <- df |> dplyr::group_by(time) |> dplyr::summarize(val_obs=val_obs[[1]], score=mean(score))
        return(df)
    }

    mean(df$score)
}
