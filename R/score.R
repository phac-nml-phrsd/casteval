# generic user-friendly scoring wrappers
# the actual scoring functions can be found in `accuracy.R`, `log_score.R`, `bias.R`, etc.


#' Score forecasts
#'
#' Score a forecast against some observations using a given scoring function.
#' Additional parameters can be passed along to the scoring function.
#' 
#' @details
#' See `vignette("scoring")` for a complete list of scoring functions available, along with the details of their use.
#'
#' @param fcsts A single forecast object, or a list of forecast objects
#' @param obs An observations data frame
#' @param fun A scoring function. `fcst` and `obs` will be passed to it
#' @param ... Additional parameters which will be passed along to `fun`
#'
#' @returns If `fcsts` is a forecast object, a single score will be returned.
#' If `fcsts` is a list of forecast objects, then a list of scores will be returned.
#' @export
#' @autoglobal
#'
#' @examples
#' fc1 <- create_forecast(data.frame(time=1:5, val=6:10))
#' fc2 <- create_forecast(data.frame(time=1:5, val=7:11))
#' 
#' obs <- data.frame(time=1:5, val_obs=c(8,8,8,8,8))
#' 
#' score(fc1, obs, bias)
#' score(fc2, obs, bias)
#' score(list(fc1, fc2), obs, bias)
score <- function(fcsts, obs, fun, ...) {
    if(is_forecast(fcsts)) {
        return(fun(fcsts, obs, ...))
    }
    else {
        # check `fcsts` is a list
        if(!is.list(fcsts)) {
            stop("`fcsts` must be a single forecast object or list of forecast objects")
        }

        # validate every element of `fcsts`
        valid <- fcsts |> purrr::map(is_valid_forecast) |> as.logical()
        if(!all(valid)) {
            i <- which(!valid)[[1]]
            stop(glue::glue("forecast number {i} is not valid"))
        }

        # score
        return(fcsts |> purrr::map(\(fc) fun(fc, obs, ...)))
    }
}
