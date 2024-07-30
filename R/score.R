# generic user-friendly scoring wrappers
# the actual scoring functions can be found in `accuracy.R`, `log_score.R`, `bias.R`, etc.


# #' Score a forecast
# #'
# #' Score a forecast against some observations using a given scoring function.
# #' Additional parameters can be passed along to the scoring function.
# #'
# #' @template fcst
# #' @param obs An observations data frame
# #' @param fun A scoring function. `fcst` and `obs` will be passed to it
# #' @param ... Additional parameters which will be passed along to `fun`
# #'
# #' @returns The output of `fun(fcst, obs, ...)`
# #' @export
# #' @autoglobal
# #'
# #' @examples
# #' 
# score_forecasts <- function(fcst, obs, fun, ...) {

# }
