#' @param summarize A boolean. If TRUE, a single number will be returned as the score for the forecast.
#'  If FALSE, a data frame with `time` and `score` columns (and possibly others) will be returned,
#'  containing the intermediate values for each time point used to calculate the score.
#'  The type of the `score` column can be anything (numbers, booleans, etc.).
#'  This can be used by graphing functions to color code observations, for example.