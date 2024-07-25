#' @param summarize A boolean, defaults to TRUE. If TRUE, a single number will be returned as the score for the forecast.
#'  If FALSE, a data frame with columns named `time`, `val_obs`, and `score` will be returned,
#'  containing the scores for each individual time point.
#'  This can be used by plotting functions to colour-code observations, for example.
