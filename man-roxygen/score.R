#' @param score (Optional) A scoring function.
#'  The function will be used to score `fcst` against `obs`.
#'  A scoring function should accept a forecast object, an observations data frame, as well as a `summarize` argument.
#'  See `?accuracy`, `?log_score` for examples.
#'  See `vignette(topic='casteval', package='casteval')` for details.
