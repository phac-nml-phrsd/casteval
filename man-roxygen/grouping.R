#' @section Grouping:
#' If `summarize=FALSE` is passed, the resulting scores will be grouped by time.
#' If group columns are present, the data will be grouped by the group columns before scoring.
#' In either case, the return value will instead be a data frame with columns for the time, observation, score, and group columns if they exist.
#' See `vignette("casteval")` for more details.
