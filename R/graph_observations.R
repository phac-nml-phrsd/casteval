#' Intelligently graph observations
#'
#' Graph observation points. If provided with a forecast,
#'  the points can be color-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame. If it contains a `score` column,
#'  the observation points will be color-coded according to the scores.
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(dplyr::tibble(
#'   time=1:3,
#'   raw=list(4:6, 7:9, 10:12)
#' ))
#' obs <- data.frame(time=1:3, obs=c(5,9,13))
#' 
#' # graph observations on their own
#' casteval:::graph_observations(NULL, obs)
#' 
#' # graph observations alongside forecast data
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), obs)
#' 
#' # graph observations alongside forecast data, and color-code by score
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), neglog(fc, obs))
graph_observations <- function(graph=NULL, obs) {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) { 
        # TODO print debugging reveals that this code is covered by tests, but covr::report() marks it as untested. This is likely a problem with either vdiffr or covr
        return(
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs, color=score), obs)
        )
    } else {
        return(
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=obs), obs)
        )
    }
}