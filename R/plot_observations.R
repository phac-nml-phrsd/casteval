#' Intelligently graph observations
#'
#' Graph observation points. If scores are provided alongside the observations,
#'  the plot can be colour-coded to convey information about score, accuracy, etc.
#'
#' @template graph
#' @param obs An observations data frame. If it contains a `score` column,
#'  the observation points will be colour-coded according to the scores.
#'  Otherwise, the `alpha` and `colour` parameters will determine the opacity and colour of the points.
#' @template ggplot2params
#'
#' @returns A ggplot object.
#' @autoglobal
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=1:3,
#'   vals=list(c(4,7,10), c(5,8,11), c(6,9,12))
#' ))
#' obs <- data.frame(time=1:3, val_obs=c(5,9,13))
#' 
#' # graph observations on their own
#' casteval:::graph_observations(NULL, obs)
#' 
#' # graph observations alongside forecast data
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), obs)
#' 
#' # graph observations alongside forecast data, and colour-code by score
#' casteval:::graph_observations(casteval:::graph_ensemble(NULL, fc), neglog(fc, val_obs))
graph_observations <- function(graph=NULL, obs, alpha=0.4, colour="black") {
    if(is.null(graph)) {
        graph <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) {
        # TODO print debugging reveals that this code is covered by tests, but covr::report() marks it as untested. This is likely a problem with either vdiffr or covr
        return(
            # we don't change alpha here because it makes the score colormap inaccurate/misleading
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=val_obs, color=score), data=obs)
        )
    } else {
        return(
            graph + ggplot2::geom_point(ggplot2::aes(x=time, y=val_obs), data=obs, alpha=alpha, colour=colour)
        )
    }
}
# TODO make TRUE consistently one color and FALSE consistently another