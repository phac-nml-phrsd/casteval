#' Intelligently plot observations
#'
#' Plot observation points. If scores are provided alongside the observations,
#'  the plot can be colour-coded to convey information about score, accuracy, etc.
#'
#' @template plt
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
#' # plot observations on their own
#' casteval:::plot_observations(NULL, obs)
#' 
#' # plot observations alongside forecast data
#' casteval:::plot_observations(casteval:::plot_ensemble(NULL, fc), obs)
#' 
#' # plot observations alongside forecast data, and colour-code by score
#' casteval:::plot_observations(casteval:::plot_ensemble(NULL, fc), neglog(fc, val_obs))
plot_observations <- function(plt=NULL, obs, alpha=0.4, colour="black") {
    if(is.null(plt)) {
        plt <- ggplot2::ggplot()
    }

    if("score" %in% colnames(obs)) {
        # TODO print debugging reveals that this code is covered by tests, but covr::report() marks it as untested. This is likely a problem with either vdiffr or covr
        return(
            # we don't change alpha here because it makes the score colormap inaccurate/misleading
            plt + ggplot2::geom_point(ggplot2::aes(x=time, y=val_obs, color=score), data=obs)
        )
    } else {
        return(
            plt + ggplot2::geom_point(ggplot2::aes(x=time, y=val_obs), data=obs, alpha=alpha, colour=colour)
        )
    }
}
# TODO make TRUE consistently one color and FALSE consistently another