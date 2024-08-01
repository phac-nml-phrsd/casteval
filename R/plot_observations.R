#' Plot observations
#'
#' Plot observation points. If scores are provided alongside the observations,
#'  the plot can be colour-coded to convey information about score, accuracy, etc.
#'
#' @template plt
#' @param obs An observations data frame. If it contains a `score` column,
#'  the observation points will be colour-coded according to the scores.
#'  Otherwise, the `alpha` and `colour` parameters will determine the opacity and colour of the points.
#' @template alpha
#' @template colour
#'
#' @returns A ggplot object.
#' @autoglobal
#' @export
#'
#' @examples
#' fc <- create_forecast(list(
#'   time=1:3,
#'   vals=list(c(4,7,10), c(5,8,11), c(6,9,12))
#' ))
#' obs <- data.frame(time=1:3, val_obs=c(5,9,13))
#' 
#' # plot observations on their own
#' plot_observations(NULL, obs)
#' 
#' # plot observations alongside forecast data
#' plot_observations(plot_ensemble(NULL, fc), obs)
#' 
#' # plot observations alongside forecast data, and colour-code by score
#' plot_observations(
#'   plot_ensemble(NULL, fc),
#'   log_score(fc, obs, summarize=FALSE)
#' )
plot_observations <- function(plt=NULL, obs, alpha=0.4, colour="black") {
    validate_obs(obs)
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


#' Plot and score observations
#'
#' Wrapper for `plot_observations()`.
#' Plots scores the given forecast against the given observations,
#' then plots the observations with a colour scale corresponding to score.
#'
#' @template plt
#' @template fcst
#' @param obs An observations data frame
#' @param invert_scale (Optional) a boolean. If `TRUE`, the color scale will be inverted.
#' This is useful for scores where smaller values are better, e.x. CRPS.
#' @template score
#' @param ... Additional parameters to be passed to `score`.
#' Note that `summarize` should not be one of them,
#' since `plot_obs_score()` already passes that to `score`.
#'
#' @returns A ggplot object
#' @export
#' @autoglobal
#'
#' @examples
#' #TODO
plot_obs_score <- function(plt=NULL, fcst, obs, invert_scale=FALSE, score, ...) {
    validate_fcst_obs_pair(fcst, obs)

    obs <- score(fcst, obs, summarize=FALSE, ...)
    plt <- plt |> plot_observations(obs, alpha=alpha, colour=colour)

    if(invert_scale) {
        plt <- plt + ggplot2::scale_color_continuous(trans="reverse")
    }

    plt
}