#' Plot the KDE used in `log_score()`
#'
#' Create a plot displaying the density function calculated by the Kernel Density Estimation (KDE),
#' for the specified time in the forecast.
#' 
#' @details
#' `plot_KDE()` creates a plot containing:
#' - The density curve at the given time calculated by the KDE
#' - The forecast data points at the given time, along the x-axis
#' - A histogram of the density of the forecast data points
#' - A vertical line showing the observation at the given time (if provided)
#'
#' @template fcst
#' @param obs (Optional) An observations data frame. Data from if will be included in the graph if provided
#' @param at (Optional) See `?log_score`
#' @param after (Optional) See `?log_score`
#' @param bw (Optional) See `?log_score`
#' @param from,to (Optional) The range over which the density will be plotted
#' @param n (Optional) How many points to calculate the density for
#' @param binwidth (Optional) The binwidth for the histogram. Not to be confused with `bw`, which stands for bandwidth and is unrelated.
#'
#' @returns A ggplot object
#' @export
#' @autoglobal
#'
#' @examples
#' withr::with_seed(42, {
#'   dat <- rnorm(100)
#'   fc <- create_forecast(dplyr::tibble(time=rep(1,100), val=dat), forecast_time=1)
#'   obs <- data.frame(time=1, val_obs=1)
#'   plot_KDE(fc, obs, at=1)
#'   plot_KDE(fc, obs, at=1, bw=0.1)
#'   plot_KDE(fc, obs, at=1, bw=1)
#' })
plot_KDE <- function(fcst, obs=NULL, at=NULL, after=NULL, bw=NULL, from=NULL, to=NULL, n=101, binwidth=NULL) {
    # this function is structured very similarly to `log_score()`
    # it just has a different goal and some different parameters
    
    ## validate

    # validate bw (mostly just make sure it isn't a vector with length >1)
    if(!(is.null(bw) || (is.numeric(bw) && length(bw)==1))) {
        stop("`bw` must be either NULL or a single number")
    }

    # validate fcst and/or obs
    if(is.null(obs)) {
        validate_forecast(fcst)
    } else {
        validate_fcst_obs_pair(fcst, obs)
    }

    ## compute density

    # get the time point
    t <- calc_specified_time(fcst, at, after)

    # get the data for this time point
    samp <- get_time_point(fcst$data, t)$val
    if(!is.null(obs)) {
        val_obs <- get_time_point(obs, t)$val_obs
    }

    # if not provided, `from` and `to` default to 2x the range beyond the max and min (of the sample)
    rng <- max(samp) - min(samp)
    if(is.null(from)) {
        from <- min(samp) - 2*rng
    }
    if(is.null(to)) {
        to <- max(samp) + 2*rng
    }

    x <- seq(from, to, length.out=n)

    # calculate negative-log score
    neglog <- x |> purrr::map(\(v) scoringRules::logs_sample(v, samp, bw=bw)) |> as.numeric()
    
    # get densities (undo negation and undo log)
    densities <- exp(-neglog)
    
    if(!all(is.finite(densities))) {
        warning("infinite densities encountered in KDE")
        
        # remove infinite densities
        w <- which(is.finite(densities))
        densities <- densities[w]
        x <- x[w]
    }

    ## plot

    # make data frames
    data <- data.frame(x=x, y=densities)
    samp_data <- data.frame(x=samp, y=0)

    # plot the densities
    plt <- ggplot2::ggplot() + ggplot2::geom_line(data=data, mapping=ggplot2::aes(x=x, y=y))

    plt <- plt +
        # plot the sample data along the x axis as points
        ggplot2::geom_point(ggplot2::aes(x=x,y=y), alpha=0.2, data=samp_data) +
        # plot a histogram too for good measure
        ggplot2::geom_histogram(mapping=ggplot2::aes(x=x,y=ggplot2::after_stat(density)), binwidth=binwidth, data=samp_data["x"], alpha=0.5)

    # plot observation point if given
    if(!is.null(obs)) {
        plt <- plt + ggplot2::geom_vline(alpha=0.2, xintercept=val_obs)
    }

    # create title
    if(is.null(bw)) {
        bw <- stats::bw.nrd(samp)
    }
    plt <- plt + ggplot2::labs(title=glue::glue("KDE at time {t} with bandwidth {bw}"))

    # label axes
    plt <- plt + ggplot2::xlab("value") + ggplot2::ylab("density")
    plt

    # uncomment this and write grouping tests for plot_KDE()
    # validate_plotting_groups(fcst$data)
    # plt |> apply_facets(get_plotting_groups(fcst$data))
}