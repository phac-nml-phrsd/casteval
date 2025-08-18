




#' @title Logarithmic score for forecast assuming a normal distribution
#' given paired quantiles.
#'
#' @description
#' Given a forecast object having at least one paired quantiles,
#' assume that the forecasted values are distributed according to
#' a normal distribution with the same quantile pair.
#' The logarithmic score is then calculated following this
#' normality assumption.
#'
#'
#' @template fcst
#' @param obs An observations data frame.
#' @template summarize
#' @param quantpair.choice (Optional) Integer. If several quantiles pairs
#' are present in the forecast object, determine which one to use to
#' calculate the mean and standard deviation of the implied normal distribution.
#'
#'
#' @returns If summarize is `FALSE`, a data frame containing times,
#' observations, scores, quantiles used, and implied mean and standard
#' deviation of the normal distribution. Otherwise, returns the mean of
#' all scores at every times.
#'
#' @export
#'
#' @examples
#' fcst <- create_forecast(
#' dplyr::tibble(
#'   time    = 1:3,
#'   val_q5  = seq(1,8, length.out = 3),
#'   val_q10 = 9:11,
#'   val_q25 = 12:14,
#'   val_q50 = 100:102,
#'   val_q75 = 200:202,
#'   val_q90 = 203:205,
#'   val_q95 = seq(210,220, length.out = 3)
#' ))
#'
#' obs <- data.frame(time=1:3, val_obs=c(105,101,300))
#'
#' s = log_score_approx_normal(fcst, obs,
#'                             summarize = F,
#'                             quantpair.choice = 1)
#' print(s)
#'
#' s = log_score_approx_normal(fcst, obs,
#'                             summarize = F,
#'                             quantpair.choice = 2)
#' print(s)
#'
#' s = log_score_approx_normal(fcst, obs,
#'                             summarize = TRUE,
#'                             quantpair.choice = 3)
#' print(s)

log_score_approx_normal <- function(fcst,
                                    obs,
                                    summarize = TRUE,
                                    quantpair.choice = 1) {

  if(0) { # DEBUG
    fcst <- create_forecast(
      dplyr::tibble(
        time=1:3,
        val_q5=1:3,
        val_q10 = 5:7,
        val_q25=12:14,
        val_q50=100:102,
        val_q75=200:202,
        val_q90 = 203:205,
        val_q95 = seq(210,220, length.out = 3)
      ))

    obs <- data.frame(time=1:3, val_obs=c(95,101,195))

    quantpair.choice = 1
  }

  validate_fcst_obs_pair(fcst, obs)

  # Extract the requested paired quantiles
  qpairs <- parse_quant_pairs(quant_pairs = NULL, fcst$data)
  nqp = length(qpairs)

  if(nqp < quantpair.choice)
    stop(glue::glue('Parameter `quantpair.choice` (={quantpair.choice}) ',
                    'cannot be larger than the number of quantile pairs (={nqp})',
                    ' defined in the forecast object.'))
  q = qpairs[[quantpair.choice]]

  # Extract forecast values at those quantiles
  x = fcst$data[, paste0('val_q',q)]

  # Calculate mean of approximate normal distribution
  m = apply(x, MARGIN = 1, FUN = mean)

  # Calculate standard deviation
  # using the quantiles only
  quantile_level <- q[1]
  value_at_quantile <- x[,1]
  z_score <- stats::qnorm(quantile_level / 100)
  sigma <- (value_at_quantile - m) / z_score

  # Calculate log score, assuming a normal
  # distribution of the forecasted values
  scores = stats::dnorm(x = obs$val_obs,
                 mean = m,
                 sd = sigma[,1],
                 log = TRUE)

  if(!summarize) res = obs |>
    dplyr::mutate(score = scores,
                  implied_normal_mean = m,
                  implied_normal_sd = sigma[,1])|>
    dplyr::bind_cols(x) |>
    dplyr::select(time, val_obs, score,
                  dplyr::starts_with('val_q'),
                  implied_normal_mean,
                  implied_normal_sd)

  if(summarize) res  = mean(scores)

  return(res)
}
