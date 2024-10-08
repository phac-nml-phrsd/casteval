% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accuracy.R
\name{accuracy}
\alias{accuracy}
\title{Get accuracy from quantiles}
\usage{
accuracy(fcst, obs, summarize = TRUE, quant_pairs = NULL)
}
\arguments{
\item{fcst}{A forecast object (see output of \code{create_forecast()}).}

\item{obs}{An observations data frame.}

\item{summarize}{A boolean, defaults to TRUE. If TRUE, a single number will be returned as the score for the forecast.
If FALSE, a data frame with columns named \code{time}, \code{val_obs}, and \code{score} will be returned,
containing the scores for each individual time point.
This can be used by plotting functions to colour-code observations, for example.}

\item{quant_pairs}{(Optional) A list of pairs of numbers between 0 and 100,
or a single pair of numbers between 0 and 100.
If provided, the score for each corresponding pairs of quantiles will be calculated.
If not provided, it will default to every symmetrical pair of quantiles that can be found in \code{fcst},
ordered from widest to narrowest (e.x. the 25\% and 75\% quantiles are symmetrical).

If \code{summarize} is \code{FALSE}, an additional column named \code{pair} will indicate which pair of quantiles each row represents.
If \code{summarize} is \code{TRUE}, the output will be a vector with the same length as \code{quant_pairs},
containing the respective score for each pair.}
}
\value{
A number from 0 to 1,
the rate at which the observations were inside the specified quantile interval
}
\description{
Given a forecast and set of observations,
compute the accuracy (# inside quantile interval / # total) of the forecast.
Raw data and/or provided quantiles will be used to compute the quantile interval.
}
\examples{
# forecast with raw data
fc1 <- create_forecast(dplyr::tibble(time=c(1,1,1,2,2,2,3,3,3), val=4:12))
obs1 <- data.frame(time=1:3, val_obs=c(5, 7.4, 11.6))

# calculate quantiles and accuracy from raw data
# returns 2/3
accuracy(fc1, obs1, quant_pairs=list(c(25, 75)))

# forecast with quantile data
fc2 <- create_forecast(dplyr::tibble(
  time=1:3, val_q5=1:3, val_q25=4:6, val_q50=100:102, val_q75=200:202, val_q95=203:205
))
obs2 <- data.frame(time=1:3, val_obs=c(4, 202, 1000))

# infer quantile pairs from forecast data (`c(5,95)` and `c(25, 75)`)
# returns c(2/3, 1/3)
accuracy(fc2, obs2)

# return a data frame with a `time`, `pair`, `val_obs`, and `score` columns
accuracy(fc2, obs2, summarize=FALSE)
}
