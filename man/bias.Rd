% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bias.R
\name{bias}
\alias{bias}
\title{Calculate forecast bias}
\usage{
bias(fcst, obs, summarize = TRUE)
}
\arguments{
\item{fcst}{A forecast object (see output of \code{create_forecast()}).}

\item{obs}{An observations data frame.}

\item{summarize}{A boolean, defaults to TRUE. If TRUE, a single number will be returned as the score for the forecast.
If FALSE, a data frame with columns named \code{time}, \code{val_obs}, and \code{score} will be returned,
containing the scores for each individual time point.
This can be used by plotting functions to colour-code observations, for example.}
}
\value{
A number between -1 and 1, inclusive. -1 means 100\% underprediction and 1 means 100\% overprediction.
}
\description{
Given a forecast and a set of observations, compute the bias of the forecast's predictions.
\code{bias()} looks for forecast data in the following order:
\enumerate{
\item raw data (\code{val})
\item mean (\code{val_mean})
\item median (\code{val_q50})
}
}
\details{
It uses the first that it finds to calculate bias,
by assigning 1 for overprediction, 0 for equality, and -1 for underprediction.
It then summarizes the score by taking the mean of the assigned values.
}
\examples{
obs <- data.frame(time=1:5, val_obs=rep(10,5))

# # a forecast with bias on individual days, but no overall bias
fc1 <- create_forecast(dplyr::tibble(
  time=c(1,1,2,2,3,3,4,4,5,5),
  val=c(9, 9, 9, 10, 10, 10, 10, 11, 11, 11)
))

bias(fc1, obs, summarize=FALSE)

bias(fc1, obs)

# A forecast with an underprediction bias
fc2 <- create_forecast(data.frame(
  time=c(1,1,1,2,2,2,3,3,3),
  val=c(9,9,9,10,10,10,11,9,9)
))

bias(fc2, obs, summarize=FALSE)

bias(fc2, obs)
}
