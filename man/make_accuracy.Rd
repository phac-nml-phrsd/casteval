% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accuracy.R
\name{make_accuracy}
\alias{make_accuracy}
\title{\code{accuracy()} function factory}
\usage{
make_accuracy(quant_pairs)
}
\arguments{
\item{quant_pairs}{See \code{?accuracy}}
}
\value{
A function wrapping \code{accuracy()} with the given quantile pairs
}
\description{
Given the desired quantile pairs,
create a function wrapping \code{accuracy()} which passes those quantile pairs to it.
}
\examples{
fc <- create_forecast(list(
  time=1:3,
  vals=list(c(4,7,8), c(5,6,7), c(4,6,6))
))
obs <- data.frame(time=1:3, val_obs=5:7)

acc <- make_accuracy(c(5,95))
acc(fc, obs)

plot_forecast(fc, obs, score=make_accuracy(c(25,75)))
}
