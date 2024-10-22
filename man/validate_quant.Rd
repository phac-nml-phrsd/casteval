% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_quant}
\alias{validate_quant}
\title{Validate a single quantile}
\usage{
validate_quant(quant)
}
\arguments{
\item{quant}{The quantile to validate}
}
\value{
\code{NULL} if valid, error otherwise
}
\description{
Given a quantile, check that it:
\itemize{
\item is a single number
\item is between 0 and 100, inclusive
}
}
\examples{
# valid
casteval:::validate_quant(50)
casteval:::validate_quant(0)
casteval:::validate_quant(100)
casteval:::validate_quant(2.5)

# invalid
try(casteval:::validate_quant("50"))
try(casteval:::validate_quant(-1))
try(casteval:::validate_quant(c(25,75)))
}
