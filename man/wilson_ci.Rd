% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wilson_ci.R
\name{wilson_ci}
\alias{wilson_ci}
\title{wilson_ci Wilson confidence interval for Binomial proportion}
\usage{
wilson_ci(events, trials, alpha = 0.05)
}
\arguments{
\item{events}{Number of outcomes(successes)}

\item{trials}{Total number of trials}

\item{alpha}{}
}
\value{
vector of lower and upper confidence interval
}
\description{
wilson_ci
}
\examples{

    The core code for this function is from
    https://rpubs.com/brouwern/binomialCI2
    https://stats.stackexchange.com/questions/59733/can-agresti-coull-binomial#-confidence-intervals-be-negative
    and was written by https://stats.stackexchange.com/users/21054/coolserdash

    for more info see
    wikipedia.org/wiki/Binomial_proportion_confidence_interval
}
