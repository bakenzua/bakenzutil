% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qqladder.R
\name{qqladder}
\alias{qqladder}
\title{qqladder}
\usage{
qqladder(data)
}
\arguments{
\item{data}{A vector of values for transformation  as per qqladder_df
and plotting qq plot for each transformation}
}
\value{
ggplot2 object
}
\description{
qqladder
}
\examples{
qqladder(rnorm(100, 10, 1))

}
