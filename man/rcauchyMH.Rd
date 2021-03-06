% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcauchyMH.R
\name{rcauchyMH}
\alias{rcauchyMH}
\title{Generate random samples from Cauchy distribution by MH sampler}
\usage{
rcauchyMH(n, theta = 0, eta = 1)
}
\arguments{
\item{n}{number of observations.}

\item{theta}{location parameter}

\item{eta}{scale parameter}
}
\value{
a random sample of size \code{n}
}
\description{
Generate random samples from Cauchy distribution by MH sampler
}
\examples{
\dontrun{
rcauchyMH(n=10,theta=0,eta=1)
}
}
