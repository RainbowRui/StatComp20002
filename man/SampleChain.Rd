% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleChain.R
\name{SampleChain}
\alias{SampleChain}
\title{Generate Metropolis chain from giving multinomial distribution}
\usage{
SampleChain(N, start)
}
\arguments{
\item{N}{the length of chain}

\item{start}{the starting value}
}
\value{
a random sample of size \code{N}
}
\description{
Generate Metropolis chain from giving multinomial distribution
}
\examples{
\dontrun{
rnR <- SampleChain(N=10, start=1)
plot(rnR,type='l')
}
}
