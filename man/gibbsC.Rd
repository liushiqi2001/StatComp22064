% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{gibbsC}
\alias{gibbsC}
\title{A Gibbs sampler for two-dimensional normal random numbers using R}
\usage{
gibbsC(N, mu1, mu2, sigma1, sigma2, pho)
}
\arguments{
\item{N}{the number of samples}

\item{mu1}{the mean of the first dimension}

\item{mu2}{the mean of the second dimension}

\item{sigma1}{the standard deviation of the first dimension}

\item{sigma2}{the standard deviation of the second dimension}

\item{pho}{the correlation of two dimensions}
}
\value{
a random sample of size \code{N}
}
\description{
A Gibbs sampler for two-dimensional normal random numbers using R
}
\examples{
\dontrun{
rnC <- gibbsC(N=2000, mu1=0, mu2=0, sigma1=1, sigma2=1, pho=0.9)
par(mfrow=c(1,3));
plot(rnC[,1],type='l')
plot(rnC[,2],type='l')
plot(rnC)
}
}
