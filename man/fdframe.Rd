\encoding{UTF-8}
\name{fdframe}
\alias{fdframe}
\alias{fdframe.default}
\title{Functional Data Frame}
\description{
  This function is used to create multivariate functional data frame
  objects to be used in combination with the functions in the package
  \code{bigdatadist}.
}
\usage{
fdframe(t, Y)
}
\arguments{
\item{t}{abscissa values at which observations took place.}
\item{Y}{matrix with functions in columns and observations in rows.}
}
\examples{
t = 1:10; Y = cbind(sin(t),cos(t))
fdata = fdframe(t,Y)
plot(fdata, xlab='Time', ylab='')
}
\keyword{multivariate time series formatting.}
