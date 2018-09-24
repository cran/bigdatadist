\name{entropy.fd}
\alias{entropy.fd}
\title{Functional Entropy Measures}
\usage{entropy.fd(fdframe, gamma = 1, kerfunc="rbf",
       kerpar = list(sigma = 1, bias=0,degree=2), 
       alpha=2,d=2,resol,k.neighbor) }
\arguments{
\item{fdframe}{functional data frame \code{fdframe} object.}
\item{gamma}{regularization parameter.}
\item{kerfunc}{kernel function (\code{rbf} or \code{poly}) to be used.}
\item{kerpar}{a list of kernel parameters where sigma is the scale with both kernels.}
\item{alpha}{Entropy parameter.}
\item{d}{Dimension truncation in the Reproducing Kernel Hilbert Space representation.}
\item{resol}{number of level sets used to compute the functional entropy.}
\item{k.neighbor}{number of points to estimate the support of the distribution.}
}
\value{
\item{local.entropy}{local entropy relative to each curve in the sample.}
\item{entropy}{estimated entropy of the the set of functions.}
}
\description{
This function allows you to compute the family of alpha-Entropy for 
functional data as stated in Martos et al (2018).
}
\details{
  This function estimates the entropy of a stochastic process. To this aim,
  the raw functional data is projected onto a Reproducing Kernel Hilbert Space,
  and the entropy is estimated using the coefficient of these functions. 
}
\references{
Martos, G. et al (2018). Entropy Measures for Stochastic Processes with 
Applications in Functional Anomaly Detection. Entropy 20(1), 33 (2018).
}

\examples{
data(Ausmale); t <- Ausmale[[1]]
t <- as.numeric(( t - min(t) ) / length(t))
raw.data <- fdframe(t=t, Y=Ausmale[[2]])

entropy.fd(raw.data,gamma=0.0001,kerfunc="rbf",kerpar=c(10), 
                        alpha=2, k.neighbor=15)
}
\keyword{rkhs, Entropy}
\author{Hernandez and Martos}
