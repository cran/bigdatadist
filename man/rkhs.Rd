\name{rkhs}
\alias{rkhs}
\title{RKHS Representation}
\usage{rkhs(fdframe, gamma=1, kerfunc='rbf', 
            kerpar=list(sigma=1, bias=0, degree=2))}
\arguments{
\item{fdframe}{functional data \code{fdframe} object.}
\item{gamma}{regularization parameter.}
\item{kerfunc}{kernel function rbf or poly to be used.}
\item{kerpar}{a list of kernel parameters where sigma is the scale with both kernels.}
}
\value{
\item{fdframe}{raw data in an fdframe object.}
\item{f}{estimated functional data} 
\item{alpha}{coefficients for the linear combination.}
\item{lambda.star}{reduced coefficients for the linear combination.}
    }
\description{
This function allows you to fit discrete functional data (fdframe) as 
functions in RKHS solving a regularization problem as stated in Munoz (2010).
}
\references{
Munoz A. et al (2010). Representing functional data using support vector 
machines. Pattern recognition letters, 31(6). 
}
\examples{
data(merval); t <- as.Date(merval[1:100,1])
t <- as.numeric(( t - min(t) ) / 154)
raw.data <-fdframe(t = t, Y = merval[1:100,2:5])
plot(raw.data, xlab='time', ylab='Merval raw data')

f.data <- rkhs(raw.data, gamma = 0.001, kerpar = list(sigma = 8))

print(f.data)

plot(f.data, xlab='time', ylab='Merval data')

}
\keyword{rkhs}
\author{Hernandez and Martos}
