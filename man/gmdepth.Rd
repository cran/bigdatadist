\name{gmdepth}
\alias{gmdepth}
\title{Generalized Mahalanobis Depth and Distance}
\usage{gmdepth(A,b,resol,k.neighbor) }
\arguments{
\item{A}{data matrix where variables in columns, observations in rows.}
\item{b}{a new point in the support of the distribution to evaluate the depth. 
        If omitted, the function compute the distances and depth between all 
        points in the sample.}
\item{resol}{resolution level, i.e. number of density level sets to consider.}
\item{k.neighbor}{number of local neighbours to estimate the support.}
}
\value{
  \item{depth}{the generalized Mahalanobis depth measure.}
  \item{distance}{the generalized Mahalanobis distance measure.}
}
\description{
This function allows you to compute the Generalized Kernel Mahalanobis depth
measure as stated in Hernandez et al (2018, submitted) and the Generalized 
Mahalanobis distance in Martos et al (2014).
}
\references{
Hernandez N. et al (2018). Generalized Mahalanobis depth functions (submitted).
Martos, G. et al (2014). Generalizing the Mahalanobis distance via density
 kernels. Inteligent Data Anal.
}
\examples{
require(MASS)
set.seed(1)
A=mvrnorm(450,c(0,0),Sigma=diag(2))
b=mvrnorm(50,c(10,10),Sigma=diag(c(0.1,0.1)))
C=rbind(A,b)
plot(C, pch=20, col=c(rep('black',450),rep('red',50)),
                      xlab='x1',ylab='x2')
\donttest{
gmd.fit = gmdepth(A=C)
depth    = gmd.fit$depth
distance = gmd.fit$distance
plot(depth,distance, pch=20, 
           col=c(rep('black',450),rep('red',50)))
gmdepth(A=A,b=mvrnorm(1,c(0,0),Sigma=diag(2))) 
}
}
\keyword{Generalized Mahalanobis depth and distance.}
\author{Hernandez and Martos}
