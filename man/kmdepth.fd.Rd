\name{kmdepth.fd}
\alias{kmdepth.fd}
\title{Kernel Mahalanobis Depth for Functional Data}
\usage{
kmdepth.fd(fdframe, gamma = 1, kerfunc = "rbf" ,
                        kerpar = list(sigma = 1, bias = 0, degree = 2) ,
                        d = 2 , robust=TRUE , h=0.1 , nsamp=250)  
}
\arguments{
\item{fdframe}{an \code{fdframe} object storing raw functional data.}
\item{gamma}{regularization parameter.}
\item{kerfunc}{kernel function to be used. }
\item{kerpar}{a list of kernel parameters where sigma is the scale 
              with both kernels.}
\item{d}{truncation parameter in the Reproducing Kernel Hilbert Space representation.}
\item{robust}{TRUE if the covariance matrix is estimated through Robust Maximum Likelihood method.}
\item{h}{numeric parameter controlling the a-prioir precentage 
of outliers in the sample (value between 0 and 1, by def = 0.1).}
\item{nsamp}{number of subsets used for initial estimates (by def = 250).}
}
\value{
  \item{depth}{the kernel-mahalanobis depth measure for the 
             curves in the sample.}
}
\description{
This function allows you to compute the Generalized Kernel Mahalanobis depth measure for a sample of functional data as stated in  Hernandez et al (2018, submitted).
}
\references{
Hernandez N. et al (2018, submitted). Generalized Mahalanobis depth functions.
}
\examples{
data(Ausmale); t <- Ausmale[[1]]
t = as.numeric(( t - min(t) ) / length(t))
raw.data = fdframe(t=t, Y=Ausmale[[2]])

kmd.fit.fd = kmdepth.fd(raw.data, gamma = 0.0001, kerfunc = "rbf" ,
                        kerpar = list(sigma = 10) , d = 2 , robust=TRUE)  

kmd.fit.fd$depth

rbPal <- colorRampPalette(c('red','black'))
color = rbPal(5)[as.numeric(cut(kmd.fit.fd$depth,breaks = 5))]
plot(rkhs(raw.data,gamma=0.0001,kerfunc="rbf",kerpar=list(sigma = 10)),
 col = color, xlab='time',ylab='')
}
\keyword{Kernel depth}
\author{Hernandez and Martos}