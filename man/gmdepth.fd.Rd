\name{gmdepth.fd}
\alias{gmdepth.fd}
\title{Generalized Mahalanobis Kernel Depth and Distance for Functional Data}
\usage{
gmdepth.fd(fdframe, gamma = 1,kerfunc="rbf" , 
   kerpar=list(sigma=1,bias=0,degree=2),d=2,resol,k.neighbor)
}
\arguments{
\item{fdframe}{an \code{fdframe} object storing raw functional data.}
\item{gamma}{regularization parameter.}
\item{kerfunc}{kernel function to be used. }
\item{kerpar}{a list of kernel parameters where sigma is the scale 
              with both kernels.}
\item{d}{truncation parameter in the Reproducing Kernel Hilbert Space representation.}
\item{resol}{resolution level to estimate the generalized 
              Mahalanobis distance.}
\item{k.neighbor}{number of neighbours to estimate the support 
                  of the disitribution.}
}
\value{
  \item{depth}{the generalized Mahalanobis depth measure for the 
               curves in the sample.}
  \item{distance}{the generalized Mahalanobis distance for the 
                  curves in the sample.}
}
\description{
This function allows you to compute the Generalized Kernel Mahalanobis 
depth measure as stated in Hernandez et al (2018, submitted) and the 
Generalized Mahalanobis distance in Martos et al (2014), for functional
data represented in a Reproducing Kernel Hilbert Space.
}
\references{
Hernandez N. et al (2018, submitted). Generalized Mahalanobis depth functions.
Martos, G. et al (2014). Generalizing the Mahalanobis distance via density 
kernels. Inteligent Data Anal.
}
\examples{
data(Ausmale); t <- Ausmale[[1]]
t = as.numeric(( t - min(t) ) / length(t))
raw.data = fdframe(t=t, Y=Ausmale[[2]])

gmd.fit.fd = gmdepth.fd(raw.data,gamma=0.001,
            kerfunc="rbf",kerpar=list(sigma = 10))

gmd.fit.fd$distance
gmd.fit.fd$depth

rbPal <- colorRampPalette(c('red','black'))
color = rbPal(5)[as.numeric(cut(gmd.fit.fd$depth,breaks = 5))]
plot(rkhs(raw.data,gamma=0.0001,kerfunc="rbf",kerpar=list(sigma = 10)),
 col = color, xlab='time',ylab='')
}
\keyword{Kernel depth}
\author{Hernandez and Martos}
