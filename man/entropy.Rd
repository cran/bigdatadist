\name{entropy}
\alias{entropy}
\title{Entropy Computation}
\usage{entropy(X,alpha=2,k.neighbor,scale=FALSE) }
\arguments{
\item{X}{data in a matrix where variables are in columns and observations are in rows.}
\item{alpha}{a parameter defining the entropy function.}
\item{k.neighbor}{number of neighbour points to consider in the computation of entropy.}
\item{scale}{logical variable indicating if scaling is required.}
}
\value{
  \item{local.entropy}{local entropy relative to each point in the sample.}
    \item{entropy}{estimated entropy.}
    }
\description{
This function allows you to compute the family of alpha entropy 
as stated in Martos et al (2018).
}
\details{
The function computes the alpha entropy and the local alpha entropy 
(see reference for further details) of a data set using a non parametric 
density estimator.
}
\references{
Martos, G. et al (2018): Entropy Measures for Stochastic Processes with 
Applications in Functional Anomaly Detection. Entropy 20(1): 33 (2018).
}
\examples{
require(MASS);
data = mvrnorm(100,c(0,0),diag(2));
entropy(data, alpha = 2, k.neighbor = 10, scale = FALSE)
}
\keyword{Entropy}
