\name{levelsetdist}
\alias{levelsetdist}
\title{Level Set Distances}
\usage{levelsetdist(A,B,n.level=10,k.neighbor=10) }
\arguments{
\item{A}{data set in a matrix where variables are in columns and 
observations are in rows.}
\item{B}{data set in a matrix where variables are in columns and 
observations are in rows.}
\item{n.level}{the number of level sets to consider for distance computation.}
\item{k.neighbor}{number of neighbour points to consider in the estimation 
      of the support of the distribution on each class.}
}
\value{
    \item{distance}{distance estimation between the two data sets or distributions.}
    }
\description{
This function allows you to compute the alpha level set distances as proposed
in Martos et al. Nomparametric distances for probability measures with 
applications, 2018 (submitted).
}
\details{
The function computes the alpha level set distance between two (samples from)
different probability measures. Details about the distance and the criterion
to fix its hyperparameter can be found in Martos et al (2018, submitted).
}
\references{
Martos, G. et al (2018): Nomparametric distances for probability measures
with applications in classification. J. of Calssification, 2018 (submitted).
}
\examples{
require(MASS);
set.seed(1)
A = mvrnorm(100,c(0,0),diag(2));  B = mvrnorm(150,c(1,1),diag(2)); 
levelsetdist(A, B)
}
\keyword{Probability metrics}
