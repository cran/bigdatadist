##  ========================================================================  ##
##  Gabriel Martos & Nicolas Hernandez                                        ##
##  Copyright (C) 2018                                                        ##
##  ------------------------------------------------------------------------  ##
##  This program is free software; you can redistribute it and/or modify      ##
##  it under the terms                                                        ##
##  ========================================================================  ##
##  Gabriel Martos & Nicolas Hernendez                                        ##
##  Copyright (C) 2018                                                        ##
##  ------------------------------------------------------------------------  ##
##  This program is free software; you can redistribute it and/or modify      ##
##  it under the terms of the GNU General Public License as published by      ##
##  the Free Software Foundation; either version 2 of the License, or         ##
##  (at your option) any later version.                                       ##
##                                                                            ##
##  This program is distributed in the hope that it will be useful,           ##
##  but WITHOUT ANY WARRANTY; without even the implied warranty of            ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             ##
##  GNU General Public License for more details.                              ##
##                                                                            ##
##  You should have received a copy of the GNU General Public License         ##
##  along with this program; if not, a copy is available at                   ##
##  http://www.r-project.org/Licenses/                                        ##
##  ========================================================================  ##

rkhs <- function(fdframe, gamma = 1, kerfunc = 'rbf' , 
                 kerpar = list(sigma = 1, bias = 0, degree = 2) )
UseMethod("rkhs")

rkhs.default <- function(fdframe, gamma = 1, kerfunc = 'rbf' , 
                         kerpar = list(sigma = 1, bias = 0, degree = 2)) {
  if (class(fdframe) != "fdframe")
    stop('Stop: the input fdframe must be an fdframe object.')
  
    if ( !isTRUE(kerfunc== "rbf" || kerfunc=="poly") ) 
      stop("Stop: kerfunc must be 'rbf' or 'poly'")
    
    if (kerfunc == "rbf") { KX <- kern.rbf(t = fdframe$t, sigma = kerpar[[1]]) } else { 
                            KX <- kern.poly(t = fdframe$t, sigma = kerpar[[1]], 
                                            bias = kerpar[[2]], degree = kerpar[[3]]) }
  
  n <- fdframe$n;  r <- qr(KX)$rank; svd <- svd(KX)
  # Reduced matrices:
  U <- as.matrix(svd$u[,1:r])
  D <- diag(svd$d[1:r], nrow=r )
  
  alpha <- ginv(diag(rep( n*gamma , n)) + KX)%*%fdframe$Y 

  lambda.star <- t(alpha)%*%U%*%sqrt(D) 

  f <- KX%*%alpha

  # Organizing the outputs:
 output <- list(fdframe = fdframe, f = f, 
                alpha = alpha , lambda.star = lambda.star, 
                call =match.call() ) 
 class(output) <- "rkhs"
 return(output)
}

print.rkhs <- function(x, ...) {
  cat("\n RKHS projection: \n ========================================= \n")
  print(x$call)
  cat("\n Functional evaluations :\n"); cat("\n")
  print(x$f)
  cat("\n Linear combination coefficients \n"); cat("\n")
  print(x$alpha)
  cat("\n Reduced coefficients: \n"); cat("\n")
  print(x$lambda.star)
}

plot.rkhs <- function(x, ...) {
  active.par <- par() # temporary save plot parameters defined by user.
  par(mar=c(5,5,6,2),oma=c(0,0,0,0))
  matplot(x$fdframe$t , x$f, type="l", 
          xlim=c(min(x$fdframe$t),max(x$fdframe$t)),
          ylim=c(min(x$f),max(x$f)),...)
  par <- active.par # restore previous ploting parameters.
}