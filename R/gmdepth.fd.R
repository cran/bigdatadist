##  ========================================================================  ##
##  Gabriel Martos & Nicolas Hernandez                                        ##
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


gmdepth.fd <- function(fdframe, gamma = 1, kerfunc = "rbf" , 
                       kerpar = list(sigma = 1, bias = 0, degree = 2) , 
                       d = 2 , resol=sqrt(fdframe$n) , k.neighbor = fdframe$n/2) {
  
  # 1) Representation Step
  rep <- rkhs(fdframe, gamma, kerfunc, kerpar)
  
  # 2) Computing the GMD
  
  lambda.trunc=rep$lambda.star[,1:min(d,rep$fdframe$n)]
  
  Func.GMD=matrix(0,rep$fdframe$D,2)
  Func.GMD[,1]=seq(1,rep$fdframe$D)
  Func.GMD[,2]=gmdepth(A=lambda.trunc,b=NULL,resol,k.neighbor)$distance
  
  # 3) Organizing the outputs:
  output <- list(depth = exp(-Func.GMD[,2]), distance = Func.GMD[,2], 
                 call =match.call() ) 
  
  class(output) <- "list"
  return(output)
  
}

