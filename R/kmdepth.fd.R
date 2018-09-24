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

kmdepth.fd <- function(fdframe, gamma = 1, kerfunc = "rbf" ,
                        kerpar = list(sigma = 1, bias = 0, degree = 2) ,
                        d = 2 , robust=TRUE , h=0.1 , nsamp=250)  {

  # 1) Representation Step
  rep <- rkhs(fdframe, gamma, kerfunc, kerpar)
  
  # 2) Computing the KMD
  
  lambda.trunc=rep$lambda.star[,1:min(d,rep$fdframe$n)]
  
  # 2) Computing the KMD
  
  Func.KMD=matrix(0,rep$fdframe$D,2)
  
  if (robust==TRUE){
  
  KMD.center=CovMcd(lambda.trunc, alpha=1-h,nsamp=nsamp)@center
  KMD.cov=CovMcd(lambda.trunc, alpha=1-h,nsamp=nsamp)@cov
  
  Func.KMD[,1]=seq(1,rep$fdframe$D)
  Func.KMD[,2]=1/sqrt(mahalanobis(lambda.trunc, center=KMD.center, cov=ginv(KMD.cov), inverted=TRUE))
  
  } else {
    
    KMD.center=matrix(0,1,dim(lambda.trunc)[2])
    for (i in 1:length(KMD.center)){KMD.center[i]=median(lambda.trunc[,i])}
    KMD.cov=var(lambda.trunc)
    
    Func.KMD[,1]=seq(1,rep$fdframe$D)
    Func.KMD[,2]=1/sqrt(mahalanobis(lambda.trunc, center=KMD.center, cov=ginv(KMD.cov), inverted=TRUE))
  
    
  }
  
  # 3) Organizing the outputs:
  
  output <- list(depth = Func.KMD,
                 call =match.call()) 
  class(output) <- "list"
  return(output)
}
  