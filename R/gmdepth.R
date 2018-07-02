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

gmdepth <- function(A,b=NULL,resol=sqrt(dim(A)[1]),k.neighbor=dim(A)[1]/2){
  A=as.matrix(A)
  n=dim(A)[1]
    
    alpha=seq(0,1,length=resol)
    res = vector(mode="list")

  for(i in 1:length(alpha))
  {
    res[[i]]=calcula.soporte(A,k=k.neighbor,cuantil=alpha[i])
  }
  
  if (is.null(b)){
  
  distance=c()
  m0=res[[1]]$ind
  distance[m0]=log(1/(1-(alpha[1])))
  for (i in 2:length(alpha)){
    for (j in res[[i]]$ind){
      if ((!(j%in%res[[i-1]]$ind))) {distance[j]=log(1/(1-(alpha[i-1]+alpha[i])/2))} 
    }
  }
  } else {
    ai = vector(mode="list")
    bi= c(1)
    for(i in length(alpha):2){
      
      ai[[i]]= setdiff(res[[i]]$ind,res[[i-1]]$ind)
      if(n%in%ai[[i]]) { bi=i }
    }
    distance = log(1/(1-
                         if(alpha[bi]<1) { (alpha[bi]) }
                       else { (alpha[bi-1]+alpha[bi])/2 }
    ))
  }
    
  # Organizing the outputs:
  output <- list(depth = exp(-distance), distance = distance, 
                 call =match.call() ) 
  class(output) <- "list"
  return(output)
}
