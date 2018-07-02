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

entropy <-function(X,alpha = 2, k.neighbor,scale = FALSE){
  X=as.matrix(X)
  n  =  dim(X)[1]
  
  if(scale==TRUE | scale==T){X=scale(X)}else{X=X}
  
  Knn.distances = knn.dist(X, k=k.neighbor)
  v_k = as.matrix(rowMeans(Knn.distances),ncol=1)
  
  l.entropy=log(1+v_k^alpha)/abs(1-alpha)
  entropy=log(sum(1+v_k^alpha))/abs(1-alpha)
  
  # Organizing the outputs:
  output <- list(local.entropy = l.entropy, entropy = entropy,
                 call =match.call() ) 
  class(output) <- "list"
  return(output)
}