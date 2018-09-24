##  ========================================================================  ##
##  Gabriel Martos                                                            ##
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
###############################################

############ To compute distances between several level sets:
levelsetdist = 
  function(A,B,n.level=10,k.neighbor=10){
    
    if(identical(A,B)==T){distancia = 0} else {
    alpha = seq(0.00000001,0.99999999,length=n.level)
    
    datos=as.matrix(A)
    datos2=as.matrix(B)
    res = vector(mode="list")
    for(i in 1:length(alpha))
    {res[[i]]=calcula.soporte(datos,k=k.neighbor,cuantil=alpha[i])}
    
    aiA = vector(mode="list")
    for(i in length(alpha):2){
      aiA[[i]]= setdiff(res[[i]]$ind,res[[i-1]]$ind)}
    aiA[[1]] = res[[1]]$ind # Colection of points that belongs to
    # different alpha-level sets for data set A.
    res2 = vector(mode="list")
    for(i in 1:length(alpha))
    {res2[[i]]=calcula.soporte(datos2,k=k.neighbor,cuantil=alpha[i])}
    
    aiB = vector(mode="list")
    for(i in length(alpha):2){
    aiB[[i]]= setdiff(res2[[i]]$ind,res2[[i-1]]$ind)}
    aiB[[1]] = res2[[1]]$ind # Colection of points that belongs to
    # different alpha-level sets for data set B.
    

      distancia_i = rep(0,length(alpha))
      factor.normaliza=sum(1-alpha)
      for(i in 1:length(alpha))
      {di=calcula.distancia(datos[aiA[[i]],],datos2[aiB[[i]],],k.neighbor)
      
      distancia_i[i]=(1-alpha[i])* di/factor.normaliza}
      
      distancia = sum(distancia_i,na.rm = TRUE)}
      return(distancia)
    
  }