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

calcula.soporte=function(datos,k=100,cuantil=0.5){
  dis=dist(datos)
  dis2=as.matrix(dis)
  ndatos=nrow(as.matrix(datos))
  lim = min(ndatos,k+1)
  matriz=matrix(nrow=dim(dis2)[1],ncol=lim)
  for(i in 1:ndatos){matriz[i,]=sort(dis2[i,])[1:lim]}
  inter = as.matrix(matriz[,-1])
  medias=matrix(0,1,dim(inter)[1])
  for (i in 1:dim(medias)[2]){medias[i]=median(inter[i,])}
  ind=seq(along=medias)[medias<=quantile(medias,p=cuantil)]
  dsop=quantile(medias,p=cuantil)
  return(list(ind=ind,dsop=dsop))
}

calcula.radio=function(datos,radio=1,min=F){
  dis=dist(datos)
  dis2=as.matrix(dis)
  ndatos=nrow(as.matrix(datos))
  lim=min(radio+1,ndatos)
  matriz=matrix(nrow=dim(dis2)[1],ncol=lim)
  for(i in 1:ndatos){matriz[i,]=sort(dis2[i,])[1:lim]}
  inter = as.matrix(matriz[,-1])
  medias = apply(inter, 1, median) 
  if(min==T){return(min(as.dist(dis)))}
  else{return(media=median(medias))}
}

calcula.distancia = function(A,B,radio){
  A= as.matrix(A); B= as.matrix(B)
  n1=dim(A)[1]   ; n2=dim(B)[1]
  radio1 = calcula.radio(A,radio=radio,min=F)
  radio2 = calcula.radio(B,radio=radio,min=F)
  
  # In the case there are 1 single value in an alpha set radio=NA and then:
  if(is.na(radio1)==T){radio1=0};if(is.na(radio2)==T){radio2=0};
  
  
  distancia = as.matrix(pdist(A,B))

  distancia=distancia<=max(radio1,radio2) # Convert into a logical matrix
  indica1 = apply(distancia,1,max) # reseting the indexes for data set A
  indica2 = apply(distancia,2,max) # reseting the indexes for data set B
  
  A.int.B=sum(indica1)  # los de A que estan en B
  B.int.A=sum(indica2)  # los de B que estan en A
  diff.A.B = n1-A.int.B
  diff.B.A = n2-B.int.A
  A.dif.simetrica.B = diff.A.B + diff.B.A
  A.int.B = A.int.B + B.int.A
  A.union.B = dim(unique(rbind(as.matrix(A),as.matrix(B))))[1]
  dist=A.dif.simetrica.B/A.union.B
  return(dist)}



kern.rbf <- function(t,sigma) {
   l <- length(t);   n <- l^2
   run <- .Fortran('kernrbf_', t = as.double(t) , l = as.integer(l), 
                   n = as.integer(n), sigma = as.double(sigma), 
                   K = numeric(n), PACKAGE = "bigdatadist")
   return(matrix(run$K, ncol = l, nrow = l))  
 }

kern.poly <- function(t,sigma, bias, degree) {
   l <- length(t);   n <- l^2
   run <- .Fortran('kernpoly_', t = as.double(t) , l = as.integer(l), 
                   n = as.integer(n), sigma = as.double(sigma),
                   bias = as.double(bias), degree = as.double(degree),
                   K = numeric(n), PACKAGE = "bigdatadist")
   return(matrix(run$K, ncol = l, nrow = l))  
 }
 
