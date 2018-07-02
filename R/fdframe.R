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

fdframe <- function(t, Y)
    UseMethod("fdframe")

fdframe.default <- function(t, Y) {
  Y = as.matrix(Y) # coherece to a matrix in case of univariate functions. 
  n <- dim(Y)[1]; D <- dim(Y)[2] 
  if(inherits(t, "Date")==T){t = as.Date(t)} else {t = t}
  ## Organize and return outputs
  outputs <- list(t = t, Y = Y, n=n, D=D, call = match.call())
  class(outputs) <- "fdframe"
    return(outputs)
}

plot.fdframe <- function(x, time.format="%m-%y", ...) {
    matplot(x$t, x$Y, type = "p", pch=20, 
         ylim = c(min(x$Y), max(x$Y)),xaxt="n",...)
  
  if(inherits(x$dates, "Date")==T){
    timelabels<-format(x$dates,time.format)
    axis(1,at=x$dates,labels=timelabels)} else {
      axis(1,at=x$dates)    }
}

