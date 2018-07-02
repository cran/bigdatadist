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

entropy.fd <- function(fdframe, gamma = 1, 
                        kerfunc = "rbf" , 
                        kerpar = list(sigma = 1, bias = 0, degree = 2), 
                        alpha = 2, d = 2 , resol=11 , k.neighbor){
  
  # 1) Representation Step
  rep <- rkhs(fdframe, gamma, kerfunc, kerpar)
  
  # 2) Entropy computation
  
  lambda.trunc=rep$lambda.star[,1:min(d,rep$fdframe$n)]
  

  entropy.estimation = entropy(lambda.trunc, alpha = alpha, k.neighbor = k.neighbor)
  
  # 3) Organizing the outputs:
  #@ Le cambie los nombres a las cosas... al orden le llamo depth.
  output <- list(local.entropy = entropy.estimation$local.entropy, 
                 entropy = entropy.estimation$entropy,
                 call =match.call() ) 
  return(output)
  
}

#print.entropy.fd  <- function(x, ...) {
#  cat("\n Functional Local Entropy: \n ========================================= \n")
#  print(x$depth)
#  cat("\n Global Entropy \n"); cat("\n")
#  print(x$entropy)
#}

#plot.entropy.fd <- function(x, alpha.levels=10, ...) {
#  active.par <- par() # temporary save plot parameters defined by user.
#  par(mar=c(5,5,6,2),oma=c(0,0,0,0))
#  matplot(x$t , x$f, type="l", lty=1,  col="#00000022", 
#          xlim=c(min(x$t),max(x$t)),
#          ylim=c(min(x$f),max(x$f)),...)
  
#  for (i in 1:round(dim(x$f)[2]*alpha.levels/100) ){
#    for (j in 1:round(dim(x$f)[2]*alpha.levels/100) ){
#      polygon( c(x$t,rev(x$t) ), c(t(x$f[,x$depth[i,1]]),rev(t(x$f[,x$depth[j,1]]))),col="slategrey", border=NA)  
#    }
#  }
  
#  lines(x$t, t(x$f[,x$depth[1,1]]), lwd=2, col="blue")
  
#  legend("topleft", c("Minimun Entropy curve", 
#                      "Functional data set", 
#                      paste(alpha.levels,"% Min. Entropy curves")), 
#         lty=c(1,1,1), lwd=c(2,1,5),
#         col=c("blue","#00000022", "slategrey"),bty="n")        
  
#  par <- active.par # restore previous ploting parameters.
#}