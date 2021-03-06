.onAttach <- function(libname, pkgname) {
  packageStartupMessage("## ============================================== ##")
  RFver <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),
                    fields = "Version")
  packageStartupMessage(paste
                       ("##", pkgname, RFver, "                            ##"))
  packageStartupMessage("## ---------------------------------------------- ##")
  packageStartupMessage("##  Copyright (C) 2018                            ##")
  packageStartupMessage("##  G. Martos and N. Hernandez                    ##")
  packageStartupMessage("## ============================================== ##")
  packageStartupMessage("")
}
