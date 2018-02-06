.onLoad <- function(libname, pkgname) {

  ns <- getNamespace(pkgname);
  pkg <- Package(pkgname);
  assign(pkgname, pkg, envir=ns);

  packageStartupMessage(paste0("#=========================================================================================#"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#                           Welcome to the NLPStudio (Beta)!                              #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("# Thank you for installing the NLPStudio package. To get started, a sample R script,      #"))
  packageStartupMessage(paste0("# 'myNLP.R' has been provided to illustrate some of the basic method calls for creating   #"))
  packageStartupMessage(paste0("# your first studio, sourcing a document collection and performing some basic analysis.   #"))
  packageStartupMessage(paste0("# Vignettes are also available at https://www.DataScienceSalon.org/NLPStudio. Thanks for  #"))
  packageStartupMessage(paste0("# for exploring NLPStudio.                                                                #"))
  packageStartupMessage(paste0("#                                       Data Science Salon                                #"))
  packageStartupMessage(paste0("#                                       https://www.DataScienceSalon.org/NLPStudio        #"))
  packageStartupMessage(paste0("#                                                                                         #"))
  packageStartupMessage(paste0("#=========================================================================================#"))
}

