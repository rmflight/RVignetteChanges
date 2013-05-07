# check that what is in dev and on the web are the same thing for bioconductor
# note, they are most definitely not

#' gets a list of Bioconductor packages on the svn server
#' @param useLink which svn server link to use
#' @param usePWD which username password combination to use
#' @import XML
#' @import RCurl
#' @examples 
#' usepwd <- "readonly:readonly"
#' devLink <- "https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/"
#' getBiocPackages(devLink, usepwd)
getBiocPackages <- function(useLink, usePWD){
  allPackages <- getURL(useLink, userpwd=usePWD)
  allPackages <- readHTMLList(allPackages, trim=T)[[1]]
  isPackage <- grep("*/", allPackages)
  allPackages <- substr(allPackages[isPackage], 1, nchar(allPackages[isPackage]) - 1)
}

