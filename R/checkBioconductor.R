# check that what is in dev and on the web are the same thing for bioconductor
# note, they are most definitely not

#' gets a list of Bioconductor packages on the svn server
#' @param useLink which svn server link to use
#' @param usePWD which username password combination to use
#' @import XML
#' @import RCurl
#' @export
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

#' checks the content of the data on the SVN server provided
#' @param packageList the list of packages to check
#' @param baseURL the url of the svn server to work with
#' @param usePWD the svn password to use to get access to files
#' @export
checkBiocPackages <- function(packageList, baseURL, usePWD="readonly:readonly"){
  potLocs <- c("vignettes/", "inst/doc/")
    
  packageResults <- lapply(packageList, function(inPackage){
    # try vignettes first, then "inst/doc/"
    rmdSrch <- c(".Rmd", ".rmd", ".RMD")
    rnwSrch <- c(".Rnw", ".rnw", ".RNW")
    hasRmd <- FALSE
    hasRnw <- FALSE
    outVersion <- NA
    
    print(inPackage)
    
    vigLoc <- file.path(baseURL, inPackage, potLocs[1], fsep="/")
    vigFiles <- getFileListing(vigLoc, usePWD)
    
    if (!is.na(vigFiles)){
      hasRmd <- hasVignette(vigFiles[[1]], rmdSrch)
      hasRnw <- hasVignette(vigFiles[[1]], rnwSrch)
    } else {
      instLoc <- file.path(baseURL, inPackage, potLocs[2], fsep="/")
      instFiles <- getFileListing(instLoc, usePWD)
      if (!is.na(instFiles)){
        hasRmd <- hasVignette(instFiles[[1]], rmdSrch)
        hasRnw <- hasVignette(instFiles[[1]], rnwSrch)
      }
    }
    
    if (hasRmd | hasRnw){
      descLoc <- file.path(baseURL, inPackage, "DESCRIPTION", fsep="/")
      outVersion <- checkBiocVersion(descLoc, usePWD)
    }
    return(list(hasRmd=hasRmd, hasRnw=hasRnw, version=outVersion))
  })
  names(packageResults) <- packageList
  return(packageResults)
}



#' creates a list from the url
#' @param inURL the url to go fetch
#' @param usePWD the password for the svn server
#' @import XML
#' @import RCurl
getFileListing <- function(inURL, usePWD="readonly:readonly"){
  if (url.exists(inURL, userpwd=usePWD)){
    outList <- readHTMLList(getURL(inURL, userpwd=usePWD))
  } else {
    outList <- NA
  }
  return(outList)
}

#' searches the file listing for an "Rmd" vignette
#' @param fileList the list of files to grep over
#' @param grepStr a list of strings to search for presence of in file listing
hasVignette <- function(inList, grepStr){
  checkVignette <- sapply(grepStr, function(inStr){
    length(grep(inStr, inList, fixed=T))
  })
  if (sum(checkVignette) == 0){
    hasVignette=F
  } else {
    hasVignette=T
  }
  return(hasVignette)
}

#' checks the version of the package on the server
#' @param inURL full url to the package
#' @param usePWD the username and password to use
#' @import RCurl
checkBiocVersion <- function(inURL, usePWD="readonly:readonly"){
  outVersion <- NA
  descriptionFile <- getURL(inURL, userpwd=usePWD)
  splitDesc <- strsplit(descriptionFile, "\n")[[1]]
  hasVersion <- grep("Version:", splitDesc, value=T)
  if (length(hasVersion) != 0){
    outVersion <- strsplit(hasVersion, ": ")[[1]][2]
  }
  return(outVersion)
}
