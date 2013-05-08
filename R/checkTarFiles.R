#' gets the list of packages contained in a specified directory
#' @param checkDir the directory containing the packages
#' @export
#' @return the returned values in the list are:
#' \itemize{
#'  \item{files: }{the tar files}
#'  \item{package: }{the package names}
#'  \item{version: }{the package versions}
#'  }
getPackageFiles <- function(checkDir){
  packageType <- "tar.gz"
  allFiles <- dir(checkDir, pattern=packageType)
  noTar <- strtrim(allFiles, nchar(allFiles) - 7)
  
  nameVer <- strsplit(noTar, "_")
  packageNames <- sapply(nameVer, function(x)x[1])
  packageVer <- sapply(nameVer, function(x)x[2])
  
  return(list(files=allFiles, package=packageNames, version=packageVer))
}

#' checks tar gz files for presence of latex and markdown vignettes
#' @param fileList the list of files to check
#' @return list containing
#' \itemize{
#'  \item{rmd: }{whether or not it has a markdown vignette}
#'  \item{rnw: }{whether or not it has a latex vignette}
#'  }
#'  @export
checkTarVignette <- function(fileList){
  rnwReg <- c("^(.*)(inst/doc)(.*)+rnw", "^(.*)(vignettes/)(.*)+rnw")
  rmdReg <- c("(/inst/doc/)*rmd", "(vignettes/)*rmd")
  
  hasVignettes <- lapply(fileList, function(inFile){
    tarFiles <- untar(inFile, list=T)
    hasRNW <- sapply(rnwReg, grep, tarFiles, ignore.case=T)
    hasRNW <- sum(sapply(hasRNW, length)) > 0
    
    hasRMD <- sapply(rmdReg, grep, tarFiles, ignore.case=T)
    hasRMD <- sum(sapply(hasRMD, length)) > 0
    
    return(list(rmd=hasRMD, rnw=hasRNW))
  })
  names(hasVignettes) <- fileList
  return(hasVignettes)
}