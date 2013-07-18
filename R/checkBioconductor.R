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


#' grabs the hasRmd and version from the bioconductor data
#' 
#' @param inList the list of results from checking bioconductor
#' @export
getBiocValues <- function(inList){
  rmdStatus <- sapply(inList, function(x){x$hasRmd})
  rnwStatus <- sapply(inList, function(x){x$hasRnw})
  packageVersion <- sapply(inList, function(x){x$version})
  return(list(rmd=rmdStatus, rnw=rnwStatus, version=packageVersion, pkgs=names(rmdStatus)))
}

#' generate three matrices for the data over all the files
#' 
#' @param useDir which directory to read the files from
#' @param filePattern the pattern to use to list the files
#' @param minIndex what is the minimum index of the file in chronological order
#' @export
biocValues2Matrix <- function(useDir, filePattern, minIndex=2){
  fileList <- dir(useDir, filePattern)
  fileInfo <- file.info(file.path(useDir, fileList))
  fileInfo <- fileInfo[order(fileInfo$ctime),]
  
  fileInfo <- fileInfo[seq(minIndex, nrow(fileInfo)),]
  
  nFile <- nrow(fileInfo)
  
  fileNames <- rownames(fileInfo)
  
  load(fileNames[nFile])
  
  devVals <- getBiocValues(devStatus)
  devMatrix <- createMatrices(devVals, numCol=nFile)
  
  relVals <- getBiocValues(relStatus)
  relMatrix <- createMatrices(relVals, numCol=nFile)
  
  for (iFile in seq(nFile-1, 1, -1)){
    load(fileNames[iFile])
    
    devVals <- getBiocValues(devStatus)
    devMatrix <- add2Matrices(devMatrix, devVals, rowNames="pkgs", useCol=iFile)
    
    relVals <- getBiocValues(relStatus)
    relMatrix <- add2Matrices(relMatrix, relVals, rowNames="pkgs", useCol=iFile)
  }
  
  return(list(dev=devMatrix, rel=relMatrix))
}

#' create matrices of the appropriate type for storing data
#' 
#' We want matrices where rows are packages, and columns are dates for each variable we are storing. This does it all in one go. Assumes that each list variable is a single vector
#' 
#' @param inList the list to transform to matrices
#' @param useVars which list variables to make matrices
#' @param rowNames which list variable has the rownames for the matrix
#' @param numCol the number of columns to pad the matrix with
#' @export
createMatrices <- function(inList, useVars=c("rmd", "rnw", "version"), rowNames="pkgs", numCol=10){
  numRow <- length(inList[[rowNames]])
  nVar <- length(useVars)
  outMatrices <- list(nVar)
  
  varTypes <- sapply(inList[useVars], class)
  
  for (iVar in 1:length(useVars)){
    if (varTypes[iVar] == "logical"){
      tmpMat <- matrix(FALSE, numRow, numCol)
    } else if (varTypes[iVar] == "character"){
      tmpMat <- matrix("NA", numRow, numCol)
    }
    rownames(tmpMat) <- inList[[rowNames]]
    
    tmpMat[inList[[rowNames]],numCol] <- inList[[iVar]]
    outMatrices[[iVar]] <- tmpMat
        
  }
  
  names(outMatrices) <- useVars
  return(outMatrices)
}

#' add to pre-existing list of matrices
#' 
#' As we add more results, we want to add data to each one, with the possibility that there are packages we didn't have already
#' 
#' @param inListMatrix the list of pre-existing matrices
#' @param inList the list of data
#' @param rowNames the list variable holding the row names in inList
#' @export
add2Matrices <- function(inListMatrix, inList, rowNames="pkgs", useCol){
  addRows <- FALSE
  
  orgRows <- rownames(inListMatrix[[1]])
  newRows <- inList[[rowNames]]
  
  rows2Add <- newRows[!(newRows %in% orgRows)]
  
  if (length(rows2Add) != 0){
    addRows <- TRUE
    nAdd <- length(rows2Add)
  }
  
  
  nVar <- length(inListMatrix)
  varNames <- names(inListMatrix)
  varTypes <- sapply(inListMatrix, function(x){class(x[1])})
  
  
  for (iVar in 1:nVar){
    if (addRows){
      if (varTypes == "logical"){
        addMat <- matrix(FALSE, nAdd, ncol(inListMatrix[[iVar]]))
      } else if (varTypes == "character"){
        addMat <- matrix("NA", nAdd, ncol(inListMatrix[[iVar]]))
      }
      inListMatrix[[iVar]] <- rbind(inListMatrix[[iVar]], addMat)
    }
    inListMatrix[[iVar]][newRows, useCol] <- inList[[iVar]]
  }
  return(inListMatrix)
}