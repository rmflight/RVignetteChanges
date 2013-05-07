\name{getBiocPackages}
\alias{getBiocPackages}
\title{gets a list of Bioconductor packages on the svn server}
\usage{
  getBiocPackages(useLink, usePWD)
}
\arguments{
  \item{useLink}{which svn server link to use}

  \item{usePWD}{which username password combination to use}
}
\description{
  gets a list of Bioconductor packages on the svn server
}
\examples{
usepwd <- "readonly:readonly"
devLink <- "https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/"
getBiocPackages(devLink, usepwd)
}

