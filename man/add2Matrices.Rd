\name{add2Matrices}
\alias{add2Matrices}
\title{add to pre-existing list of matrices}
\usage{
  add2Matrices(inListMatrix, inList, rowNames = "pkgs",
    useCol)
}
\arguments{
  \item{inListMatrix}{the list of pre-existing matrices}

  \item{inList}{the list of data}

  \item{rowNames}{the list variable holding the row names
  in inList}
}
\description{
  As we add more results, we want to add data to each one,
  with the possibility that there are packages we didn't
  have already
}

