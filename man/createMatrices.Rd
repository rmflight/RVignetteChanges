\name{createMatrices}
\alias{createMatrices}
\title{create matrices of the appropriate type for storing data}
\usage{
  createMatrices(inList,
    useVars = c("rmd", "rnw", "version"),
    rowNames = "pkgs", numCol = 10)
}
\arguments{
  \item{inList}{the list to transform to matrices}

  \item{useVars}{which list variables to make matrices}

  \item{rowNames}{which list variable has the rownames for
  the matrix}

  \item{numCol}{the number of columns to pad the matrix
  with}
}
\description{
  We want matrices where rows are packages, and columns are
  dates for each variable we are storing. This does it all
  in one go. Assumes that each list variable is a single
  vector
}

