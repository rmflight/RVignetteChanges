# RVignetteChanges

## Installation

```
library(devtools)
install_github("RVignetteChanges", "rmflight")
```

## Background

With the release of `R 3.0.0`, package authors have the opportunity to write vignettes using alternative engines than the default `Sweave`. For example, it is now possible to generate `html` vignettes using `knitr`. 

## Purpose


We would like to examine **CRAN** and **Bioconductor** `R` packages to track:

  * if there is a change in the frequency of new packages that include vignettes (in the case of CRAN, where vignettes are optional)
  * how many packages switch from Sweave to alternative vignette engines (CRAN and Bioconductor)

## Frequency

The **Bioconductor** SVN repo is being crawled weekly, and a **CRAN** mirror is being queried weekly for changed packages.

## Data availability

As of July 18 2013, summarized **Bioconductor** data is available as three matrices. For each package queried, is there a sweave (rnw) or markdown (rmd) vignette. Use `load(bioconductorData)` to load the data and examine it.

Functions to query the `tar.gz` files still need to be written.