# MKmisc
The repository includes the development version of R package MKmisc

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MKmisc)](http://cran.r-project.org/package=MKmisc)
[![cran checks](https://cranchecks.info/badges/summary/MKmisc)](https://cran.r-project.org/web/checks/check_results_MKmisc.html)

## Description
The package contains several functions for statistical data analysis; e.g. for 
sample size and power calculations, computation of confidence intervals, 
multiple imputation and generation of similarity matrices.


## Installation
The package requires Bioconductor package limma, which can be installed via

```{r, eval = FALSE}
## Install package BiocManager
install.packages("BiocManager")
## Use BiocManager to install limma
BiocManager::install("limma")
```

For the installation of package MKmisc use

```{r, eval = FALSE}
## Installation of CRAN version
install.packages("MKmisc")

## Or the development version from GitHub
# install.packages("remotes")
remotes::install_github("stamats/MKmisc")
```

## Getting started

```{r}
library(MKmisc)
```
