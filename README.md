# The `certetemplate` package for R

[![certetemplate status badge](https://certe-medical-epidemiology.r-universe.dev/badges/certetemplate?color=01617e)](https://certe-medical-epidemiology.r-universe.dev)
[![codecov](https://codecov.io/gh/certe-medical-epidemiology/certetemplate/branch/main/graph/badge.svg)](https://codecov.io/gh/certe-medical-epidemiology/certetemplate)
[![R-CMD-check](https://github.com/certe-medical-epidemiology/certetemplate/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/certe-medical-epidemiology/certetemplate/actions/workflows/R-CMD-check.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certetemplate/badge/main)](https://www.codefactor.io/repository/github/certe-medical-epidemiology/certetemplate/overview/main)

<img src="./logo.svg" height="150px" />

### General Information

This is an R package developed by [**Certe**](https://www.certe.nl), a non-profit medical laboratory in the Northern Netherlands that provides routine diagnostic tests for clinical chemistry and clinical microbiology, as well as medical logistics and a thrombosis service. Their department of Medical Epidemiology, which developed this R package, conducts (and develops new methods for) medical data analyses, for both routine workflows and scientific research.

For all our packages, please visit [our GitHub organisation overview](https://github.com/certe-medical-epidemiology). Our R packages are not on CRAN since their use is primarily intended for own staff, but they are publicly available to support open science. 

#### R-universe

All our R packages are published [here at R-universe](https://certe-medical-epidemiology.r-universe.dev), allowing anyone to install and update the packages using common methods, such as the RStudio menu bar or `install.packages()`. To use the R-universe of Certe Medical Epidemiology, run:

```r
# set the repos option to include the Certe Medical Epidemiology R-universe
options(repos = c(
  CerteMedEpi = "https://certe-medical-epidemiology.r-universe.dev",
  CRAN = "https://cloud.r-project.org"))

# our 'loader package' certedata installs all Certe R packages:
install.packages("certedata")
```

### Copyright regarding this software

We believe open science matters, which is why this package is available on GitHub.

This R package is free, open-source software and licensed under the [GNU General Public License v2.0 (GPL-2)](./LICENSE.md). In a nutshell, this means that this package:

- May be used for commercial purposes
- May be used for private purposes
- May **not** be used for patent purposes
- May be modified, although:
  - Modifications **must** be released under the same license when distributing the package
  - Changes made to the code **must** be documented
- May be distributed, although:
  - Source code **must** be made available when the package is distributed
  - A copy of the license and copyright notice **must** be included with the package.
- Comes with a LIMITATION of liability
- Comes with NO warranty

----

© Certe Medical Diagnostics & Advice Foundation - [www.certe.nl](https://www.certe.nl). Certe is the sole copyright holder and funder of this software.
