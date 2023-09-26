# bootStat
Bootstrap Interface for Descriptive Statistics

## Description

The R package `bootStat` offers basically two tools: 

* `sboot()` which generates a list of bootstrapped samples of a vector, matrix or a data frame of a requested length

* `bmap()` which allows to compute bootstraps of virtually any numeric statistic of a vector, matrix or a data frame

## Installation

You may easily fetch the package with `devtools`: 

```r

devtools::install_github('PiotrTymoszuk/bootStat')

```

## Terms of use

The package is available under a [GPL-3 license](https://github.com/PiotrTymoszuk/bootStat/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

`clustTools` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [coxed](https://cran.r-project.org/web/packages/coxed/index.html) and [furrr](https://furrr.futureverse.org/). Many thanks to their developers, maintainers and contributors.
