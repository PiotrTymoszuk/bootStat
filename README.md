[![R](https://github.com/PiotrTymoszuk/bootStat/actions/workflows/r.yml/badge.svg)](https://github.com/PiotrTymoszuk/bootStat/actions/workflows/r.yml)

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

## Usage

### Bootstrap samples

<details>

Bootstraps are samples of an object such as vector, matrix or a data frame with the same length as the genuine feature obtained by random sampling with replacement. 
The function `sboot()` provides an easy way to create them: 

```r
library(bootStat)
library(tidyverse)

set.seed(1234)

## bootstrap samples of a vector:

sboot(LETTERS, B = 100)
  
## of a matrix:
  
sboot(iris3[, ,'Virginica'], B = 100)
  
## of a data frame
  
sboot(mtcars, B = 200)

```
The `B` argument is an integer that controls the number of samples. The `sboot()` function always returns a named list.
   
</details>

### Bootstrapped descriptive statistic

<details>

The classical application of the [bootstrap workflow](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)) is to obtain confidence intervals of a descriptive statistic even when we have no clear idea on its distribution. Let's have a look at the inter-rater reliability problem addressed by [Cohen's kappa metric](https://en.wikipedia.org/wiki/Cohen%27s_kappa) in a simulated data set. Let the `obs` variable represent the observed phenomenon and the `pred` the prediction, each of them being a binary yes/no feature. 

```r
set.seed(1234)

  fct_dataset <-
    data.frame(obs = sample(c('no', 'yes'), size = 100, replace = TRUE),
               pred = sample(c('no', 'yes'), size = 100, replace = TRUE)) %>%
    map_dfc(factor)

> fct_dataset
# A tibble: 100 × 2
   obs   pred 
   <fct> <fct>
 1 yes   yes  
 2 yes   no   
 3 yes   no   
 4 yes   no   
 5 no    no   
 6 yes   yes  
 7 no    no   
 8 no    yes  
 9 no    no   
10 yes   no   
# … with 90 more rows
# ℹ Use `print(n = ...)` to see more rows

```
The kappa statistic as well as additional metrics of ROC (receiver operating characteristic) such as sensitivity, specificity, recall and precision can be computed with the `multiClassSummary()` function provided by the `caret` package:

```r

library(caret)

> multiClassSummary(as.data.frame(fct_dataset), lev = c('no', 'yes'))

         Accuracy             Kappa                F1       Sensitivity       Specificity    Pos_Pred_Value    Neg_Pred_Value 
        0.5600000         0.1143317         0.5111111         0.5476190         0.5689655         0.4791667         0.6346154 
        Precision            Recall    Detection_Rate Balanced_Accuracy 
        0.4791667         0.5476190         0.2300000         0.5582923

```
Bootstrapped 95% confidence intervals for this bunch of descriptive statistics can be obtained with the bootStat's `bmap()` function, which takes a vector, matrix or data frame as the first argument and an user-provided function used for computation of the requested numeric statistic as another argument, `FUN`. The FUN-specific arguments are passed simply via the R's `...` mechanism. Of note, the function will run in parallel, when the respective backend is registeged by `future::plan()`:

```r

## marking it run in parallel

library(future)

plan('multisession')

## computing the bootstrapped kappa and ROC statistics with bmap() and caret's multiClassSummary()

roc_stats <- bmap(x = as.data.frame(fct_dataset),
                  B = 2000,
                  FUN = multiClassSummary,
                  lev = c('no', 'yes'),
                  ci_method = 'bca')

> roc_stats
$dataset
         Accuracy             Kappa                F1       Sensitivity       Specificity    Pos_Pred_Value    Neg_Pred_Value 
        0.5600000         0.1143317         0.5111111         0.5476190         0.5689655         0.4791667         0.6346154 
        Precision            Recall    Detection_Rate Balanced_Accuracy 
        0.4791667         0.5476190         0.2300000         0.5582923 

$bootstrap
# A tibble: 11 × 8
   statistic         boot_mean boot_sd boot_median boot_lower_iqr boot_upper_iqr boot_lower_ci boot_upper_ci
   <chr>                 <dbl>   <dbl>       <dbl>          <dbl>          <dbl>         <dbl>         <dbl>
 1 Accuracy              0.560  0.0487       0.56          0.53            0.59         0.45           0.65 
 2 Kappa                 0.112  0.0969       0.111         0.0491          0.18        -0.0809         0.297
 3 F1                    0.508  0.0644       0.506         0.467           0.552        0.379          0.632
 4 Sensitivity           0.546  0.0767       0.548         0.489           0.6          0.391          0.691
 5 Specificity           0.569  0.0639       0.571         0.526           0.611        0.444          0.693
 6 Pos_Pred_Value        0.479  0.0717       0.478         0.431           0.529        0.341          0.619
 7 Neg_Pred_Value        0.634  0.0643       0.635         0.592           0.675        0.509          0.76 
 8 Precision             0.479  0.0717       0.478         0.431           0.529        0.341          0.619
 9 Recall                0.546  0.0767       0.548         0.489           0.6          0.391          0.691
10 Detection_Rate        0.230  0.0418       0.23          0.2             0.26         0.143          0.31 
11 Balanced_Accuracy     0.558  0.0498       0.557         0.525           0.591        0.458          0.652

```

The `bmap()` function returns always a list of two elements: `dataset` with the estimates in the geniune data set and `bootstrap` with the boostrapped metrics. Copncerning the confidence intervals, the user may choose between percentile confidence intervals (default) and, often more accurate, bias-corrected and accelerated intervals (BCA). 

The user may also specify the output of the boostrrapped metrics by providing a custom `summary_function`:

```r

## customized output of the bootstrapped Cohen's kappa and ROC statistics:
## provide a custom function operating on a numeric vector
##
## In this case I'd like to know the 90% percentile confidence intervals and median
## of the boostrapped stats:

my_summary <- function(x) quantile(x, probs = c(0.1, 0.5, 0.9))

roc_stats_custom <- bmap(x = as.data.frame(fct_dataset),
                           B = 2000,
                           FUN = multiClassSummary,
                           lev = c('no', 'yes'),
                           ci_method = 'bca', 
                           summary_function = my_summary)

> roc_stats_custom
$dataset
         Accuracy             Kappa                F1       Sensitivity       Specificity    Pos_Pred_Value    Neg_Pred_Value 
        0.5600000         0.1143317         0.5111111         0.5476190         0.5689655         0.4791667         0.6346154 
        Precision            Recall    Detection_Rate Balanced_Accuracy 
        0.4791667         0.5476190         0.2300000         0.5582923 

$bootstrap
$bootstrap$Accuracy
 10%  50%  90% 
0.49 0.56 0.62 

$bootstrap$Kappa
        10%         50%         90% 
-0.01611988  0.10996835  0.23868313 

$bootstrap$F1
      10%       50%       90% 
0.4235294 0.5102041 0.5882353 

$bootstrap$Sensitivity
      10%       50%       90% 
0.4468085 0.5446571 0.6470588 

$bootstrap$Specificity
      10%       50%       90% 
0.4905290 0.5666667 0.6500000 

$bootstrap$Pos_Pred_Value
      10%       50%       90% 
0.3859649 0.4782609 0.5682264 

$bootstrap$Neg_Pred_Value
      10%       50%       90% 
0.5483871 0.6363636 0.7209302 

$bootstrap$Precision
      10%       50%       90% 
0.3859649 0.4782609 0.5682264 

$bootstrap$Recall
      10%       50%       90% 
0.4468085 0.5446571 0.6470588 

$bootstrap$Detection_Rate
 10%  50%  90% 
0.18 0.23 0.28 

$bootstrap$Balanced_Accuracy
      10%       50%       90% 
0.4916667 0.5572488 0.6223322

```   
</details>

## Terms of use

The package is available under a [GPL-3 license](https://github.com/PiotrTymoszuk/bootStat/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

`clustTools` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [coxed](https://cran.r-project.org/web/packages/coxed/index.html) and [furrr](https://furrr.futureverse.org/). Many thanks to their developers, maintainers and contributors.
