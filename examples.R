# Usage of the package

# packages -------

  library(tidyverse)
  library(caret)
  library(future)
  library(bootStat)
  library(MASS)

  select <- dplyr::select

# sampling ------

  set.seed(1234)

  ## bootstrap samples of a vector:

  sboot(LETTERS, B = 100)

  ## of a matrix:

  sboot(iris3[, ,'Virginica'], B = 100)

  ## of a data frame

  sboot(mtcars, B = 200)

# a factor dataset ----------

  set.seed(1234)

  fct_dataset <-
    data.frame(obs = sample(c('no', 'yes'), size = 100, replace = TRUE),
               pred = sample(c('no', 'yes'), size = 100, replace = TRUE)) %>%
    map_dfc(factor)

  yes_freq <- function(x) sum(x == 'yes')

  my_summary <- function(x) quantile(x, probs = c(0.1, 0.5, 0.9))

  multiClassSummary(as.data.frame(fct_dataset), lev = c('no', 'yes'))

# bootstrapped inter-rater and ROC stats --------

  fct_stats <- bmap(x = as.data.frame(fct_dataset),
                    B = 100,
                    FUN = yes_freq,
                    summary_function = my_summary)

  plan('multisession')

  roc_stats <- bmap(x = as.data.frame(fct_dataset),
                    B = 2000,
                    FUN = multiClassSummary,
                    lev = c('no', 'yes'),
                    ci_method = 'bca')

  roc_stats_custom <- bmap(x = as.data.frame(fct_dataset),
                           B = 2000,
                           FUN = multiClassSummary,
                           lev = c('no', 'yes'),
                           ci_method = 'bca',
                           summary_function = my_summary)

  plan('sequential')

# Numeric metrics --------

  iris %>%
    select(- Species) %>%
    bmap(B = 100,
         FUN = function(x) map_dbl(x, mean))

# Block bootstrap -------

  my_bacteria <- as_tibble(MASS::bacteria)

  bact_presence <- function(x) table(x[['y']])[2]/sum(table(x[['y']]))

  plan('multisession')

  ## benchmarking the canonical and block bootstrap resampling

  system.time({sboot(my_bacteria, B = 1000, .by = c('ID', 'trt'))})
  system.time({sboot(my_bacteria, B = 1000, .by = NULL)})

  plan('sequential')

  plan('multisession')

  system.time({my_bacteria %>%
      split(f = my_bacteria$trt) %>%
      map(bmap,
          B = 1000,
          FUN = bact_presence)})

  system.time({my_bacteria %>%
      split(f = my_bacteria$trt) %>%
      map(bmap,
          B = 1000,
          FUN = bact_presence,
          .by = 'ID')})

  plan('sequential')

# END -------
