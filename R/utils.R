# Utilities used by methods

# Bootstrap statistic summary -----

#' Summary of bootstrapped statistics
#'
#' @description
#' A default function used by the package to compute summary metrics of a
#' bootstrapped statistic.
#'
#' @details
#' Silently ignores any missing values.
#'
#'
#' @return
#' A data frame with the following columns:
#'
#' * `boot_mean` and `boot_sd`: mean and standard deviation across
#' the bootstraps
#'
#' * `boot_median`: median bootstrapped value
#'
#' * `boot_lower_iqr` and `boot_upper_iqr`: lower and upper bound of the
#' interquartile range
#'
#' * `boot_lower_ci` and `boot_upper_ci`: lower and upper bound of the
#' 95% confidence interval
#'
#' @param x a numeric vector.
#' @param ci_method methods for calculation of confidence intervals:
#' `percentile` (default) or `bca` (bias corrected and accelerated).
#'
#' @export

  boot_summary <- function(x, ci_method = c('percentile', 'bca')) {

    ## entry control ------

    stopifnot(is.numeric(x))

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca'))

    ci_fun <- switch(ci_method,
                     percentile = function(x) stats::quantile(x, probs = c(0.025, 0.975), na.rm = TRUE),
                     bca = function(x) coxed::bca(x, conf.level = 0.95))

    ## the summary stats -------

    iqr <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)

    ci <- ci_fun(x)

    tibble::tibble(boot_mean = mean(x, na.rm = TRUE),
                   boot_sd = stats::sd(x, na.rm = TRUE),
                   boot_median = stats::median(x, na.rm = TRUE),
                   boot_lower_iqr = iqr[1],
                   boot_upper_iqr = iqr[2],
                   boot_lower_ci = ci[1],
                   boot_upper_ci = ci[2])

  }

# END -------
