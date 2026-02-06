# Utilities used by methods and other utilities

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
                     percentile = function(x) quantile(x,
                                                       probs = c(0.025, 0.975),
                                                       na.rm = TRUE),
                     bca = function(x) bca(x, conf.level = 0.95))

    ## the summary stats -------

    iqr <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)

    ci <- ci_fun(x)

    tibble::tibble(boot_mean = mean(x, na.rm = TRUE),
                   boot_sd = sd(x, na.rm = TRUE),
                   boot_median = median(x, na.rm = TRUE),
                   boot_lower_iqr = iqr[1],
                   boot_upper_iqr = iqr[2],
                   boot_lower_ci = ci[1],
                   boot_upper_ci = ci[2])

  }

# Selection of repeated elements from a data frame -------

#' Filter a data frame with repeated variable levels.
#'
#' @description
#' Filters the input data frame by levels of a variable specified by the user
#' in a character vector called `levels`. If there are duplicated elements
#' in such character vector, they will be accounted for in the output
#' data frame: those levels will be duplicated es well. Note that this is
#' the difference between `filter_repeated()` and `dplyr::filter()`
#'
#' @details
#' Designed for internal use.
#'
#' @return a data frame.
#'
#' @param data a data frame.
#' @param variable character specifying the variable name.
#' @param levels a character vector with levels of the variable which will
#' be selected for the output.

  filter_repeated <- function(data, variable, levels) {

    ## entry control ------

    stopifnot(is.data.frame(data))

    stopifnot(is.character(levels))
    stopifnot(is.character(variable))

    if(!variable %in% names(data)) {

      stop("'variable' is absent from the input data frame.",
           call. = FALSE)

    }

    ## splitting and stitching -----

    if(!is.factor(data[[variable]])) {

      split_fct <- factor(data[[variable]])

    } else {

      split_fct <- data[[variable]]

    }

    data_lst <- split(data, f = split_fct)

    do.call('rbind', data_lst[levels])

  }

# Evaluation functions ------

  evaluate_stats <- function(x, resamples, FUN, summary_function = NULL, ...) {

    if(is.null(summary_function)) {

      boot_fun <- boot_summary

    } else {

      boot_fun <- summary_function

    }

    ## dataset estimates, output control ------

    dataset_estimate <- FUN(x, ...)

    if(!is.numeric(dataset_estimate)) {

      stop("'FUN' has to return a numeric vector.", call. = FALSE)

    }

    if(is.matrix(dataset_estimate)) {

      stop("'FUN' has to return a numeric vector.", call. = FALSE)

    }

    ## estimates for the resamples -------

    boot_estimates <- future_map(resamples, function(x) FUN(x, ...))

    boot_estimates <- try(as.data.frame(do.call('rbind', boot_estimates)))

    if(inherits(boot_estimates, 'try-error')) {

      err_txt <-
        paste("The user-provided 'FUN' arguments should be a function",
              'that returns a numeric vector',
              'which can be coerced to a numeric matrix.')

      stop(err_txt, call. = FALSE)

    }

    ## formatting the output -------

    if(ncol(boot_estimates) > 1) {

      boot_stats <- map(boot_estimates, boot_fun)

      if(is.null(summary_function)) {

        statistic <- NULL

        boot_stats <-
          map2_dfr(boot_stats, names(boot_stats),
                   ~mutate(.x, statistic = .y))

        boot_stats <- relocate(boot_stats, statistic)

      }

    } else {

      boot_stats <- boot_fun(boot_estimates[[1]])

    }

    c(list(dataset = dataset_estimate),
      list(bootstrap = boot_stats))

  }

# BCA confidence intervals ------------

#' Bias-corrected and accelerated (BCA) confidence intervals
#'
#' This function uses the method proposed by DiCiccio and Efron (1996) to
#' compute so called bias-corrected and accelerated (BCA) confidence intervals.
#' The code is a modified version of the code from
#' https://github.com/jkropko/coxed/blob/master/R/bca.R by Jonathan Kropko and
#' colleagues and used by the no-more CRAN package coxed.
#'
#' @param theta a numeric vector of estimates obtained in bootstrap estimates.
#' @param conf.level confidence interval. Defaults to 0.95.
#'
#' @details
#' Any NAs are silently removed.
#'
#'
#' @return returns a numeric vector of length 2 with the lower and upper bound
#' of the confidence interval
#'
#' @author Jonathan Kropko <jkropko@@virginia.edu> and Jeffrey J. Harden <jharden@@nd.edu>, based
#' on the code for the \code{\link[mediation]{mediate}} function in the \code{mediation} package
#' by Dustin Tingley, Teppei Yamamoto, Kentaro Hirose, Luke Keele, and Kosuke Imai.
#'
#' @references DiCiccio, T. J. and B. Efron. (1996). Bootstrap Confidence Intervals. \emph{Statistical Science}.
#' 11(3): 189–212. \url{https://doi.org/10.1214/ss/1032280214}

  bca <- function(theta, conf.level = 0.95){

    ## entry control ---------

    stopifnot(is.numeric(theta))
    stopifnot(is.numeric(conf.level))

    theta <- theta[!is.na(theta)]

    if(length(theta) < 2) {

      stop("Not enough elements in the 'theta' vector", call. = FALSE)

    }

    conf.level <- conf.level[1]

    if(var(theta) == 0){

      ## handling of the special case, when all elements are equal

      lower <- mean(theta)
      upper <- mean(theta)

      return(c(lower, upper))

    }

    if(any(is.infinite(theta))){

      stop("bca() function does not work when some values are infinite",
           call. = FALSE)

    }

    low <- (1 - conf.level)/2
    high <- 1 - low

    ## calculation of the CI

    sims <- length(theta)

    z.inv <- length(theta[theta < mean(theta)])/sims
    z <- qnorm(z.inv)

    U <- (sims - 1) * (mean(theta) - theta)

    top <- sum(U^3)
    under <- 6 * (sum(U^2))^{3/2}

    a <- top / under

    lower.inv <-  pnorm(z + (z + qnorm(low))/(1 - a * (z + qnorm(low))))
    lower <- quantile(theta, lower.inv, names=FALSE)

    upper.inv <-  pnorm(z + (z + qnorm(high))/(1 - a * (z + qnorm(high))))
    upper <- quantile(theta, upper.inv, names=FALSE)

    return(c(lower, upper))

  }

# END -------
