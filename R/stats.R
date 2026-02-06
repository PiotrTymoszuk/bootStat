# Computation of bootstrapped metrics

#' Bootstrapped descriptive statistic
#'
#' @description
#' Computes an estimate of the descriptive statistic (or statistics) returned
#' by a user-provided function in the genuine data set and in bootstrapped
#' samples of the data set.
#'
#' @details
#' The `bmap()` function is a S3 generic.
#' It will run in parallel when a parallel backend is registered
#' via `future::plan()`.
#' Specifically for data frames, the user may specify one or more columns
#' used to defined blocks used for block bootstrap. In such approach, levels
#' of the block structure (e.g. unique participant ID in a repeated measurement
#' data set) are sampled instead of observations. This option offers a
#' possibility to handle non-independently distributed or auto-correlated
#' observations.
#'
#' @return
#' A list of two elements `dataset` and `bootstrap` which store the evaluation
#' result in the entire dataset and bootstrapped metrics, respectively.
#'
#' @param x an object, currently atomic vectors, matrices and data frames
#' are implemented.
#' @param B number of bootstrapped samples.
#' @param FUN a user-provided function. It should return a numeric vector.
#' @param ci_method methods for calculation of confidence intervals:
#' `percentile` (default) or `bca` (bias corrected and accelerated).
#' Used by the \code{\link{boot_summary}} function.
#' @param summary_function a function used for calculation of the bootstrapped
#' metrics, can be virtually any user-provided function which takes
#' a numeric vector. If NULL, \code{\link{boot_summary}} is used.
#' @param ... extra arguments provided to the function `FUN`.
#' @inheritParams sboot
#'
#' @export

  bmap <- function(x, B, FUN, ...) {

    UseMethod('bmap')

  }

#' @rdname bmap
#' @export bmap.default
#' @export

  bmap.default <- function(x, B, FUN,
                           ci_method = c('percentile', 'bca'),
                           summary_function = NULL, ...) {

    ## entry control ------

    stopifnot(is.numeric(B))

    B <- as.integer(B)

    if(!is_function(FUN)) {

      stop("'FUN' has to be a valid R function.", call. = FALSE)

    }

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca'))

    if(!is.null(summary_function)) {

      if(!is_function(summary_function)) {

        stop("'summary_function' must be a valid R function.",
             call. = FALSE)

      }

    }

    ## resamples and computation of the stats ---------

    boot_samples <- sboot(x, B = B)

    evaluate_stats(x = x,
                   resamples = boot_samples,
                   FUN = FUN,
                   summary_function = summary_function, ...)

  }

#' @rdname bmap
#' @export bmap.data.frame
#' @export

  bmap.data.frame <- function(x, B, FUN,
                              ci_method = c('percentile', 'bca'),
                              summary_function = NULL, ...,
                              .by = NULL,
                              .drop = TRUE) {

    ## entry control ------

    stopifnot(is.numeric(B))

    B <- as.integer(B)

    if(!is_function(FUN)) {

      stop("'FUN' has to be a valid R function.", call. = FALSE)

    }

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca'))

    if(!is.null(summary_function)) {

      if(!is_function(summary_function)) {

        stop("'summary_function' must be a valid R function.",
             call. = FALSE)

      }

    }

    ## resamples and computation of the stats ---------

    boot_samples <- sboot(x, B = B, .by = .by, .drop = .drop)

    evaluate_stats(x = x,
                   resamples = boot_samples,
                   FUN = FUN,
                   summary_function = summary_function, ...)



  }

# END ------
