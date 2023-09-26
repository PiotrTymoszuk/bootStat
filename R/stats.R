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
                           summary_function = NULL,...) {

    ## entry control ------

    stopifnot(is.numeric(B))

    B <- as.integer(B)

    if(!rlang::is_function(FUN)) {

      stop("'FUN' has to be a valid R function.", call. = FALSE)

    }

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca'))

    if(is.null(summary_function)) {

      boot_fun <- function(x) boot_summary(x = x, ci_method = ci_method)

    } else {

     boot_fun <- summary_function

    }

    if(!rlang::is_function(boot_fun)) {

      stop("'summary_function' must be a valid R function.",
           call. = FALSE)

    }

    ## whole dataset estimates and estimates for the bootstraps --------

    dataset_estimate <- FUN(x, ...)

    if(!is.numeric(dataset_estimate)) {

      stop("'FUN' has to return a numeric vector.", call. = FALSE)

    }

    if(is.matrix(dataset_estimate)) {

      stop("'FUN' has to return a numeric vector.", call. = FALSE)

    }

    boot_samples <- sboot(x, B = B)

    boot_estimates <- furrr::future_map(boot_samples, function(x) FUN(x, ...))

    boot_estimates <- try(as.data.frame(do.call('rbind', boot_estimates)))

    if(inherits(boot_estimates, 'try-error')) {

      err_txt <-
        paste("The user-provided 'FUN' arguments should be a function",
              'that returns a numeric vector',
              'which can be coerced to a numeric matrix.')

      stop(err_txt, call. = FALSE)

    }

    ## bootstrapped stats --------

    if(ncol(boot_estimates) > 1) {

      boot_stats <- purrr::map(boot_estimates, boot_fun)

      if(is.null(summary_function)) {

        statistic <- NULL

        boot_stats <-
          purrr::map2_dfr(boot_stats, names(boot_stats),
                          ~dplyr::mutate(.x, statistic = .y))

        boot_stats <- dplyr::relocate(boot_stats, statistic)

      }

    } else {

      boot_stats <- boot_fun(boot_estimates[[1]])

    }

    c(list(dataset = dataset_estimate),
      list(bootstrap = boot_stats))

  }

# END ------
