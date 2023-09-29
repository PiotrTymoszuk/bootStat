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

    boot_estimates <- furrr::future_map(resamples, function(x) FUN(x, ...))

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

# END -------
