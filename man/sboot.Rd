% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling.R
\name{sboot}
\alias{sboot}
\alias{sboot.default}
\alias{sboot.matrix}
\alias{sboot.data.frame}
\title{Bootstrap samples a vector, matrix or a data frame}
\usage{
sboot(x, B, ...)

\method{sboot}{default}(x, B, ...)

\method{sboot}{matrix}(x, B, ...)

\method{sboot}{data.frame}(x, B, .by = NULL, .drop = TRUE, ...)
}
\arguments{
\item{x}{an object, currently a vector (default), matrix or a data frame.}

\item{B}{an integer, the number of bootstrapped samples to be returned.}

\item{...}{extra arguments passed to the methods.}

\item{.by}{a character vector of column names which defines blocks
in block bootstrap. Ignored if \code{NULL}: classical bootstrap.}

\item{.drop}{logical, should empty vector in the block structure be removed?
Defaults to \code{TRUE}. Ignored if \code{.by} is set to \code{NULL}.}
}
\value{
a named list of vectors, matrices or data frames named with the
\code{boot_n} scheme, where \code{n} denotes the sample number.
}
\description{
Creates a given number of bootstrap samples of a vector or a data frame.
}
\details{
The bootstraps are generated by drawing random samples of indexes (vector)
or row indexes (matrix or data frame) with replacement.
Specifically for data frames, the user may specify one or more columns
used to defined blocks used for block bootstrap. In such approach, levels
of the block structure (e.g. unique participant ID in a repeated measurement
data set) are sampled instead of observations. This option offers a
possibility to handle non-independently distributed or auto-correlated
observations.
The function \code{sboot()} is a S3 generic.
Works in parallel, if a parallel backend is registered via \code{future::plan()}.
}
