% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/summary-methods.R
\name{summary}
\alias{summary}
\alias{summary,LM-method}
\title{Summary methods}
\usage{
\S4method{summary}{LM}(object, ...)
}
\arguments{
\item{object}{An object to be summarized.}

\item{...}{Further arguments passed to methods.}
}
\description{
Provides summary methods for objects.
}
\section{Methods (by class)}{
\itemize{
\item \code{LM}: Summarizes the results of the overall test and the
tests for each parameter.
}}

\examples{
data("mtcars")
fit <- el_lm(mpg ~ wt, data = mtcars)
summary(fit)
}
