% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_egen.R
\name{row_egen}
\alias{row_egen}
\alias{row_mean}
\alias{row_sum}
\alias{row_max}
\alias{row_miss}
\alias{row_non_miss}
\alias{row_min}
\alias{row_median}
\alias{row_var}
\alias{row_sd}
\alias{row_unique}
\title{Generate a variable based on row-wise operations.}
\usage{
row_mean(...)

row_sum(...)

row_max(...)

row_miss(...)

row_non_miss(...)

row_min(...)

row_median(...)

row_var(...)

row_sd(...)

row_unique(...)
}
\arguments{
\item{...}{a data frame or tibble, or <\code{\link[=dplyr_tidy_select]{tidy-select}}> within the mutate() function.}
}
\value{
A numeric vector.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
\itemize{
\item \code{row_mean()} - Calculate the row-wise mean of selected varaibles.
\item \code{row_sum()} - Calculate the row-wise sum of selected varaibles.
\item \code{row_max()} - Calculate the row-wise maximum of selected varaibles.
\item \code{row_min()} - Calculate the row-wise minimum of selected varaibles.
\item \code{row_median()} - Calculate the row-wise median of selected varaibles.
\item \code{row_var()} - Calculate the row-wise variance of selected varaibles.
\item \code{row_sd()} - Calculate the row-wise standard deviation of selected varaibles.
\item \code{row_unique()} - Calculate the row-wise number of unique values of selected varaibles.
\item \code{row_miss()} - Calculate the row-wise number of missing values of selected varaibles.
\item \code{row_non_miss()} - Calculate the row-wise number of non-missing values of selected varaibles.
}
}
\examples{
tb <- tibble::tibble(
  x = 1:10,
  y = 11:20,
  z = 21:30
)

row_mean(tb)

row_sum(tb)

row_non_miss(tb)

tb \%>\%
  dplyr::mutate(
    mean = row_mean(x, y),
    sum = row_sum(x, y)
  )
}
