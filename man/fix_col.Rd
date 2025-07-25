% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_col.R
\name{fix_col}
\alias{fix_col}
\title{Fix column with insufficient number of suppressed values.}
\usage{
fix_col(df, i, rc_char)
}
\arguments{
\item{df}{Dataframe with row to be fixed.}

\item{i}{Index of column to be fixed.}

\item{rc_char}{Character to replace suppressed value.}
}
\value{
A dataframe.
}
\description{
Fix column with insufficient number of suppressed values.
}
\examples{
x <-
 data.frame(
   x = c('-', '6', '8'),
   y = c('-', '6', '-'),
   z = c('10', '-', '9')
)

fix_col(x, 1, '-')
fix_col(x, 3, '-')
}
