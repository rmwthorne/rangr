% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_functions.R
\name{truncate_homer}
\alias{truncate_homer}
\title{Truncates homer column names matching .Tag.Count(...)}
\usage{
truncate_homer(df, pattern = ".Tag.Count.{0,}?.$", replacement = "")
}
\arguments{
\item{df}{A data frame.}

\item{pattern}{character string containing a regular expression to trim.
Defaults to ".Tag.Count.\{0,\}?.$".}

\item{replacement}{a replacement for matched pattern in sub and gsub.
Defaults to "".}
}
\value{
A dataframe with truncated columns.
}
\description{
Truncates homer column names matching .Tag.Count(...)
}
