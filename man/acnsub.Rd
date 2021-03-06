% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.r
\name{acnsub}
\alias{acnsub}
\title{Analysis of the CN sub categories.}
\usage{
acnsub(from = Sys.Date() - 7, to = Sys.Date(), sql_con, if_plot = FALSE, top = 10)
}
\arguments{
\item{from}{A Date object or character in '2017-10-24' format of the beginning of the period}

\item{to}{A Date object or character in '2017-10-24' format of the end of the period}

\item{sql_con}{A SQL connection}

\item{if_plot}{A logic value of whether plot the time series}

\item{top}{A numeric value of the Top tags for plotting}
}
\value{
A figure showing the active cn sub tag
}
\description{
Analysis of the CN sub categories.
}
