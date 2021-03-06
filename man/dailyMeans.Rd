% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dailyMeans.R
\name{dailyMeans}
\alias{dailyMeans}
\title{Aggregates sub-daily raster time series to daily means.}
\usage{
dailyMeans(variable, cores = 10, timedate, clcall = NULL)
}
\arguments{
\item{variable}{A \code{RasterBrick} or \code{RasterStack} object with values on a sub-daily
resolution.}

\item{cores}{The number of cores to be used in parallel computing.}

\item{timedate}{A \code{POSIXct} vector containing the time
information for each band/layer of \code{variable}.}

\item{clcall}{A function passed to \code{\link[parallel]{clusterCall}}.}
}
\value{
A list with the first element representing the aggregated
raster time series with daily mean values as a \code{RasterBrick} and
the second element a \code{POSIXct} vector containing the date information
(i.e. the days).
}
\description{
\code{dailyMeans} aggregates raster based time series
(\code{RasterBrick} or \code{RasterStack} object, see:
\code{\link[raster]{Raster-class}}) on a sub-daily basis
by computing the respective daily mean values for each
raster cell.
}
\examples{
#
}
