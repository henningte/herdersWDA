% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluateDroughtNema1.R
\name{evaluateDroughtNema1}
\alias{evaluateDroughtNema1}
\title{Evaluates Drought Conditions (All Criteria) Based on Raster Time Series.}
\usage{
evaluateDroughtNema1(droughttemperaturemain, droughttemperatureadditional,
  droughtprecipitation, droughtairhumidity, timedate)
}
\arguments{
\item{droughttemperaturemain}{A \code{RasterBrick} object a returned by
\code{\link{evaluateDroughtNemaTemperatureMain}} containing the classification
if the temepratures of respective ten-day intervals for a pixel result in
drought conditions or near-drought conditions (according to the meain criteria).}

\item{droughttemperatureadditional}{A \code{RasterBrick} object a returned by
\code{\link{evaluateDroughtNemaTemperatureAdditional}} containing the classification
if the temepratures of respective ten-day intervals for a pixel result in
drought conditions (according to the additional criteria).}

\item{droughtprecipitation}{A \code{RasterBrick} object a returned by
\code{\link{evaluateDroughtNemaPrecipitation}} containing the classification
if the precipitation of respective ten-day intervals for a pixel result in
drought conditions or near-drought conditions.}

\item{droughtairhumidity}{A \code{RasterBrick} object a returned by
\code{\link{evaluateDroughtNemaAirhumidity}} containing the classification
if the relative air humidity of respective ten-day intervals for a pixel result in
drought conditions.}

\item{timedate}{A \code{POSIXct} vector containing the time information
for all layers of the input raster objects.}
}
\value{
A list with two elements:
\describe{
  \item{\code{drought}}{A \code{RasterBrick} object containing for each
  fixed ten-day interval  a layer classifying if there were
  drought conditions or near-drought conditions according to all criteria or not.}
  \item{\code{time}}{A character vector specifying the date of the first day of the
  respective fixed ten-day interval.}
}
}
\description{
\code{evaluateDroughtNema1} evaluates
for a target time period, if
for a given spatial and temporal point drought or near drought conditions are
prevalent or not (according to the main criteria classification of the NEMA).
}
\examples{
#
}
