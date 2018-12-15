#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions (Main Criteria) Relating to Temperature Based on Raster Time Series.
#'
#' \code{evaluateDroughtNemaTemepratureMain} evaluates based on temperature values
#' for a target time period and representing long term mean values as
#' raster based time series (\code{RasterBrick} or \code{RasterStack} object, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the main criteria classification of the NEMA).
#' This function considers only main criteria related to temerature. See
#' \code{\link{evaluateDroughtNemaTemepratureAdditional}} for temperature related
#' threshold values that represent additional criteria.
#'
#' @param temperature A \code{RasterBrick} or \code{RasterStack} object with
#' (mean) temperature values for each time interval within the target time
#' period. See the details section. Time intervals are supposed to represent fixed
#' ten-day intervals.
#' @param ltmtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' long-term mean temperature values for fixed ten-day intervals within a
#' year. See the details section.
#' @param ltsdtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' standard deviation values of mean air temperature values within the time interval
#' considered as long-term time interval. \code{ltsdairtemperature} must have the
#' same number of layers as \code{ltmtemperature}.
#' @param temeprature_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{temperature} as returned by \code{\link{weatherMean}}
#' (each element denoting the first day of the respective ten-day interval).
#' @param ltmtemperature_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{ltmtemperature} as returned by \code{\link{intervalMean}}
#' (each element denoting the first day of the respective ten-day interval).
#' @details The start and end time point of the target time interval are derived from
#' \code{timedate_daily}. All time intervals of \code{timedate_aggregated} are supposed
#' to fit perfectly within \code{timedate_daily} (i.e. there are no more or less days in
#' \code{timedate_daily}). All \code{Raster*} time series data are supposed to have the
#' same spatial extent and resolution.
#' @seealso
#' @examples #
#' @export
evaluateDroughtNemaTemepratureMain <- function(temperature, ltmtemperature, ltsdtemperature, temperature_t,
                                               ltmtemperature_t
){

  # check if temperature and temperature_t are of the same length
  if(nlayers(temperature) != length(temperature_t)){
    stop("The layers of temperature have to correspond to the ten-day intervals denoted by temperature_t\n")
  }

  # check if ltmtemperature has 36 layers
  if(nlayers(ltmtemperature) != 36){
    stop("ltmtemperature must have 36 layers\n")
  }

  # check if ltsdtemperature has 36 layers
  if(nlayers(ltsdtemperature) != 36){
    stop("ltsdtemperature must have 36 layers\n")
  }

  # check if ltmtemperature_t has 36 elements
  if(length(ltmtemperature_t) != 36){
    stop("ltmtemperature_t must be of length 36\n")
  }

  # compute for each study period tdi where near drought and drought conditions existed according to the temperature
  brickdroughttemperature <- foreach(studytdi = seq_along(temperature_t), .combine = brick, .multicombine = TRUE, .export = c("temperature", "ltmtemperature", "ltsdtemperature")) %do% {

    # create a raster with the same spatial properties as temperature and fill all values with NA
    rasterdroughttemperature <- temperature[[1]]
    values(rasterdroughttemperature) <- NA

    # assign the current tdi to a long-term tdi
    index <- which(ltmtemperature_t == strftime(temperature_t[studytdi], format = "%m-%d"))

    # compute the variance of the long-term mean temperature value along the time vector
    ltvartemperature <- calc(ltsdtemperature[[index]], fun = function(x) x^2)

    # compute the current thresholds for near-drought conditions
    ndroughttemperature_threshold1 <- calc(ltmtemperature[[index]], function(x) x + 1)
    ndroughttemperature_threshold2 <- overlay(x = ltmtemperature[[index]], y = ltvartemperature, fun = function(x, y){
      x + y
    })

    # compute the current threshold for drought conditions
    droughttemperature_threshold1 <- calc(ltmtemperature[[index]], function(x) x + 2)
    droughttemperature_threshold2 <- overlay(x = ltmtemperature[[index]], y = ltvartemperature, fun = function(x, y){
      x + 2*y
    })

    # evaluate the thresholds for the current tdi
    ndroughttemperature_threshold_index <- which(values(temperature[[studytdi]]) >= values(ndroughttemperature_threshold1) | values(temperature[[studytdi]]) > values(ndroughttemperature_threshold2))
    droughttemperature_threshold_index <- which(values(temperature[[studytdi]]) >= values(droughttemperature_threshold1) | values(temperature[[studytdi]]) > values(droughttemperature_threshold2))

    # write the results to rasterdroughttemperature
    rasterdroughttemperature[ndroughttemperature_threshold_index] <- 1
    rasterdroughttemperature[droughttemperature_threshold_index] <- 2

    # return rasterdroughttemperature
    return(rasterdroughttemperature)

  }

  # return the raster along with the time information
  list(droughttemperature = brickdroughttemperature, time = temperature_t)

}
