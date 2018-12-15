#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions (Additional Criteria) Relating to Temperature Based on Raster Time Series.
#'
#' \code{evaluateDroughtNemaTemepratureAdditional} evaluates based on air temeprature
#' values for a target time period as
#' raster based time series (\code{RasterBrick} or \code{RasterStack} object, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the classification of the NEMA).
#' This function considers only additional criteria related to temerature. See
#' \code{\link{evaluateDroughtNemaTemepratureMain}} for temperature related
#' threshold values that represent main criteria.
#'
#' @param temperature A \code{RasterBrick} or \code{RasterStack} object with
#' (mean) temperature values on a daily basis within the target time
#' period.
#' @param temperature_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{airhumidity} as returned by \code{\link{dailyMeans}}
#' (each element denoting the respective day).
#' @param landcover A \code{RasterBrick} object containing the following layers
#' in the following sequence
#' \describe{
#'   \item{\code{"alpine"}}{}
#'   \item{\code{"steppe"}}{}
#'   \item{\code{"forest_steppe"}}{}
#'   \item{\code{"desert"}}{}
#'   \item{\code{"desert_steppe"}}{}
#'   \item{\code{"lakes"}}{}
#'   \item{\code{"taiga"}}{}
#' }
#' Each raster layer has a value of \code{1} if the respective landcover class exists
#' for a pixel and \code{NA} if not.
#' @return A list with two elements:
#' \describe{
#'   \item{\code{droughttemperatureadditional}}{A \code{RasterBrick} object containing for each
#'   fixed ten-day interval in \code{temperature_t} a layer classifying if there were
#'   drought conditions according to the relative air humidity or not.}
#'   \item{\code{time}}{A character vector specifying the date of the first day of the
#'   respective fixed ten-day interval.}
#' }
#' @seealso
#' @examples #
#' @export
evaluateDroughtNemaTemepratureAdditional <- function(temperature, temperature_t, landcover
){

  # check if temperature and temperature_t are of the same length
  if(nlayers(temperature) != length(temperature_t)){
    stop("The layers of temperature have to correspond to the ten-day intervals denoted by temperature_t\n")
  }

  # assign each day of temperature_t to a fixed ten-day interval
  tdi <- assignfixedtendayinterval(temperature_t, timerange = 1:length(temperature_t))
  tdi <- lapply(tdi, function(x) which(as.character(temperature_t) %in% x))
  tdi <- do.call("c", lapply(seq_along(tdi[[2]]), function(x){
    currenttdi <- tdi[[1]][x]:tdi[[2]][x]
    names(currenttdi) <- rep(x, length(currenttdi))
    return(currenttdi)
  }))

  # define temeprature thresholds for each landcover class
  droughttemperaturethreshold <- c(alpine = 25, steppe = 30, forest_steppe = 30, desert = 32, desert_steppe = 32, lakes = NA, taiga = NA) + 273.15

  # create a raster layer with these temeprature thresholds
  landcoverthresholdstemperature <- landcover[[1]]
  values(landcoverthresholdstemperature) <- NA
  for(i in seq_along(droughttemperaturethreshold)){
    if(!is.na(droughttemperaturethreshold[i])){
      landcoverthresholdstemperature[which(values(landcover[[i]]) == 1)] <- droughttemperaturethreshold[i]
    }
  }

  # compute for each study period tdi where near drought and drought conditions existed according to the temeprature (additional criteria)
  brickdroughttemperature <- foreach(studytdi = unique(names(tdi)), .combine = brick, .multicombine = TRUE, .export = c("temperature", "tdi", "landcoverthresholdstemperature")) %do% {

    # create a raster with the same spatial properties as temperature and fill all values with NA
    rasterdroughttemperature <- temperature[[1]]
    values(rasterdroughttemperature) <- NA

    # assign the respective days to the current tdi
    index <- tdi[names(tdi) == studytdi]

    # count the number of days with a relative air humidity < 30 %
    a <- over(x = temperature[[index]], y = landcoverthresholdstemperature, fun = function(x, y){
      x > y
    })

    nodaystemperaturebelowthreshold <- calc(temperature[[index]], function(x){
      length(which(x < 30))
    })

    # evaluate the thresholds for the current tdi
    ndroughttemperature_threshold_index <- which(values(nodaystemperaturebelowthreshold) > 5)

    # write the results to rasterdroughttemperature
    rasterdroughttemperature[ndroughttemperature_threshold_index] <- 1

    # return rasterdroughttemperature
    return(rasterdroughttemperature)

  }

  # return the raster along with the time information
  list(droughttemperature = brickdroughttemperature, time = tapply(tdi, names(tdi), function(x) temperature_t[x[1]]))


}
