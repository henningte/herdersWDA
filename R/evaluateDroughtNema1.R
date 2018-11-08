#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions (All Criteria) Based on Raster Time Series.
#'
#' \code{evaluateDroughtNema1} evaluates
#' for a target time period, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the main criteria classification of the NEMA).
#'
#' @param droughttemperaturemain A \code{RasterBrick} object a returned by
#' \code{\link{evaluateDroughtNemaTemperatureMain}} containing the classification
#' if the temepratures of respective ten-day intervals for a pixel result in
#' drought conditions or near-drought conditions (according to the meain criteria).
#' @param droughttemperatureadditional A \code{RasterBrick} object a returned by
#' \code{\link{evaluateDroughtNemaTemperatureAdditional}} containing the classification
#' if the temepratures of respective ten-day intervals for a pixel result in
#' drought conditions (according to the additional criteria).
#' @param droughtprecipitation A \code{RasterBrick} object a returned by
#' \code{\link{evaluateDroughtNemaPrecipitation}} containing the classification
#' if the precipitation of respective ten-day intervals for a pixel result in
#' drought conditions or near-drought conditions.
#' @param droughtairhumidity A \code{RasterBrick} object a returned by
#' \code{\link{evaluateDroughtNemaAirhumidity}} containing the classification
#' if the relative air humidity of respective ten-day intervals for a pixel result in
#' drought conditions.
#' @param timedate A \code{POSIXct} vector containing the time information
#' for all layers of the input raster objects.
#' @details
#' @return A list with two elements:
#' \describe{
#'   \item{\code{drought}}{A \code{RasterBrick} object containing for each
#'   fixed ten-day interval  a layer classifying if there were
#'   drought conditions or near-drought conditions according to all criteria or not.}
#'   \item{\code{time}}{A character vector specifying the date of the first day of the
#'   respective fixed ten-day interval.}
#' }
#' @seealso
#' @examples #
#' @export
evaluateDroughtNema1 <- function(droughttemperaturemain,
                                 droughttemperatureadditional,
                                 droughtprecipitation,
                                 droughtairhumidity,
                                 timedate
){

  # check if all raster objects have the same number of layers
  if(length(unique(c(nlayers(droughttemperaturemain), nlayers(droughttemperatureadditional), nlayers(droughtprecipitation), nlayers(droughtairhumidity)))) != 1){
    stop("All raster objects must have the same number of layers\n")
  }

  # check if timedate has as many elements as droughttemperaturemain layers
  if(length(timedate) != nlayers(droughttemperaturemain)){
    stop("length(timedate) != nlayers(droughttemperaturemain)\n")
  }

  # compute for each study period tdi where near drought and drought conditions existed according to the temperature
  brickdrought <- foreach(studytdi = seq_along(timedate), .combine = brick, .multicombine = TRUE, .export = c("droughttemperaturemain", "droughttemperatureadditional", "droughtprecipitation", "droughtairhumidity", "timedate")) %dopar% {

    # create a raster with the same spatial properties as droughttemperaturemain and fill all values with NA
    rasterdrought <- droughttemperaturemain[[1]]
    values(rasterdrought) <- NA

    # get an index for near dought pixels
    ndindex <- which(values(droughttemperaturemain[[studytdi]]) == 1 & values(droughtprecipitation[[studytdi]]) == 1)

    # get an index for dought pixels
    dindex <- which(values(droughttemperaturemain[[studytdi]]) == 2 & values(droughtprecipitation[[studytdi]]) == 2 | values(droughttemperatureadditional[[studytdi]]) == 1 | values(droughtairhumidity[[studytdi]]) == 1)

    # write the results to rasterdrought
    rasterdrought[ndindex] <- 1
    rasterdrought[dindex] <- 2

    # return rasterdrought
    return(rasterdrought)

  }

  # return the raster along with the time information
  list(drought = brickdrought, time = timedate)

}
