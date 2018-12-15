#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions Relating to Precipitation Based on Raster Time Series.
#'
#' \code{evaluateDroughtNemaPrecipitation} evaluates based on precipitation values
#' for a target time period and representing long term mean values as
#' raster based time series (\code{RasterBrick} or \code{RasterStack} object, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the classification of the NEMA). This function
#' considers only criteria related to precipitation.
#'
#' @param precipitation A \code{RasterBrick} or \code{RasterStack} object with
#' (mean) total precipitation values for each time interval within the target time
#' period. See the details section. Time intervals are supposed to represent fixed
#' ten-day intervals.
#' @param ltmprecipitation A \code{RasterBrick} or \code{RasterStack} object with
#' long-term mean total precipitation values for fixed ten-day intervals within a
#' year. See the details section.
#' @param precipitation_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{precipitation} as returned by \code{\link{weatherMean}}
#' (each element denoting the first day of the respective ten-day interval).
#' @param ltmprecipitation_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{ltmprecipitation} as returned by \code{\link{intervalMean}}
#' (each element denoting the first day of the respective ten-day interval).
#' @details The start and end time point of the target time interval are derived from
#' \code{timedate_daily}. All time intervals of \code{timedate_aggregated} are supposed
#' to fit perfectly within \code{timedate_daily} (i.e. there are no more or less days in
#' \code{timedate_daily}). All \code{Raster*} time series data are supposed to have the
#' same spatial extent and resolution.
#' @seealso
#' @examples #
#' @export
evaluateDroughtNemaPrecipitation <- function(precipitation, ltmprecipitation, precipitation_t,
                                             ltmprecipitation_t
){

  # check if precipitation and precipitation_t are of the same length
  if(nlayers(precipitation) != length(precipitation_t)){
    stop("The layers of precipitation have to correspond to the ten-day intervals denoted by precipitation_t\n")
  }

  # check if ltmprecipitation has 36 layers
  if(nlayers(ltmprecipitation) != 36){
    stop("ltmprecipitation must have 36 layers\n")
  }

  # check if ltmprecipitation_t has 36 elements
  if(length(ltmprecipitation_t) != 36){
    stop("ltmprecipitation_t must be of length 36\n")
  }

  # compute for each study period tdi where near drought and drought conditions existed according to the precipitation
  brickdroughtprecipitation <- foreach(studytdi = seq_along(precipitation_t), .combine = brick, .multicombine = TRUE, .export = c("precipitation", "ltmprecipitation")) %do% {

    # create a raster with the same spatial properties as precipitation and fill all values with NA
    rasterdroughtprecipitation <- precipitation[[1]]
    values(rasterdroughtprecipitation) <- NA

    # assign the current tdi to a long-term tdi
    index <- which(ltmprecipitation_t == strftime(precipitation_t[studytdi], format = "%m-%d"))

    # compute the current threshold for near-drought conditions
    ndroughtprecipitation_threshold <- calc(ltmprecipitation[[index]], function(x) x * 0.8)

    # compute the current threshold for drought conditions
    droughtprecipitation_threshold <- calc(ltmprecipitation[[index]], function(x) x * 0.5)

    # evaluate the thresholds for the current tdi
    ndroughtprecipitation_threshold_index <- which(values(precipitation[[studytdi]]) <= values(ndroughtprecipitation_threshold))
    droughtprecipitation_threshold_index <- which(values(precipitation[[studytdi]]) <= values(droughtprecipitation_threshold))

    # write the results to rasterdroughtprecipitation
    rasterdroughtprecipitation[ndroughtprecipitation_threshold_index] <- 1
    rasterdroughtprecipitation[droughtprecipitation_threshold_index] <- 2

    # return rasterdroughtprecipitation
    return(rasterdroughtprecipitation)

  }

  # return the raster along with the time information
  list(droughtprecipitation = brickdroughtprecipitation, time = precipitation_t)

}
