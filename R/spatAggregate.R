#'@importFrom Rdpack reprompt
#'@import rgdal
#'@import raster
NULL

#' Spatially aggregates raster time series within geometries.
#'
#' \code{spatAggregate} performs aggregation of raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) within certain spatial entity (polygon geometry)
#'  applying \code{over} from package \code{sp} on all raster layers (time instances)
#'  within a given time interval.
#' @param variable A \code{RasterBrick} or \code{RasterStack} object.
#' @param spatialentity A \code{SpatialPolygonsDataFrame} of the  spatial entity to
#' iaggregate within..
#' @param tstart Start of the time interval during which to perform aggregation
#' given as character values in the format "yyyy-mm-dd".
#' @param tend End of the time interval during which to perform aggregation given
#' as character values in the format "yyyy-mm-dd".
#' @param fn Function to be used for aggregation. One of \code{mean}, \code{min}
#' or \code{max}.
#' @return A data frame containing the aggregated vaules of each raster layer (time instance)
#' within each spatial entity
#' @seealso
#' @examples #
#' @export
spatAggregate <- function(variable, spatialentity, tstart, tend, fn = "mean"){
  #extract only date characters in case time is also provided
  tstart = as.POSIXct(substr(tstart, 1, 10))
  tend = as.POSIXct(substr(tend, 1, 10))
  layerlist <- variable@z[[1]][variable@z[[1]] >= tstart & variable@z[[1]] <= tend]

  if(fn == "mean"){
    agg_per_areas <- as.data.frame(lapply(layerlist, function(x){
      m <- variable[[which(variable@z[[1]] == x)]]
      over(spatialentity, as(m,"SpatialPixelsDataFrame"), fn = mean)[[1]]
    }))
    names(agg_per_areas) <- paste("mean",substr(layerlist, start = 1, stop = 10), sep="_")
  } else if(fn == "min"){
    agg_per_areas <- as.data.frame(lapply(layerlist, function(x){
      m <- variable[[which(variable@z[[1]] == x)]]
      over(spatialentity, as(m,"SpatialPixelsDataFrame"), fn = min)[[1]]
    }))
    names(agg_per_areas) <- paste("min",substr(layerlist, start = 1, stop = 10), sep="_")
  } else if(fn == "max"){
    agg_per_areas <- as.data.frame(lapply(layerlist, function(x){
      m <- variable[[which(variable@z[[1]] == x)]]
      over(spatialentity, as(m,"SpatialPixelsDataFrame"), fn = max)[[1]]
    }))
    names(agg_per_areas) <- paste("max",substr(layerlist, start = 1, stop = 10), sep="_")
  }

  return(agg_per_areas)
  #optionally the geometries could be returned with the aggregated vaules added to the data frame. Curently not used, see below:
  #spatialentity@data <- cbind(spatialentity@data,means_per_areas)
  #return(spatialentity)
}
