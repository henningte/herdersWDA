#'@importFrom Rdpack reprompt
NULL

#' Calculates cumulative precipitation raster time series.
#'
#' \code{totErainterimPrecipitation} computes the total precipitation
#' sum of a raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) on a sub-daily basis by computing
#' the sum of each raster cell for a specified temporal resolution.
#' In principle, this function can be used in order to compute any
#' aggregate value that is based on sums.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a sub-daily resolution.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated
#' raster time series with daily summed values as a \code{RasterBrick} and
#' the second element a \code{POSIXct} vector containing the date information
#' (i.e. the days).
#' @seealso
#' @examples #
#' @export
totErainterimPrecipitation <- function(precipitation, cores = 10, timedate, clcall = NULL){

  # extract time information from timedate
  z <- strftime(timedate, format = "%Y-%m-%d")

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate precipitation sum
  precipitation.sum <-
    foreach(step_i = unique(z), .packages = c("raster"), .combine = stack) %dopar%{

      if(length(which(z == step_i) > 1)){
        precipitation.sum.step <- sum(precipitation[[which(z == step_i)]])
      }else{
        precipitation.sum.step <- precipitation[[which(z == step_i)]]
      }


    }

  # convert to RasterBrick object
  precipitation.sum <- brick(precipitation.sum)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = precipitation.sum, day = z))
}
