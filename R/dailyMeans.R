#'@importFrom Rdpack reprompt
NULL

#' Aggregates sub-daily raster time series to daily means.
#'
#' \code{dailyMeans} aggregates raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) on a sub-daily basis
#' by computing the respective daily mean values for each
#' raster cell.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with values on a sub-daily
#' resolution.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated
#' raster time series with daily mean values as a \code{RasterBrick} and
#' the second element a \code{POSIXct} vector containing the date information
#' (i.e. the days).
#' @seealso
#' @examples #
#' @export
dailyMeans <- function(variable, cores = 10, timedate, clcall = NULL){

  # extract time information from variable
  z <- timedate
  z <- strftime(z, format = "%Y-%m-%d")

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate the daily mean of variable
  dmean <-
    foreach(step_i = unique(z), .packages = c("raster"), .combine = stack) %dopar%{

      dmean1 <- calc(variable[[which(z == step_i)]], fun = mean)

    }

  # convert to RasterBrick object
  dmean <- brick(dmean)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = dmean, day = unique(z)))
}
