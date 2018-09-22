#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Computes long-term monthly means of raster time series.
#'
#' \code{monthlyLongTermMeans} aggregates raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) by computing the long-term
#' mean values for a specified time period grouped by months.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a sub-monthly resolution.
#' @param tstart Numeric value (four places) for the first year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 1984}.
#' @param tend Numeric value (four places) for the last year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 2014}.
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
monthlyLongTermMeans <- function(variable, tstart, tend, cores = 10, timedate, clcall = NULL){

  # extract time information from variable
  z <- timedate

  # define vectors for different time levels
  z.year <- strftime(z, format = "%Y")
  z.month <- strftime(z, format = "%m")
  z <- strftime(z, format = "%Y-%m")

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate monthly means
  mean.months <-
    foreach(step_i = unique(z.month), .packages = c("raster"), .combine = stack) %dopar%{

      mean.month <- calc(variable[[which(z.month == step_i & z.year >= tstart & z.year <= tend)]], fun = mean)

    }

  # convert to RasterBrick object
  mean.months <- brick(mean.months)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = mean.months, month = unique(z.month)))
}
