#'@importFrom Rdpack reprompt
#'@import rgdal
#'@import raster
NULL

#' Spatially aggregates raster time series within geometries.
#'
#' \code{dailyLongTermSD} aggregates raster based time series (\code{RasterBrick} or \code{RasterStack} object, see: \code{\link[raster]{Raster-class}}) by computing the long-term standard deviation values for a specified time period grouped by days. Leap years are not handled separately, i.e. for the 29th February, the computed values are the sd values of only the input values for 29th Februaries.
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with values on a sub-monthly resolution.
#' @param timedate A \code{POSIXct} vector containing the time information for each band/layer of \code{variable}. \code{variable} and \code{timedate} are assumed to be on daily or sub-daily resolution.
#' @param cores The number of cores to be used in parallel computing.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated raster time series with daily sd values as a \code{RasterBrick} and the second element a \code{POSIXct} vector containing the date information (i.e. the days).
#' @seealso
#' @examples #
#' @export
#'
dailyLongTermSD <- function (variable, timedate, cores = 1, clcall = NULL)
{
  if (!(inherits(timedate, "POSIXct"))) {
    stop("timedate must be of class POSIXct\n")
  }
  if (length(timedate) != nlayers(variable)) {
    stop("The number of elements in timedate does not match the number of layers in variable\n")
  }
  timedate <- strftime(timedate, format = "%m-%d")
  index <- tapply(seq_along(timedate), timedate, function(x) x)
  cl <- makeCluster(cores, outfile = "", type = "PSOCK")
  registerDoParallel(cl)
  if (!is.null(clcall)) {
    clusterCall(cl, clcall)
  }
  on.exit(stopCluster(cl))
  md <- brick(foreach(d = seq_along(index), .packages = c("raster")) %dopar%
  {
    calc(variable[[index[[d]]]], fun = sd)
  })
  list(dm = md, day = names(index))
}
