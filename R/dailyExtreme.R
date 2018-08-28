#'@importFrom Rdpack reprompt
NULL

#' Extracts extreme values from sub-daily raster time series.
#'
#' \code{dailyExtreme} extracts extreme values from raster
#' based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) on a sub-daily basis.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with values
#' on a sub-daily resolution.
#' @param min A logical value indicating if daily minima (\code{min = TRUE}) or
#' daily maxima (\code{min = FALSE}) should be extracted.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the extracted extreme values of the
#' raster time series as a \code{RasterBrick} and
#' the second element a \code{POSIXct} vector containing the date information
#' (i.e. the days).
#' @seealso
#' @examples #
#' @export
dailyExtreme <- function(variable, min = T, cores = 10, timedate, clcall = NULL){

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  # clusterEvalQ(cl, .libPaths()) # side effects and functionality have to be checked
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # get days of variable
  days <- unique(strftime(timedate, "%Y-%m-%d"))

  # get raster stack with daily extreme values
  extreme.stack <-
    foreach(day_i = c(1:length(unique(strftime(timedate, "%Y-%m-%d")))), .combine = stack, .packages = "raster") %dopar% {

      if(min == T){

        # get raster with minima for the current day
        r.ext <- min(variable[[which(strftime(timedate, "%Y-%m-%d") == days[day_i])]])

      }else if(min == F){

        # get raster with minima for the current day
        r.ext <- max(variable[[which(strftime(timedate, "%Y-%m-%d") == days[day_i])]])

      }

      r.ext@data@names <- days[day_i]
      return(r.ext)

    }

  # convert to RasterBrick
  extreme.stack <- brick(extreme.stack)

  # stop cluster
  stopCluster(cl)

  return(list(variable = extreme.stack, day = days))

}
