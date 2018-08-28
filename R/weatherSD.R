#'@importFrom Rdpack reprompt
NULL

#' Computes the standard deviation of raster time series.
#'
#' \code{weatherSD} computes the standard deviation of raster
#' based time series (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) for the respective temporal
#' resolution.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a daily resolution.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated
#' raster time series with mean values as specified by \code{resolution} as
#' a \code{RasterBrick} and the second element a \code{POSIXct} vector containing
#' information on the aggregated time intervals as specified by \code{resolution}.
#' For fixed ten day intervals, this is an integer vector with increasing values for
#' each ten-day interval.
#' @seealso
#' @examples #
#' @export
weatherSD <- function(variable, tstart = NULL, tend = NULL, resolution = "monthly", timedate, cores = 10, clcall = NULL){

  # duplicate entries for 02-28 for non-leap years if resolution = mwtendays
  if(resolution == "mwtendays"){

    # get number of days for each year
    daysperyear <- list(unique(strftime(timedate, format = "%Y")), tapply(timedate, strftime(timedate, format = "%Y"), length))

    timedate1 <- NULL
    index <- 1
    for(i in seq_along(daysperyear[[1]])){

      if(daysperyear[[2]][i] == 365){
        timedate1 <- c(timedate1, timedate[c(index:(index + 58), index + 58, (index + 59):(index + 365))])
        index <- index + 365
      }else{
        timedate1 <- c(timedate1, timedate[index, index + 366])
        index <- index + 366
      }

    }

  }

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # compute mean values
  meanvariable <-
    foreach(i = seq_along(indices1), .export = c("variable", "indices1"), .combine = stack, .packages = "raster") %dopar% {

      calc(variable[[indices1[[i]][1]:indices1[[i]][2]]], fun = mean)

    }

  # convert to RasterBrick
  meanvariable <- brick(meanvariable)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(meanvariable, indices[[1]]))

}
