#'@importFrom Rdpack reprompt
NULL

#' Calculates cumulative precipitation raster time series.
#'
#' \code{dailySums} computes total daily sums of a raster based time series
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
dailySums <- function(variable, cores = 10, timedate, clcall = NULL){

  # extract time information from timedate
  z <- strftime(timedate, format = "%Y-%m-%d")

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # define an indexing parameter over which to iterate (100 days per iteration)
  indices1 <- as.integer(as.factor(z))
  indices <- do.call(rbind, tapply(seq_along(indices1), indices1, function(x)x))
  steps <- seq(1, nrow(indices), 50)
  steps[length(steps)] <- nrow(indices)

  # calculate precipitation sum
  dailysum <-
    foreach(step_i = seq_along(steps)[-length(steps)], .packages = c("raster"), .combine = stack, .multicombine = TRUE, .export = c("z", "steps", "indices", "indices1")) %dopar%{

      if(step_i == rev(seq_along(steps)[-length(steps)])[1]){
        currentindex <- indices[steps[step_i], 1]:indices[steps[step_i+1], 2]
      }else{
        currentindex <- indices[steps[step_i], 1]:indices[steps[step_i+1]-1, 2]
      }

      stackApply(x = variable[[currentindex]],
                 indices = indices1[currentindex],
                 fun = sum)

    }

  # convert to RasterBrick object
  dailysum <- brick(dailysum)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = dailysum, day = unique(z)))
}
