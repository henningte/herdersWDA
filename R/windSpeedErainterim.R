#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Computes total wind speed for raster based time series.
#'
#' \code{windSpeedErainterim} calculates the total wind speed [m/s] of raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) derived from netcdf raster files as downloaded from
#' ERA interim. This is done by computing the length of the total vector from the north-south
#' and east-west components as suggested by
#' \url{https://confluence.ecmwf.int/pages/viewpage.action?pageId=77229867}.
#'
#' @param u10 A \code{RasterBrick} or \code{RasterStack} object with values
#' for the wind speed at 10 m height in east direction (negative signs indicate
#' a wind direction to the west). \code{u10} is
#' supposed to have the same temporal extent and resolution as \code{v10}.
#' @param v10 A \code{RasterBrick} or \code{RasterStack} object with values
#' for the wind speed at 10 m height in north direction (negative signs indicate
#' a wind direction to the south). \code{v10} is
#' supposed to have the same temporal extent and resolution as \code{u10}.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the relative air humidity [%]
#' time series with as a \code{RasterBrick} with the same temporal resultution as
#' \code{dt} and \code{t} and
#' the second element a \code{POSIXct} vector containing the time information.
#' @seealso
#' @examples #
#' @export
windSpeedErainterim <- function(u10, v10, cores = 10, timedate, clcall = NULL){

  # extract time information from timedate
  z <- strftime(timedate, format = "%Y-%m-%d")

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # define an indexing parameter over which to iterate (100 days per iteration)
  steps <- seq(1, nlayers(variable), 100)
  steps[length(steps)] <- nlayers(variable)

  # calculate the wind speed
  windspeed <-
    foreach(step_i = seq_along(steps), .packages = c("raster"), .combine = stack, .multicombine = TRUE, .export = c("z", "steps")) %dopar%{

      windspeedstep <- do.call(stack, lapply(steps[step_i]:(steps[step_i+1]-1), function(x){

        overlay(x = u10, y = v10, fun = function(x, y){
          sqrt(x^2+y^2)
        })

      }))

    }

  # convert to RasterBrick object
  windspeed <- brick(windspeed)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = windspeed, day = unique(z)))

} # not tested yet!
