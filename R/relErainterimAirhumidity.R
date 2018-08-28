#'@importFrom Rdpack reprompt
NULL

#' Computes the relative air humidity for raster based time series.
#'
#' \code{relErainterimAirhumidity} calculates the relative air humidity [%] of raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) derived from netcdf raster files as downloaded from
#' ERA interim as suggested by
#' \url{https://software.ecmwf.int/wiki/display/CKB/Do+ERA+datasets+contain+parameters+for+near-surface+humidity},
#' based on the dew point temperature and the air temperature. This is done using thw Teten's
#' formula for saturation over water as suggested at
#' \url{https://software.ecmwf.int/wiki/display/CKB/Do+ERA+datasets+contain+parameters+for+near-surface+humidity} and in
#' \url{https://www.ecmwf.int/sites/default/files/elibrary/2016/16648-part-iv-physical-processes.pdf}.
#'
#' @param dt A \code{RasterBrick} or \code{RasterStack} object with values
#' for the dew point temperature. \code{dt} is supposed to have the same temporal extent and
#' resolution as \code{t}.
#' @param t A \code{RasterBrick} or \code{RasterStack} object with values for the air
#' temperature. \code{t} is supposed to have the same temporal extent and resolution
#' as \code{dt}.
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
relErainterimAirhumidity <- function(dt, t, interval = NULL, cores = 10, timedate, clcall = NULL){

  # load packages
  require("foreach")
  require("doParallel")
  require("parallel")

  # define function for Teten's formula
  tetens <- function(temp, temp0 = 273.16, a1 = 611.21, a3 = 17.502, a4 = 32.19){
    esat <- a1 * exp(a3*((temp - temp0)/(temp - a4)))
    esat
  }

  # extract time information from dt and t
  z <- timedate

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate rh
  rh <-
    foreach(step_i = unique(z), .packages = c("raster"), .combine = stack) %dopar%{

      esat_t <- calc(t[[which(z == step_i)]], fun = function(x){tetens(x)})
      esat_dt <- calc(dt[[which(z == step_i)]], fun = function(x){tetens(x)})

      rh1 <- 100*esat_dt/esat_t

    }

  # convert to RasterBrick object
  rh <- brick(rh)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = rh, day = z))
}
