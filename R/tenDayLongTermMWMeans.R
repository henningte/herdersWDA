#'@importFrom Rdpack reprompt
NULL

#' Computes long-term ten-day moving window means of raster time series.
#'
#' \code{tenDayLongTermMWMeans} aggregates raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) by computing the long-term
#' mean values for a specified time period grouped by ten-day intervals.
#' Ten-day intervals are constructed for each day of a year by sliding a
#' moving window with a length of 10 days along the sequence of days of a
#' year. For leap years, the corresponding ten day interval containing the
#' 29 th February. In all other years, the interval reaches one day more
#' in the future and the ten day interval for the 29 th February is the
#' same as for the 28 th february.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a daily resolution.
#' @param tstart Numeric value (four places) for the first year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 1984}.
#' @param tend Numeric value (four places) for the last year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 2014}.
#' @param cores The number of cores to be used in parallel computing.
#' @param timedate A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{variable}.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing the aggregated
#' raster time series with moving window ten-day mean values as a \code{RasterBrick} and
#' the second element a \code{POSIXct} vector containing the date information
#' (i.e. the days representing the start of each moving window interval).
#' @seealso
#' @examples #
#' @export
tenDayLongTermMWMeans <- function(variable, tstart, tend, cores = 10, timedate, clcall = NULL){

  # extract time information from variable
  z <- timedate

  # define vectors for different time levels
  z.year <- strftime(z, format = "%Y")
  z.day <- strftime(z, format = "%m-%d")
  z.day1 <- strftime(z, format = "%m-%d")
  z <- strftime(z, format = "%Y-%m")

  # sort z.day
  z.day <- unique(names(sapply(z.day, function(x){as.Date(x, format = "%m-%d")})))
  z.day <- z.day[c(1:59, length(z.day), 60:(length(z.day)-1))]

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate ten day means
  mean.tds <-
    foreach(step_i = z.day, .packages = c("raster"), .combine = stack) %dopar%{

      # get index to subset rasters
      subset.index <- which(z.day1 == step_i)
      if(step_i == "02-29"){
        subset.index <- c(which(z.day1 == "02-28"))
      }
      for(int_day_i in c(1:9)){
        subset.index <- unique(c(subset.index, subset.index+1))
      }

      mean.td <- calc(variable[[subset.index[which(subset.index %in% which(z.year >= tstart & z.year <= tend))]]], fun = mean)

    }

  # convert mean.tds to a RasterBrick
  mean.tds <- brick(mean.tds)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(mean.tds, day = z.day))
}
