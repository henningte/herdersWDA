#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Computes long-term fixed ten-day interval means of raster time series.
#'
#' \code{intervalMean} aggregates raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) by computing the long-term
#' mean values for a specified time period grouped by ten-day intervals.
#' For each month of a year, three fixed ten-day interval is constructed
#' with the first reaching from the first to the 10th day, the second from
#' the 11th to the 20th and the third from the 21 to the 28th/29th/30th/31th
#' day of the month, depending on its length.
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
#' raster time series with fixed ten-day interval mean values as a \code{RasterBrick} and
#' the second element a \code{POSIXct} vector containing information on the sequence of
#' ten-day intervals (i.e. an integer value from 1 to 36).
#' @seealso
#' @examples #
#' @export
intervalMean <- function(variable, tstart, tend, timedate, cores, clcall = NULL
){

  # function in order to classify on a ten-day interval resolution
  assigntendayinterval <- function(timedate){

    # get the years covered
    years <- unique(strftime(timedate, format = "%Y"))

    # get the number of days in each year
    days <- list()
    for(year_i in seq_along(years)){
      days[[year_i]] <- seq(as.POSIXct(paste0(years[year_i], "-01-01"), tz = attr(timedate, "tzone")), as.POSIXct(paste0(years[year_i], "-12-31"), tz = attr(timedate, "tzone")), "day")
    }

    # create a list with indices for days
    tendayintervals <- lapply(seq_along(days), function(y){

      # extract months
      months <- strftime(days[[y]], format = "%y-%m")

      # define an additional offset value if data for several years exist there
      offset <- (y - 1) * 36

      # get a vector with ten day-intervals
      tdi <- do.call(c, as.vector(sapply(seq_along(table(months)), function(x){

        # get indices of ten day-intervals
        if(which(names(table(months)) == names(table(months)[x])) == 1){
          indices <- 1:3
        }else{
          indices <- seq((which(names(table(months)) == names(table(months)[x]))-1) * 3 + 1, (which(names(table(months)) == names(table(months)[x]))-1) * 3 + 3)
        }

        c(rep(indices[1], 10), rep(indices[2], 10), rep(indices[3], table(months)[x] - 20))

      }))) + offset

    })

    # assign each day to a ten day-interval
    tendayintervals <-
      do.call(c, sapply(seq_along(days), function(x){
        names(tendayintervals[[x]]) <- days[[x]]
        return(tendayintervals[[x]])
      }))

    # get unique ten day-intervals for each data value in track
    timeinterval <- rep(0, length(timedate))
    days <- strftime(timedate, format = "%Y-%m-%d")
    iter <- 1
    iter2 <- 0
    for(day_i in unique(days)){
      timeinterval[which(days == day_i)] <- tendayintervals[which(names(tendayintervals) == day_i)] - iter2*36

      # reset iter after one year
      if((tendayintervals[which(names(tendayintervals) == day_i)] %% 36 == 0) && strftime(day_i, format = "%m-%d") == "12-31"){
        iter <- 1
        iter2 <- iter2 + 1
      }
    }

    return(timeinterval)

  }

  # get the number of years in timedate
  numberofyears <- length(unique(strftime(timedate, "%Y")))

  # get indices grouping days in fixed ten-day intervals
  tdi <- assigntendayinterval(timedate)

  # remove all values from tdi and variable for years not > tstart and < tend
  index <- which(strftime(timedate, "%Y") >= tstart & strftime(timedate, "%Y") <= tend)
  tdi <- tdi[index]
  variable <- variable[[index]]

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # calculate ten day means
  mean.tds <-
    foreach(step_i = unique(tdi), .packages = c("raster"), .combine = stack) %dopar%{

      mean.td <- calc(variable[[which(tdi == step_i)]], fun = mean)

    }

  # convert mean.tds to a RasterBrick
  mean.tds <- brick(mean.tds)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(mean.tds, interval = unique(tdi)))

}
