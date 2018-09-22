#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Aggregates daily raster time series.
#'
#' \code{weatherMean} aggregates raster based time series
#' (\code{RasterBrick} or \code{RasterStack} object, see:
#' \code{\link[raster]{Raster-class}}) by computing mean values
#' along the time series (i.e. no long-term mean values) as
#' monthly mean values, moving window ten-day interval mean
#' values as defined in \code{\link{tenDayLongTermMWMeans}} and
#' fixed ten-day interval mean values as defined in
#' \code{\link{intervalMean}}.
#'
#' @param variable A \code{RasterBrick} or \code{RasterStack} object with
#' values on a daily resolution.
#' @param resolution Character value indicating the temporal resolution of the mean values.
#' If \code{resolution = "fixedtendays"}, mean values will be computed for each
#' fixed ten-day interval as defined in \code{\link{intervalMean}}. If
#' \code{resolution = "mwtendays"}, mean values will be computed for each
#' moving window ten-day interval as defined  in
#' \code{\link{tenDayLongTermMWMeans}}. If \code{resolution = "months"}, mean
#' values will be computed for each month.
#' @param tstart Numeric value (four places) for the first year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 1984}.
#' @param tend Numeric value (four places) for the last year of the time period
#' to consider as long-term interval, e.g. \code{tstart = 2014}.
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
weatherMean <- function(variable, tstart = NULL, tend = NULL, resolution = "monthly", timedate, cores = 10, clcall = NULL){

  # function in order to classify on a ten-day interval resolution
  assignfixedtendayinterval <- function(timedate, timerange){

    # get the years covered
    years <- unique(strftime(timedate[timerange], format = "%Y"))

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

    # get the date of the first and last days of each ten day interval
    a <- c()
    b <- c()
    for(year_i in seq_along(days)){

      a <- c(a, tapply(strftime(days[[year_i]], "%Y-%m-%d"), tendayintervals[[year_i]], function(x) as.character(x[1])))
      b <- c(b, tapply(strftime(days[[year_i]], "%Y-%m-%d"), tendayintervals[[year_i]], function(x) as.character(x[length(x)])))

    }

    # return the result
    list(a, b)

  }

  # get the time interval specified by tstart and tend
  timedate_years <- as.numeric(as.character(strftime(timedate, format = "%Y")))
  if(!(is.null(tstart) || is.null(tend))){
    timerange <- which(timedate_years >= tstart & timedate_years <= tend)
  }else{
    timerange <- 1:length(timedate)
  }

  # prune the time interval if resolution = mwtendays
  if(resolution == "mwtendays"){
    timerange <- timerange[1:(length(timerange) - 9)]
  }

  # define indices based on resolution
  switch(resolution,
         monthly = {indices1 <- list(strftime(timedate[timerange], format = "%Y-%m"))
         indices <- indices1
         indices[[1]] <- tapply(as.character(timedate[timerange]), indices1[[1]], function(x) x[1])
         indices[[2]] <- tapply(as.character(timedate[timerange]), indices1[[1]], function(x) x[length(x)])
         },
         fixedtendays = {indices <- assignfixedtendayinterval(timedate, timerange)
         },
         mwtendays = {indices <- list(as.character(timedate[timerange]), as.character(strftime(timedate[timerange] + 9*24*60*60, "%Y-%m-%d")))
         }
  )

  # get numerical indices
  indices1 <- lapply(indices, function(x) which(as.character(timedate[timerange]) %in% x))

  # remove entries that do not fit within the timerange(if the last day of the last tdi(s) is (are) not within the specified data)
  if(length(indices1[[2]]) < length(indices1[[1]])){
    indices1[[1]] <- indices1[[1]][1:length(indices1[[2]])]
  }

  # prune indices (will be exported as time information)
  index1 <- which(indices[[1]] %in% as.character(timedate[timerange]))[1]
  index2 <- rev(which(indices[[2]] %in% as.character(timedate[timerange])))[1]
  indices <- lapply(indices, function(x){
    x[index1:index2]
  })

  # remove entries that do not fit within the timerange(if the last day of the last tdi(s) is (are) not within the specified data)
  indices <- lapply(seq_along(indices), function(x){
    if(length(indices[[x]]) > length(indices1[[x]])){
      indices[[x]][1:length(indices1[[x]])]
    }else{
      indices[[x]]
    }
  })

  # prune variable to timerange
  variable <- variable[[timerange]]

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  # compute mean values
  meanvariable <-
    foreach(i = seq_along(indices1[[1]]), .export = c("variable", "indices1"), .combine = stack, .packages = "raster") %dopar% {

      calc(variable[[indices1[[1]][i]:indices1[[2]][i]]], fun = mean)

    }

  # convert to RasterBrick
  meanvariable <- brick(meanvariable)

  # stop cluster
  stopCluster(cl)

  # return result
  return(list(variable = meanvariable, time = indices[[1]]))

}
# monthly resolution seems to work
# corrected the function for fixed tdi, works now
# mwtendays seems to work
# it has to be tested if the function also works if timedate does not begin at the start of a year
