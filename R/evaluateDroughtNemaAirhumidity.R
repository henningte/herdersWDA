#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions Relating to Air Humidity Based on Raster Time Series.
#'
#' \code{evaluateDroughtNemaAirhumidity} evaluates based on relative air humidity
#' values for a target time period as
#' raster based time series (\code{RasterBrick} or \code{RasterStack} object, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the classification of the NEMA). This function
#' considers only the additional criteria related to the relative air humidity.
#'
#' @param airhumidity A \code{RasterBrick} or \code{RasterStack} object with
#' mean relative air humidity values on a daily basis within the target time
#' period.
#' @param airhumidity_t A \code{POSIXct} vector containing the time information
#' for all layers in \code{airhumidity} as returned by \code{\link{dailyMeans}}
#' (each element denoting the respective day).
#' @return A list with two elements:
#' \describe{
#'   \item{\code{droughtairhumidity}}{A \code{RasterBrick} object containing for each
#'   fixed ten-day interval in \code{airhumidity_t} a layer classifying if there were
#'   drought conditions according to the relative air humidity or not.}
#'   \item{\code{time}}{A character vector specifying the date of the first day of the
#'   respective fixed ten-day interval.}
#' }
#' @seealso
#' @examples #
#' @export
evaluateDroughtNemaAirhumidity <- function(airhumidity, airhumidity_t){

  # check if airhumidity and airhumidity_t are of the same length
  if(nlayers(airhumidity) != length(airhumidity_t)){
    stop("The layers of airhumidity have to correspond to the ten-day intervals denoted by airhumidity_t\n")
  }

  # assign each day of airhumidity_t to a fixed ten-day interval
  tdi <- assignfixedtendayinterval(airhumidity_t, timerange = 1:length(airhumidity_t))
  tdi <- lapply(tdi, function(x) which(as.character(airhumidity_t) %in% x))
  tdi <- do.call("c", lapply(seq_along(tdi[[2]]), function(x){
    currenttdi <- tdi[[1]][x]:tdi[[2]][x]
    names(currenttdi) <- rep(x, length(currenttdi))
    return(currenttdi)
  }))

  # compute for each study period tdi where near drought and drought conditions existed according to the precipitation
  brickdroughtairhumidity <- foreach(studytdi = unique(names(tdi)), .combine = brick, .multicombine = TRUE, .export = c("airhumidity", "tdi")) %do% {

    # create a raster with the same spatial properties as airhumidity and fill all values with NA
    rasterdroughtairhumidity <- airhumidity[[1]]
    values(rasterdroughtairhumidity) <- NA

    # assign the respective days to the current tdi
    index <- tdi[names(tdi) == studytdi]

    # count the number of days with a relative air humidity < 30 %
    nodaysairhumiditybelowthreshold <- calc(airhumidity[[index]], function(x){
      length(which(x < 30))
    })

    # evaluate the thresholds for the current tdi
    ndroughtairhumidity_threshold_index <- which(values(nodaysairhumiditybelowthreshold) > 5)

    # write the results to rasterdroughtairhumidity
    rasterdroughtairhumidity[ndroughtairhumidity_threshold_index] <- 1

    # return rasterdroughtairhumidity
    return(rasterdroughtairhumidity)

  }

  # return the raster along with the time information
  list(droughtairhumidity = brickdroughtairhumidity, time = tapply(tdi, names(tdi), function(x) airhumidity_t[x[1]]))

}

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
