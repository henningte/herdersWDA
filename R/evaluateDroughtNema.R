#'@importFrom Rdpack reprompt
#'@import doParallel
#'@import raster
NULL

#' Evaluates Drought Conditions Based on Raster Time Series.
#'
#' \code{evaluateDroughtNema} evaluates based on a set of weather variables as
#' raster based time series (\code{RasterBrick} or \code{RasterStack} object, if
#' for a given spatial and temporal point drought or near drought conditions are
#' prevalent or not (according to the classification of the NEMA).
#'
#' @param precipitation A \code{RasterBrick} or \code{RasterStack} object with
#' (mean) total precipitation values for each time interval within the target time
#' period. See the details section.
#' @param ltmprecipitation A \code{RasterBrick} or \code{RasterStack} object with
#' long-term mean total precipitation values for all time intervals denoted by
#' \code{resolution} within a year. See the details section.
#' @param airtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' mean air temperature values for each time interval within the target time
#' period. See the details section.
#' @param ltmairtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' long-term mean air temperature values for all time intervals denoted by
#' \code{resolution} within a year. See the details section.
#' @param ltsdairtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' standard deviation values of mean air temperature values within the time interval
#' considered as long-term time interval. See the details section.
#' @param dailymaxairtemperature A \code{RasterBrick} or \code{RasterStack} object with
#' daily maximum air temperature values within the target time period. See the
#' details section.
#' @param dailymeanrh A \code{RasterBrick} or \code{RasterStack} object with
#' daily mean relative air humidity values within the target time period. See the
#' details section.
#' @param naturalzonesraster A \code{RasterLayer} object containing information
#' on landcover classes for which additional criteria concerning drought conditions are
#' defined. Classes that should not be considered must have the value "0". Mountainous
#' regions must have the value "1". Forest steppe and steppe must have the value "2".
#' The gobi-desert must have the value "3".
#' @param resolution A character value indicating the temporal resolution of the
#' \code{Raster*} time series data for the target time period (\code{precipitation} and
#' \code{airtemperature}). One of \code{"monthly"}, \code{"ftdi"} (fixed ten-day
#' intervals) or \code{"mwtdi"} (moving window ten-day intervals).
#' @param timedate_aggregated A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{precipitation} and
#' \code{airtemperature}.
#' @param timedate_daily A \code{POSIXct} vector containing the time
#' information for each band/layer of \code{dailymaxairtemperature} and
#' \code{dailymeanrh}.
#' @param cores The number of cores to be used in parallel computing.
#' @param clcall A function passed to \code{\link[parallel]{clusterCall}}.
#' @return A list with the first element representing daily values if drought (2), near
#' drought (1) or no drought (0) conditions prevail, as a \code{RasterBrick} object, and
#' the second element being equal to \code{timedate_daily} and indicating the time information
#' relating to the layers of the first element.
#' @details The start and end time point of the target time interval are derived from
#' \code{timedate_daily}. All time intervals of \code{timedate_aggregated} are supposed
#' to fit perfectly within \code{timedate_daily} (i.e. there are no more or less days in
#' \code{timedate_daily}). All \code{Raster*} time series data are supposed to have the
#' same spatial extent an resolution. \code{precipitation} and \code{airtemperature} are
#' supposed to have the same temporal resolution. \code{dailymaxairtemperature} and
#' \code{dailymeanrh} are supposed to have the same temporal resolution.
#' @seealso
#' @examples #
#' @export
evaluateDroughtNema <- function(precipitation, ltmprecipitation, airtemperature,
                                ltmairtemperature, ltsdairtemperature, dailymaxairtemperature,
                                dailymeanrh, naturalzonesraster, resolution = "monthly",
                                timedate_aggregated, timedate_daily, cores = 10, clcall = NULL
                                ){

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

  # group values of timedate_daily according to timedate_aggregated
  switch(resolution,
         monthly = {
           months <- strftime(timedate_daily, "%Y-%m")
           indices1 <- sapply(seq_along(unique(months)), function(x){
             rep(x, length(which(months == unique(months)[x])))
           })
           indices2 <- as.integer(as.factor(months))
           while(length(which(indices2 > 12)) > 0){
             indices2[which(indices2 > 12)] <- indices2[which(indices2 > 12)] - 12
           }
           indices3 <- as.integer(as.factor(months))
         },
         fixedtendays = {
           timerange <- 1:length(timedate_daily)
           attr(timedate_daily, "tzone") <- "UTC"
           indices <- assignfixedtendayinterval(timedate = timedate_daily, timerange)
           indices <- lapply(indices, function(x) which(as.character(timedate[timerange]) %in% x))
           indices3 <- do.call(c, sapply(seq_along(indices[[1]]), function(x){
             rep(x, length(indices[[1]][x]: indices[[2]][x]))
           }))
           indices2 <- indices3
           while(length(which(indices2 > 36)) > 0){
             indices2[which(indices2 > 36)] <- indices2[which(indices2 > 36)] - 36
           }
         },
         mwtendays = {
           indices3 <- seq_along(timedate_daily)[1:(length(timedate_daily) - 9)]
           indices2 <- indices3
           while(length(which(indices2 > 366)) > 0){
             indices2[which(indices2 > 366)] <- indices2[which(indices2 > 366)] - 366
           }
         })

  # create a dummy raster for the classification of near drought conditions
  rasterdrought <- precipitation[[1]]
  values(rasterdrought) <- 0

  # set up cluster
  cl <- makeCluster(cores, outfile="", type = "PSOCK")
  registerDoParallel(cl)
  if(!is.null(clcall)){
    clusterCall(cl, clcall)
  }

  droughtclassified <-
    foreach(day_i = seq_along(timedate_daily), .multicombine = TRUE, .combine = stack, .packages = "raster") %dopar%{

      # get a raster to store the results in
      rasterdroughtdayi <- rasterdrought

      # value plus two times the standard deviation
      airtemperaturethreshold2 <- overlay(x = airtemperature[[indices3[day_i]]],
                                          y = ltsdairtemperature[[indices2[day_i]]],
                                          fun = function(x, y) x + y^2)

      # classify weather data as near drought conditions

      # define the variables
      x = precipitation[[indices3[day_i]]]
      y = airtemperature[[indices3[day_i]]]
      x1 = ltmprecipitation[[indices2[day_i]]]
      y1 = ltmairtemperature[[indices2[day_i]]]
      y2 = airtemperaturethreshold2
      neardrought = rasterdroughtdayi

      # classificaton based on the precipitation
      precipitationthreshold <- calc(x1, fun = function(z) z - 0.2*z)
      precipitationthresholdindices <- which(values(x) <= values(precipitationthreshold))

      # classification based on the air temperature
      airtemperaturethreshold1 <- calc(y1, fun = function(z) z + 1)
      airtemperaturethresholdindices <- which(values(y) > values(airtemperaturethreshold1) | values(y) > values(y2))

      # combine indices (temperature and precipitation related criteria have both to be met)
      combinedindices <- c(precipitationthresholdindices[which(precipitationthresholdindices %in% airtemperaturethresholdindices)])

      # set classificator values
      neardrought[combinedindices] <- 1

      x = dailymaxairtemperature[[day_i]]
      y = naturalzonesraster
      naturalzonesthreshold = rasterdroughtdayi
      naturalzonesthreshold[which((values(x) > (273.15 + 20) & values(naturalzonesraster) == 1)|
                                    (values(x) > (273.15 + 30) & values(naturalzonesraster) == 2)|
                                    (values(x) > (273.15 + 32) & values(naturalzonesraster) == 3))] <- 1


      # value plus two times the standard deviation
      airtemperaturethreshold2 <- overlay(x = airtemperature[[indices3[day_i]]],
                                          y = ltsdairtemperature[[indices2[day_i]]],
                                          fun = function(x, y) x + y^2*2)

      # classify weather data as drought conditions (main and additional)
      x = precipitation[[indices3[day_i]]]
      y = airtemperature[[indices3[day_i]]]
      x1 = ltmprecipitation[[indices2[day_i]]]
      y1 = ltmairtemperature[[indices2[day_i]]]
      y2 = airtemperaturethreshold2
      rh = dailymeanrh[[day_i]]
      nz = naturalzonesthreshold
      drought = rasterdroughtdayi

      # classificaton based on the precipitation
      precipitationthreshold <- calc(x1, fun = function(z) z - 0.5*z)
      precipitationthresholdindices <- which(values(x) <= values(precipitationthreshold))

      # classification based on the air temperature
      airtemperaturethreshold1 <- calc(y1, fun = function(z) z + 2)
      airtemperaturethresholdindices <- which(values(y) > values(airtemperaturethreshold1) | values(y) > values(y2))

      # additionaly criterium: rh
      rhthresholdindices <- which(values(rh) < 30)

      # additional criterium: nz
      naturalzonesthresholdindices <- which(values(naturalzonesthreshold) == 1)

      # combine indices and consider additional criteria
      combinedindices <- c(precipitationthresholdindices[which(precipitationthresholdindices %in% airtemperaturethresholdindices)])
      combinedindices <- unique(c(combinedindices, rhthresholdindices, naturalzonesthresholdindices))

      # set classificator values
      drought[combinedindices] <- 1

      # merge the classification results
      droughtclassification <- rasterdroughtdayi
      droughtclassification[which(values(neardrought) == 1)] <- 2
      droughtclassification[which(values(drought) == 1)] <- 3

      return(droughtclassification)
    }

  # convert to RasterBrick object
  droughtclassified <- brick(droughtclassified)

  # stop cluster
  stopCluster(cl)

  # return the result
  return(list(droughtclassified = droughtclassified, days = timedate_daily))

}
