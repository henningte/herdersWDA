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

  # group values of timedate_daily according to timedate_aggregated
  switch(resolution,
         monthly = {
            months <- strftime(timedate_daily, "%Y-%m")
            indices1 <- sapply(seq_along(unique(months)), function(x){
              rep(x, length(which(months == unique(months)[x])))
            })
         },
         fixedtendays = {
            timerange <- 1:length(timedate_daily)
            indices <- assignfixedtendayinterval(timedate_daily, timerange)
            # has to be finished

         },
         mwtendays = {
            indices1 <- seq_along(days)[1:(length(days) - 9)]
         })

  # create a dummy raster for the classification of near drought conditions
  rasterdrought <- precipitation[[1]]
  values(rasterdrought) <- 0

  droughtclassified <-
    foreach(day_i = seq_along(timedate_daily), .multicombine = TRUE) %dopar%{

      # get a raster to store the results in
      rasterdroughtdayi <- rasterneardrought

      airtemperaturethreshold2 <- over(x = airtemperature[[indices1[day_i]]],
                                       y = ltsdairtemperature[[indices1[day_i]]],
                                       fun = function(x, y) x + y^2)

      # classify weather data as near drought conditions
      neardrought <- overlay(x = precipitation[[indices1[day_i]]],
                             y = airtemperature[[indices1[day_i]]],
                             x1 = ltmprecipitation[[indices1[day_i]]],
                             y1 = ltmairtemperature[[indices1[day_i]]],
                             y2 = airtemperaturethreshold2,
                             nd = rasterdroughtdayi,
                             fun = function(x, y, x1, y1, y2, nd){

                               # classificaton based on the precipitation
                               precipitationthreshold <- calc(y1, fun = function(z) z - 0.2*z)
                               precipitationthresholdindices <- which(y <= precipitationthreshold)

                               # classification based on the air temperature
                               airtemperaturethreshold1 <- calc(y1, fun = function(z) z + 1)
                               airtemperaturethresholdindices <- which(y > airtemperaturethreshold1 | y > y2)

                               # combine indices
                               combinedindices <- c(precipitationthresholdindices, airtemperaturethresholdindices)
                               combinedindices <- combinedindices[duplicated(combinedindices)]

                               # set classificator values
                               nd[combinedindices] <- 1

                               # return nd
                               return(nd)

                             })

      airtemperaturethreshold2 <- over(x = airtemperature[[indices1[day_i]]],
                                       y = ltsdairtemperature[[indices1[day_i]]],
                                       fun = function(x, y) x + y^2*2)

      naturalzonesthreshold <- over(x = dailymaxairtemperature[[day_i]],
                                    y = naturalzonesraster,
                                    z = rasterdroughtdayi,
                                    fun = function(x, y ,z){

                                    z[which((x > (273.15 + 20) & naturalzonesraster == 1)|
                                           (x > (273.15 + 30) & naturalzonesraster == 2)|
                                           (x > (273.15 + 32) & naturalzonesraster == 3))] <- 1

                                    return(z)

                                   })

      # classify weather data as drought conditions (main and additional)
      drought <- overlay(x = precipitation[[indices1[day_i]]],
                         y = airtemperature[[indices1[day_i]]],
                         x1 = ltmprecipitation[[indices1[day_i]]],
                         y1 = ltmairtemperature[[indices1[day_i]]],
                         y2 = airtemperaturethreshold2,
                         rh = dailymeanrh[day_i],
                         nz = naturalzonesthreshold,
                         nd = rasterdroughtdayi,
                         fun = function(x, y, x1, y1, y2, rh, nz, nd){

                           # classificaton based on the precipitation
                           precipitationthreshold <- calc(y1, fun = function(z) z - 0.5*z)
                           precipitationthresholdindices <- which(y <= precipitationthreshold)

                           # classification based on the air temperature
                           airtemperaturethreshold1 <- calc(y1, fun = function(z) z + 2)
                           airtemperaturethresholdindices <- which(y > airtemperaturethreshold1 | y > y2)

                           # additionaly criterium: rh
                           rhthresholdindices <- which(rh < 30)

                           # additional criterium: nz
                           naturalzonesthresholdindices <- which(naturalzonesthreshold == 1)

                           # combine indices and consider additional criteria
                           combinedindices <- c(precipitationthresholdindices, airtemperaturethresholdindices)
                           combinedindices <- combinedindices[duplicated(combinedindices)]
                           combinedindices <- unique(c(combinedindices, rhthresholdindices, naturalzonesthresholdindices))

                           # set classificator values
                           nd[combinedindices] <- 1

                           # return nd
                           return(nd)

                         })

      # merge the classification results
      overlay(x = neardrought,
              y = drought,
              nd = rasterdroughtdayi,
              fun = function(x, y, nd){

                nd[which(x == 1)] <- 2
                nd[which(y == 1)] <- 3
                return(nd)

              })

  }

}
